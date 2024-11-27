# main
library(tidyverse)
library(tidytext)
library(stringi)

# calculations
library(doParallel)
library(philentropy)
library(randomForest)
library(stylo)

# plots
library(ggtree)
library(paletteer)


########################
### data preparation ###
########################


min_lines = 5000 ## filter meters by size # 1000k x 5 samples

fs <- c("data/dump/cs/", "data/dump/de", "data/dump/ru/")
file_names <- c("data/prepared/cs_lines.tsv", "data/prepared/de_lines.tsv", "data/prepared/ru_lines.tsv")

## iterate over folders
for(i in 1:3) {
### file names
f<-list.files(fs[i],full.names = T)

### collect
cs <- lapply(f, read_csv, 
             col_names = c("poem_id", "line_id","token", "pattern", "pos", "author", "born","died"),
             col_types = list("i", "i", "c", "c", "c", "c","i", "i"))

### table with metrical labels
labels <- tibble(meter=as.character(1:length(cs)),
                 label=str_replace(f,".*/(.*?)\\.csv", "\\1"),
                 label_foot=str_remove(label, "_..?$"))

### combine tables
cs_df <- cs %>% bind_rows(.id = "meter") %>% left_join(labels)

## filter meters
selection <- cs_df %>%
  count(label,line_id) %>%
  count(label,sort=T) %>% 
  filter(n > min_lines)

## apply selection to the larger table
cs_df <- cs_df %>% filter(label %in% selection$label)

## cast lines: one line = one row
lines_df <- cs_df %>%
  mutate(pos_syl=paste0(pos, nchar(pattern))) %>%
  group_by(meter, label, label_foot, poem_id, line_id,author) %>%
  summarize(token=paste(token, collapse=" "),
            pattern=paste(pattern, collapse=" "),
            pos=paste(pos,collapse=" "),
            pos_syl=paste(pos_syl, collapse=" "))

write_tsv(lines_df,file = file_names[i])
}




################
### sampler ####
################

sample_lines <- function(x,n_lines=1000,n_samples=1,label=NA) {
  
  ## groups 
  if(!is.na(label)) {
    x <- x %>% mutate(label=str_remove(label, "_..?$"))
  }
  
  
  ### this to stratify further sampling by authors (i.e. take uniform distribution, but only for super rich) 
  ### those who wrote many lines in one meter
  major_poets <- x %>% count(author, label) %>% filter(n>5000) %>% mutate(gr_id=paste(author, label,sep="_"))
  
  ### two pools
  p1 <- x %>% mutate(gr_id=paste(author, label,sep="_")) %>% filter(gr_id %in% major_poets$gr_id) %>% select(-gr_id)
  p2 <- x %>% anti_join(p1 %>% select(line_id),by="line_id")
  
  ### if data has major poets
  if(nrow(major_poets) != 0) {
  p1 <- p1 %>% group_by(label, author) %>% sample_n(5000) %>% ungroup()
  }
  
  pool <- bind_rows(p1,p2)
  ### end of stratification
  
  
  ## quick way to make independent samples without replacement: take n_lines*n_samples worth of lines, then shuffle, and divide to n_sample buckets
  sample_out <- pool %>% 
    group_by(label) %>% 
    sample_n(n_lines*n_samples) %>% # take all that is required
    slice_sample(prop=1) %>% # shuffle within group
    mutate(sample_no=ceiling(row_number()/n_lines)) %>% # annotate buckets 
    ungroup() 
  
  return(sample_out)
}

##################
### vectorizer ###
##################

### ftr list
# "token"
# "pos"
# "syl"
# "pos_syl"

# custom n-gram tokenizer

generate_sliding_ngrams <- function(x, n) {
  words <- unlist(stri_extract_all_regex(x, "\\b[\\p{L}\\d]+\\b"))
  if (n > length(words)) {
    return(character(0))  # Return empty if n is larger than number of words
  }
  ngrams <- sapply(1:(length(words) - n + 1), function(i) {
    paste(words[i:(i + n - 1)], collapse = " ")
  })
  return(ngrams)
}

ngrammy <- function(x,n=2) {
#  nmatch <- "\\b\\w+\\b" # basic char match
  nmatch <- "\\b[\\p{L}\\d]+\\b" # alternative
  pattern <- paste0("(?=(", paste(rep(nmatch,n),collapse=" "), "))") # allow multiple n

  matches<-gregexpr(pattern, x, perl=TRUE)
  # a little post-processing needed to get the capture groups with regmatches
  attr(matches[[1]], 'match.length') <- as.vector(attr(matches[[1]], 'capture.length')[,1])
  r <- regmatches(x, matches)
  return(r)
}

vectorizer <- function(x,mff=50,ftr="token",ngram=1,scale=T) {
  
  ## unnest tokens
  if(ngram == 1) {
  
  long <- x %>%
    group_by(label, sample_no) %>% 
    unnest_tokens(input=!! sym(ftr),output=token,to_lower = F)
  
  ## if bigrams, cut to bigrams
  
  } else {
    
    ngr <- x %>%
      select(label, sample_no, !! sym(ftr)) %>% 
      rename(token = !! sym(ftr)) %>% 
      rowwise() %>%
      mutate(token = list(generate_sliding_ngrams(token, n=ngram)))
    
    long <- ngr %>%
      unnest(token) %>% 
      ungroup()
    
  }
  
  ## count total 
  mf <- long %>% ungroup() %>%  count(token,sort=T) %>% pull(token)
  
  ## wide table
  wide <- long %>% 
    ungroup() %>% 
    count(label, sample_no, token) %>%
    mutate(token=factor(token, levels=mf)) %>%
    pivot_wider(names_from = token,values_from = n,values_fill = 0,names_sort=T)
  
  dtm <- wide[,-c(1,2)] %>% as.matrix()
  rownames(dtm) <- paste(wide$label, wide$sample_no,sep="_")
  
  
  ## handle if we want to grab all features
  if(is.na(mff)) {
    mff <- ncol(dtm)
  }
  
  dtm_s <- dtm[,1:mff]/rowSums(dtm[,1:mff])
  dtm_s[which(is.na(dtm_s))] <- 0
   
  if(scale) {
    dtm_s <- scale(dtm_s)
  }
  
  return(dtm_s)
}

################
### drawings ###
################


plt <- c(
  "#003f5c",
  "#444e86",
  "#955196",
  "#dd5182",
  "#ff6e54",
  "#ffa600"
)


draw_tree_meters <- function(x,is_dtm=T,labels,groupings,hnode=NA, pal=2,col=1,skip=1,ord=c(1,2)) {
  dm <- as.matrix(x)
  if(is_dtm) {
    dm <- philentropy::JSD(as.matrix(x))
  } 
  

  
  
  ## tree object
  dm <- dm  %>% 
    `rownames<-`(labels) %>% # reset rownames
    as.dist() %>% # to dist object
    hclust(method="ward.D2") %>%
    ape::as.phylo()
  
  ## groups
  tree <- groupOTU(dm,groupings)
  
  plot=ggtree(tree,aes(color=group),layout="circular",size=1) 
  plot$data <- plot$data  %>%
    mutate(class = str_remove_all(label, "_.*?$"),
           label = str_remove_all(label, " .*?$"), # get class labels
           group=str_replace(group, "0", "A")) 
  #  left_join(trans, by="class")  %>% # translations
  root <- plot$data %>% filter(parent==node) %>% pull(node)
  print(root)
  
  col_seq <- c(5,2,1)
  ## to highlight the binary split
  if(is.na(hnode) %>% unique()) {
    high_nodes <- c(root+1,root+2)
    d <- data.frame(node=high_nodes, foot=c("ternary", "binary"))
    pal <- paletteer_d("beyonce::X2")[col_seq[ord]]
  } else {
    # custom node clust
    high_nodes <- hnode
    d <- data.frame(node=high_nodes, foot=LETTERS[1:length(hnode)])
    pal <- paletteer_d("beyonce::X2")[col]
  }
  
  
  p <- plot +
    geom_highlight(data=d,aes(node=node, fill=foot),type="auto",alpha=0.3) +
    geom_tiplab(aes(label=label),hjust=-.1,size=3) + 
    guides(color="none",fill="none") +
    scale_fill_manual(values=pal) +
    scale_color_manual(values=paletteer_d("beyonce::X2")[-skip]) 
  
  return(p)
  
  
} 
draw_scatter <- function(df, xpos, ypos, plt,filter=T) {
  
  tibble(x=df[,xpos],
         y=df[,ypos],
         label0=rownames(df)) %>% 
    mutate(label=str_remove(label0,"_.*")) %>% 
    {if (filter) filter(., label %in% c("iamb", "trochee")) else .} %>% 
    ggplot(aes(x,y,color=label)) + 
    geom_point(size=2,shape = 1,stroke = 1) + 
    theme_minimal() + 
    scale_color_manual(values=plt) + 
    theme(axis.title = element_text(size=10), legend.title=element_blank(),plot.title = element_text(size=14),
          plot.background = element_rect(fill="white",color=NA)) + labs(x=colnames(df)[xpos], y=colnames(df)[ypos])
  
}




######################
### random forests ###
######################


library(randomForest)
library(caret)

df <- read_tsv("data/prepared/ru_lines.tsv") %>% 
  mutate(syl= str_remove_all(pos_syl, "[A-Z]"))


set.seed(28)
s <- sample_lines(df,n_lines = 500,n_samples = 5,label = NA)

d <- vectorizer(s,mff = NA,ngram = 2,ftr = "syl",scale=T)
colnames(d) <- paste0("syl",colnames(d) %>% str_remove_all(" "))
rownames(d) <- rownames(d) %>% str_remove("_.$")

df <- d %>% as.data.frame() 

df$meter <- rownames(d)
df$meter <- as.factor(df$meter)

rf <- randomForest(meter~., data=df,ntree=500)

plot(rf)
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")

varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf)
partialPlot(rf, df, syl32, "dactyl_4")

cm <- confusionMatrix(rf$predicted,df$meter)

cm$overall[1]
imp <- importance(ozone.rf)
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op <- par(mfrow=c(2, 3))
for (i in seq_along(impvar)) {
  partialPlot(ozone.rf, airquality, impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]),
              ylim=c(30, 70))
}
par(op)

############
### main ###
############


df <- read_tsv("data/prepared_foot/cs_lines.tsv") %>% 
  mutate(syl= str_remove_all(pos_syl, "[A-Z]"))


set.seed(28)
s <- sample_lines(df,n_lines = 200,n_samples = 20,label = "foot")

d <- vectorizer(s,mff = NA,ngram = 1,ftr = "syl",scale=T)

draw_scatter(d,xpos = 2,ypos = 1,plt=plt[c(4,6)]) + labs(x="Monosyllables",y="Disyllables")
ggsave("plots/monosyllables/monosyl_ru.jpeg",height = 6,width = 8)

draw_scatter(d,xpos = 2,ypos = 1,plt=plt[c(3,5,4,6)],filter = F) + labs(x="Monosyllables",y="Disyllables")
ggsave("plots/monosyllables/monosyl_all.jpeg",height = 6,width = 8)


d2 <- vectorizer(s,mff = 100,ngram = 1,ftr = "token",scale=T)
draw_scatter(d2,xpos = 12,ypos = 11,plt=plt[c(4,6)]) + labs(x="so ('so')",y="den ('that')")#@+ ylim(-2.5,2.5)
ggsave("plots/monosyllables/functionw_de.jpeg",height = 6,width = 8)

d3 <- vectorizer(s,mff = NA,ngram = 1,ftr = "pos",scale=T)
draw_scatter(d3,xpos = 6,ypos = 1,plt=plt[c(4,6)]) + labs(x="Conjuctions",y="Nouns")
ggsave("plots/monosyllables/pos.jpeg",height = 6,width = 8)


gr <- list(A = rownames(d)[1:10],
                  B = rownames(d)[11:25],
                  C = rownames(d)[26:55],
                  D = rownames(d)[56:90])

draw_tree_meters(d,groupings = gr ,labels = rownames(d),skip=c(1,2))


################
### Parallel ###
################

# How many cores to use in the cluster?
ncores <- 4
# Set up a cluster
my_cluster <- makeCluster(ncores)
# Register the cluster
registerDoParallel(my_cluster)

# list of packages, for foreach package
packages_used <- c("tidyverse", "doParallel", "tidytext", "randomForest", "stringi","caret")

##################
### classifier ###
##################


corp <- list.files("data/prepared/",full.names = T)
langs <- c("cs","de","ru")
#langs <- c("de","ru")

n_lines = c(25,50,100, 200, 300, 500, 750, 1000)
ngram <- c(1,2)
features <- c("token", "pos", "syl", "pos_syl")



## corpus
for(c in 1:length(corp)) {
  
  df <- read_tsv(corp[c]) %>% 
    mutate(syl= str_remove_all(pos_syl, "[A-Z]"))
  
  foreach(i=1:100,
          .packages = packages_used) %dopar%  { 
  
  for(n in n_lines) {
    
    s <- sample_lines(df,n_lines = n,n_samples = 5,label = NA)
    
    for(f in features) {
      
      for(ng in ngram) {
        
        mff=NA
        
        if(f=="token") {
          mff=200
        }
        
        if(f=="token" & ng==2 & n==25) {
          mff=NA
        }
        
        # if(langs[c]=="ru" & f=="token" & ng== 2) {
        #   next
        # }
        
        if(f=="pos" & ng==2 & n >= 50) {
          mff=100
        }
        
        if(f=="pos_syl" & ng==2 & n >= 50) {
          mff=200
        }

        d <- vectorizer(s,mff = mff,ngram = ng,ftr = f,scale=T)
        
        ## RF
        colnames(d) <- paste0("f_",colnames(d) %>% str_replace_all(" ", "_"))
        rownames(d) <- rownames(d) %>% str_remove("_.$")
        
        train <- d %>% as.data.frame() 
        
        train$meter1 <- rownames(d)
        train$meter1 <- as.factor(train$meter)
        
        rf <- randomForest(meter1~., data=train,ntree=500)
        cm <- confusionMatrix(rf$predicted, train$meter1)
        acc <- cm$overall[1]
        
        ## stylo
        
        #set=d
        #rownames(set) <- str_remove(rownames(d),"_.$") %>% str_remove("_")
        
        # <- crossv(training.set = set, cv.mode = "leaveoneout", classification.method = "delta")
        #acc <- sum(results$y)/length(results$y)
        
        write_lines(file = "res_form_rf.csv", x=paste0(langs[c],",",f,",",n,",",ng, ",",acc),append = T)
        
    
      }
    }
  }
}
}



## rewrite the sampling! now it does 5 samples per metrical variation! so ~ 20,25 samples per meter.
## check German weird numbers
## fix cyrillic non-ASCII regex for n-grams


# df <- read_tsv(corp[3]) %>% 
#   mutate(syl= str_remove_all(pos_syl, "[A-Z]"))
# s <- sample_lines(df,n_lines = 100,n_samples = 5)
# d <- vectorizer(s,mff = NA,ngram = 2,ftr = "syl",scale=T)

resdf<-read_csv("res_foot_rf.csv",col_names = c("lang","feature","sample_size","ngram","acc")) 

library(paletteer)

xlines <- tibble(lang=c("cs", "de", "ru"),
                 baseline=c(0.25, 0.33, 0.20))

p1 <- resdf %>% mutate(ngram=as.character(ngram)) %>% 
  group_by(lang,feature,sample_size,ngram) %>%
  summarize(lo=quantile(acc,0.025),
            hi=quantile(acc,0.975),
            acc=mean(acc)) %>% 
  ggplot(aes(sample_size, acc)) + 
  geom_line(aes(linetype=ngram,color=feature),linewidth=0.8) + 
#  geom_ribbon(aes(ymin=lo,ymax=hi,group=interaction(feature,ngram)),fill="grey", alpha=0.3) +
  facet_wrap(~lang) + 
  labs(x=NULL,y="Accuracy",title = "a. Classifying by foot type (3-5 classes)") + 
  scale_color_paletteer_d("basetheme::clean") + 
  theme_minimal() + 
  geom_hline(data=xlines,aes(yintercept=baseline),linetype=3) + 
  theme(strip.text = element_text(size=14),
        plot.background = element_rect(fill="white",color=NA),
        plot.title = element_text(size=10)) +
  scale_y_continuous(breaks = seq(0,1,by=0.1)) + guides(linetype="none", color="none")

resdf<-read_csv("res_form_rf.csv",col_names = c("lang","feature","sample_size","ngram","acc")) 


xlines <- tibble(lang=c("cs", "de", "ru"),
                 baseline=c(0.0625, 0.125, 0.06))


p2 <- resdf %>% mutate(ngram=as.character(ngram)) %>% 
  group_by(lang,feature,sample_size,ngram) %>%
  summarize(lo=quantile(acc,0.025),
            hi=quantile(acc,0.975),
            acc=mean(acc)) %>% 
  ggplot(aes(sample_size, acc)) + 
  geom_line(aes(linetype=ngram,color=feature),linewidth=0.8) + 
  #  geom_ribbon(aes(ymin=lo,ymax=hi,group=interaction(feature,ngram)),fill="grey", alpha=0.3) +
  facet_wrap(~lang) + 
  labs(x="Sample size (lines)",y="Accuracy",title = "b. Classifying by metrical form (15-17 classes)") + 
  scale_color_paletteer_d("basetheme::clean") + 
  theme_minimal() + 
  geom_hline(data=xlines,aes(yintercept=baseline),linetype=3) + 
  theme(strip.text = element_blank(),
        plot.background = element_rect(fill="white",color=NA,),plot.title = element_text(size=10),legend.position = "bottom") +
  scale_y_continuous(breaks = seq(0,1,by=0.1))

p1/p2

ggsave("classify_foot_rf.png",height=6,width=8)

#########################
### by feature plots  ### 
#########################

plt1 <- palettes_d$basetheme$clean
fs=c("syl","pos_syl", "pos", "token")
ord =c(3,2,1,4)


for(f in 1:4) {

resdf %>% mutate(ngram=as.character(ngram)) %>% 
  group_by(lang,feature,sample_size,ngram) %>%
  summarize(lo=quantile(acc,0.1),
            hi=quantile(acc,0.9),
            acc=mean(acc)) %>% 
  ggplot(aes(sample_size, acc,group=interaction(feature,ngram))) + 
  geom_line(data=. %>% filter(feature!=fs[f]),aes(linetype=ngram),linewidth=0.5,color="grey90") + 
  geom_line(data=. %>% filter(feature==fs[f]),aes(linetype=ngram),color=plt1[ord[f]],linewidth=0.8) + 
  geom_ribbon(data=. %>% filter(feature==fs[f]), aes(ymin=lo,ymax=hi,group=interaction(feature,ngram)),fill="grey", alpha=0.2) +
  facet_wrap(~lang) + 
  labs(x="Sample size (lines)",y="Accuracy",title = "Recognizing metrical foot (3-5 classes)") + 
#  scale_color_paletteer_d("basetheme::clean") + 
  theme_minimal() + 
  geom_hline(data=xlines,aes(yintercept=baseline),linetype=3) + 
  theme(strip.text = element_text(size=14),
        plot.background = element_rect(fill="white")) +
  scale_y_continuous(breaks = seq(0,1,by=0.1)) 

ggsave(paste0("plots/by_feature/foot_", fs[f], ".png"), width=8, height = 4)


}

############
### UMAP ###
############

library(umap)

df <- read_tsv("data/prepared/ru_lines.tsv") %>% 
  mutate(syl= str_remove_all(pos_syl, "[A-Z]"))

df %>% count(label_foot,sort=T)


set.seed(1989)
s1 <- sample_lines(df %>% filter(label_foot=="iamb"),n_lines = 500,n_samples = 120,label = "foot")
s2 <- sample_lines(df %>% filter(label_foot=="trochee"),n_lines = 500,n_samples = 80,label = "foot")
s3 <- sample_lines(df %>% filter(!label_foot %in% c("iamb", "trochee")), n_lines=500,n_samples=40,label = "foot")
d <- vectorizer(bind_rows(s1,s2,s3),mff = 100,ngram = 1,ftr = "token",scale=T)


set.seed(1875)
proj <- umap(d,n_neighbors=35,preserve.seed = T)


## plot 1
tibble(label=str_remove(rownames(proj$layout), "_.{1,3}$"),x=proj$layout[,1],y=proj$layout[,2]) %>%
  ggplot(aes(x,y)) + 
  geom_point(color="grey40",shape=21,stroke=1) + 
  theme_void() + scale_color_paletteer_d("basetheme::clean") + theme(plot.background = element_rect(fill="white"))

ggsave("projection_01.png",width = 8,height = 5)

## plot 2 


proj_df <- tibble(label=str_remove(rownames(proj$layout), "_.{1,3}$"),x=proj$layout[,1],y=proj$layout[,2]) #%>%
centroid <- proj_df %>% summarise(x=mean(x),y=mean(y))

proj_df %>%   
  ggplot(aes(x,y)) + 
  geom_point(color="grey40",shape=21,stroke=1) + 
  geom_point(data=centroid, color="red", shape=4,size=20,fill="red",stroke=4) +
  theme_void() + scale_color_paletteer_d("basetheme::clean") + theme(plot.background = element_rect(fill="white"))

ggsave("projection_02.png",width = 8,height = 5)

## plot 3

centroids <- proj_df %>% group_by(label) %>% summarize(x=mean(x),y=mean(y))
proj_df %>%   
  ggplot(aes(x,y)) + 
  geom_point(aes(color=label),shape=21,,stroke=1) + 
  geom_point(data=centroid, color="grey60", shape=4,size=20,fill="grey60",stroke=4,alpha=0.5) +
#  geom_point(data=centroids, aes(color=label),stroke=3,size=10,shape=4,alpha=0.8) +
  theme_void() + 
#  scale_color_paletteer_d("basetheme::clean") + 
  scale_color_manual(values=plt[-1]) +
  theme(plot.background = element_rect(fill="white")) + guides(color="none")

ggsave("projection_03.png",width = 8,height = 5)

proj_df %>%   
  ggplot(aes(x,y)) + 
  geom_point(aes(color=label),shape=21,stroke=1) + 
  geom_point(data=centroid, color="grey60", shape=4,size=20,fill="grey60",stroke=4,alpha=0.5) +
  geom_point(data=centroids, aes(color=label),size=10,shape=4,alpha=0.8,stroke=3) +
  theme_void() + 
#  scale_color_paletteer_d("basetheme::clean") + 
  scale_color_manual(values=plt[-1]) +
  theme(plot.background = element_rect(fill="white")) + guides(color="none")
ggsave("projection_04.png",width = 8,height=5)


# N2 N2 - trochee
# A2 N2 - kind of trochee
# R1 N2 - iamb
# N2 V2 - kind of trochee
# J1 N2 - iamb
# N2 A2 - troch
# V2 N2 - troch
# R1 P1 - troch? 
# R1 N3 - troch?
# A3 N2 - some specific iamb ? It is in I2 ,whatever unholy thing that is
# P2 N2 - troch
# J1 P1 - troch?
# J1 V2 - iamb
# A2 N1 - kind of iamb, weird trochees (T2. Are these even make sense? Misrecognized? Part of 42/32 stanza?)
# N2 A3 kind of iamb. 

### ADVERB 1? 

### 1. Construct features from all available data
### Make them by-line, then pass to vectorizer 
# 1. MFW Words (we have them), 10, 100, 1000
# 2. Opening words frequency, MFW, 10, 100
# 3. N syllables
# 4. POS vanilla
# 5. POS n-grams (within line)
# 6. POS SYL
# 7. POS SYL n-grams
# 8. Masked formulas? "the N2 of N3"
### 2. Vectorizers for samples
### 3. Sampler
# 1. Take balanced sample of meters.
# 2. Of size N (10, 50, 100, 200, 300, 500, 1000)
# 3. No more than X lines per author? 
### 4. Classifier
# 0. Two modes: general meter vs. metrical form
# 1. Unsupervised sanity check
# 2. Some classifier (SVM? Trees? Need to check for features)

