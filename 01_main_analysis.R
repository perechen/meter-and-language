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

# scripts
source("00_scripts.R")

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


#####################################
### random forest classification ###
#####################################


### parallel setup

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

