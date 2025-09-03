# main
library(tidyverse)
library(tidytext)
library(stringi)

# calculations
library(philentropy)
library(caret)
library(randomForest)

# plots
library(ggtree)
library(paletteer)

# load necessary scripts
source("00_scripts.R")


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

################
### FORMULAS ###
################


df_cs <- read_tsv("data/prepared/cs_lines.tsv") %>% 
  mutate(syl= str_remove_all(pos_syl, "[A-Z]"))

df_de <- read_tsv("data/prepared/de_lines.tsv") %>% 
  mutate(syl= str_remove_all(pos_syl, "[A-Z]"))

df_ru <-  read_tsv("data/prepared/ru_lines.tsv") %>% 
  mutate(syl= str_remove_all(pos_syl, "[A-Z]"))

df <- bind_rows(df_cs %>% mutate(lang="cs"),
                df_ru %>% mutate(lang="ru"),
                df_de %>% mutate(lang="de"))

df %>%
  #filter(author != "Pushkin A.S.") %>%
  count(label,sort=T)

counts <- df %>%
  filter(lang=="ru") %>% 
  filter(label %in% c("trochee_4", "iamb_4", "trochee_5", "iamb_5")) %>% 
  #filter(str_detect(pattern, "1$")) %>%
  count(label,pos_syl,lang, sort=T) %>% 
  group_by(label,lang) %>%
  mutate(p=n/sum(n)*100) %>%
#  top_n(10) %>% 
  arrange(lang, label, -n)


## recalculate nsyl
counts_syl <- counts %>% mutate(nsyl=str_extract_all(pos_syl, "[0-9]")) %>% 
  rowwise() %>% 
  mutate(nsyl=unlist(nsyl) %>% as.numeric() %>% sum())

i4 <- counts_syl %>% filter(label == "iamb_4",nsyl == 8)
t4 <- counts_syl %>% filter(label == "trochee_4",nsyl == 8) 

intersection <- intersect(i4$pos_syl,t4$pos_syl)
union <- union(i4$pos_syl,t4$pos_syl)
# IOU
length(intersection)/length(union)*100
# 7.5% of all patterns
# what amount of lines in general? 
int_counts <- counts_syl %>% filter(pos_syl %in% intersection,
                                    label=="iamb_4" | label=="trochee_4")

int_t4 <- counts_syl %>% filter(pos_syl %in% intersection,label=="trochee_4")
int_i4 <- counts_syl %>% filter(pos_syl %in% intersection,label=="iamb_4")

n_int <- sum(int_counts$n)
n_total <- sum(i4$n) + sum(t4$n)

i_prop <- sum(int_i4$n) / sum(i4$n)
t_prop <- sum(int_t4$n) / sum(t4$n)
# only 40% of lines of the same length (8 syllables) come from templates shared across I4 and T4
n_int/n_total 


df2 <- df %>% filter(label %in% c("trochee_4", "iamb_4")) %>% 
  #filter(str_detect(pattern, "1$")) %>% 
  mutate(#per = floor(born/20)*20,
         n_syl = nchar(str_remove_all(pattern, "\\s")),
         syl_gr = paste(label, n_syl,sep=" ")) %>% 
  filter(n_syl %in% c(7,8,9), !syl_gr %in% c("trochee_5 11", "iamb_4 7"))

counts_year <- df %>%
  filter(label %in% c("trochee_4", "iamb_4", "trochee_5", "iamb_5")) %>%
  filter(str_detect(pattern, "1$")) %>%
 # mutate(per = floor(born/20)*20) %>% 
  count(label,pattern, pos, per, sort=T) %>% group_by(label) %>% mutate(p=n/sum(n))

df2 %>% count(label,pattern, pos, sort=T) %>% group_by(label) %>% mutate(p=n/sum(n))

### transitional probabilities

m_tp <- "trochee_4"

df_tp <- df_ru %>% 
  filter(label==m_tp) %>%
  mutate(pos_syl = paste0("SOL ",pos_syl, " EOL")) %>% 
  select(poem_id,line_id,pos_syl) %>% 
  unnest_tokens(output = "ng", input = "pos_syl", token = "ngrams",n=2,drop = F,to_lower = F)

df_tp_n3 <- df_ru %>% 
  filter(label==m_tp) %>%
  mutate(pos_syl = paste0("SOL ",pos_syl, " EOL")) %>% 
  select(poem_id,line_id,pos_syl) %>% 
#  filter(poem_id %in% c(1:500)) %>% 
  unnest_tokens(output = "ng", input = "pos_syl", token = "ngrams",n=3,drop = F,to_lower = F)

df_tp_n4 <- df_ru %>% 
  filter(label==m_tp) %>%
  mutate(pos_syl = paste0("SOL ",pos_syl, " EOL")) %>% 
  select(poem_id,line_id,pos_syl) %>% 
 # filter(poem_id %in% c(1:500)) %>% 
  unnest_tokens(output = "ng", input = "pos_syl", token = "ngrams",n=4,drop = F,to_lower = F)

df_tp_n5 <- df_ru %>% 
  filter(label==m_tp) %>%
  mutate(pos_syl = paste0("SOL ",pos_syl, " EOL")) %>% 
  select(poem_id,line_id,pos_syl) %>% 
  # filter(poem_id %in% c(1:500)) %>% 
  unnest_tokens(output = "ng", input = "pos_syl", token = "ngrams",n=5,drop = F,to_lower = F)


## transitional probabilities for n=2
tp <- df_tp %>%
  mutate(ng2 = ng) %>%
  separate(ng2,into = c("source", "target"),sep=" ") %>%
  count(source,target,sort = T) %>%
  mutate(p=n/sum(n)*100) %>%
  select(-n) %>% 
  pivot_wider(names_from = target,values_from = p,values_fill = 0)

## transitional probabilities for n=3
tp3 <- df_tp_n3 %>%
  mutate(ng2 = ng) %>%
  separate(ng2,into = c("source", "source2", "target"),sep=" ") %>%
  unite("source",c(source, source2),sep = " ") %>%
  count(source,target,sort = T) %>%
  mutate(p=n/sum(n)*100) %>%
  select(-n) %>% 
  pivot_wider(names_from = target,values_from = p,values_fill = 0)

## transitional probabilities for n=4
tp4 <- df_tp_n4 %>%
  mutate(ng2 = ng) %>%
  separate(ng2,into = c("source", "source2","source3", "target"),sep=" ") %>%
  unite("source",c(source, source2, source3),sep = " ") %>%
  count(source,target,sort = T) %>%
  mutate(p=n/sum(n)*100) %>% select(-n) #%>% pivot_wider(names_from = target,values_from = p,values_fill = 0)

v<-tp4$target %>% table()
tp4 <- tp4 %>% filter(target %in% names(v)) %>% pivot_wider(names_from = target,values_from = p,values_fill = 0) 

tp5 <- df_tp_n5 %>%
  mutate(ng2 = ng) %>%
  separate(ng2,into = c("source", "source2","source3", "source4", "target"),sep=" ") %>%
  unite("source",c(source, source2, source3,source4),sep = " ") %>%
  count(source,target,sort = T) %>%
  filter(!str_detect(source, "^NA")) %>% 
  mutate(p=n/sum(n)*100) %>% select(-n) %>% pivot_wider(names_from = target,values_from = p,values_fill = 0)


cols <- colnames(tp)[-1]
rows <- tp$source

cols3 <- colnames(tp3)[-1]
rows3 <- tp3$source

cols4 <- colnames(tp4)[-1]
rows4 <- tp4$source


cols5 <- colnames(tp5)[-1]
rows5 <- tp5$source

tp_matrix <- as.matrix(tp[,-1])
rownames(tp_matrix) <- rows

tp3_matrix <- as.matrix(tp3[,-1])
rownames(tp3_matrix) <- rows3

tp4_matrix <- as.matrix(tp4[,-1])
rownames(tp4_matrix) <- rows4

tp5_matrix <- as.matrix(tp5[,-1])
rownames(tp5_matrix) <- rows5

# trochee
# [1] "SOL"   "CONJ1" "S3"    "CONJ1" "S3"    "EOL" 
# iamb
# [1] "SOL"   "CONJ1" "S2"    "A3"    "S2"    "EOL"  
# first POS+SYL in Iamb4
#  [1] "CONJ1"   "SPRO1"   "V2"      "PR1"     "S2"      "V3"      "PRNA" "S3"      "PART1"   "ADVPRO1"
#  [1] "CONJ1" "SPRO1" "S2"    "PR1"   "PRNA"  "PART1" "S1"    "V2"    "V3" "V4"  

sort(tp_matrix["SOL",],decreasing = T)[1:20]
sort(tp3_matrix["SOL CONJ1",],decreasing = T)[1:5]
sort(tp4_matrix["SOL CONJ1 S2",],decreasing = T)[1:5]
sort(tp4_matrix["CONJ1 S2 A3",],decreasing = T)[1:5]
sort(tp5_matrix["CONJ1 S3 CONJ1 S3",],decreasing = T)[1:3]

sort(tp_matrix["SOL",],decreasing = T)[1:10]
sort(tp3_matrix["SOL CONJ1",],decreasing = T)[1:3]
sort(tp3_matrix["CONJ1 PR1",],decreasing = T)[1:3]
sort(tp3_matrix["PR1 S2",],decreasing = T)[1:3]
set.seed(1989)
slist <- vector(length = 20000)
for(i in 1:20000) {
 line1 <- max_ll(tp_matrix,tp3_matrix,tp4_matrix,maxll = F) %>% paste(collapse = " ")
 slist[i] <- line1
}

lines_trochee <- tibble(line=slist) %>% count(line,sort=T)
lines_iamb <- tibble(line=slist) %>% count(line,sort=T)

ll <- df_ru %>% filter(label=="trochee_4", str_detect(pos_syl, "^A4"))
max_ll <- function(tp_matrix,tp3_matrix,tp4_matrix,maxll=F) {
  x_line <- c("SOL")
  start <- "SOL"
  
  if(length(x_line) == 1) {
#  while(!"EOL" %in% x_line) {
    
    if(!maxll) {
    next_tok <- names(sample(tp_matrix[start,],size = 1,prob=tp_matrix[start,]))
    } else {
      next_tok <- names(which.max(tp_matrix[start,]))
    }
      
    start <- paste(x_line, next_tok)
    
    x_line = c(x_line,next_tok)
    
    
  #}
  }
  
  #while(!"EOL" %in% x_line) {
    
    if(!maxll) {
    next_tok <- names(sample(tp3_matrix[start,],size = 1,prob=tp3_matrix[start,])) } else {
      next_tok <- names(which.max(tp3_matrix[start,]))
    }
    
    x_line = c(x_line,next_tok)
    
    start <- paste(x_line,collapse = " ")
    

    while(!"EOL" %in% x_line) {
      
        if(!maxll) {
        next_tok <- names(sample(tp4_matrix[start,],size = 1,prob=tp4_matrix[start,])) } else {
          next_tok <- names(which.max(tp4_matrix[start,]))
        }
        
        x_line = c(x_line,next_tok)
        l <- length(x_line)
        start <- paste(x_line[c(l-2,l-1,l)],collapse = " ")
      
    
  }
  return(x_line)
  

}

df_tp %>%
  unnest_tokens(input="ng",output="word",drop=F,to_lower = F) %>% 
  paste()

sample_entropy = function(size=1000) {

df2 %>% group_by(syl_gr) %>%
  sample_n(size) %>%
  count(pos_syl,syl_gr) %>% 
  mutate(p=n/sum(n)) %>%
  group_by(syl_gr) %>%
  summarize(e=-sum(p*log(p)))
}

l <- vector(mode="list",100)

for(i in 1:100) {
  l[[i]] <- sample_entropy(size=5000)
}

l %>% bind_rows() %>% mutate(label=str_remove(syl_gr, " [0-9]{1,2}$")) %>%
                               ggplot(aes(syl_gr,e,color=label)) + geom_boxplot(alpha=0.5)

lb <- tibble(foot=c("4","5"),
             foot2=c("tetrameter", "pentameter"))

counts %>% ungroup() %>% 
  separate(label, into=c("meter","foot"),sep="_") %>% 
  arrange(meter,foot,-n) %>% group_by(meter,foot) %>% mutate(rank=row_number()) %>%
  filter(rank <= 100) %>% 
  left_join(lb) %>% 
  ggplot(aes(rank, p,color=meter)) + 
  geom_line() + 
  facet_wrap(~foot2,scales = "free_y") + 
  theme_linedraw() + labs(y="relative frequency", title="Rhythm/Syntax formulas: Czech")


counts %>% ungroup() %>% 
  separate(label, into=c("meter","foot"),sep="_") %>% 
  arrange(meter,foot,-n)
  

ggsave("formulas_de.png")

examples_df <- NULL
for(i in 1:nrow(counts)) {
  e1 <- df %>% filter(label==counts$label[i] &
                        pattern==counts$pattern[i] &
                        pos==counts$pos[i]) %>% group_by(label) %>% sample_n(5)
  examples_df <- rbind(examples_df,e1)
}

edf <- examples_df %>% select(poem_id,label,author,token,pattern,pos)
edf %>% filter(pattern=="0 1000 10 100",pos=="P V N N")
edf$token %>% sample()
edf
write_tsv(edf, paste0("ru_rhythm_formulas.tsv"))

############################
### PROSODY AND PATTERNS ###
############################

df <- read_tsv("data/prepared/cs_lines.tsv") %>% 
  mutate(syl= str_remove_all(pos_syl, "[A-Z]"))


set.seed(1890)

### Czech iamb

df_unnested <- df %>% filter(label=="iamb_5") %>% group_by(label) %>% 
  ungroup() %>% 
  mutate(token=str_split(token,"\\s",),
         pattern=str_split(pattern,"\\s"),
         pos=str_split(pos,"\\s")) %>% 
  select(label,line_id, token, pattern,pos) %>% 
  unnest(c(token, pattern,pos)) 

df_unnested %>% 
  group_by(line_id) %>%
  mutate(position=cumsum(nchar(pattern))) %>%
  filter(token=="a") %>% 
  ungroup() %>% 
  count(position) %>%
  mutate(n=n/sum(n)) %>%
  ggplot(aes(position,n)) + 
  geom_col() + 
  theme_minimal() + 
  labs(x="Positions of Czech 'and' in a line of iamb-5")


### POS STRESS PATTERNS

df <- read_tsv("data/prepared/cs_lines.tsv") %>% 
  mutate(syl= str_remove_all(pos_syl, "[A-Z]"))


df_unnested <- df %>% 
  mutate(token=str_split(token,"\\s",),
         pattern=str_split(pattern,"\\s"),
         pos=str_split(pos,"\\s")) %>% 
  select(label,line_id, token, pattern,pos) %>% 
  unnest(c(token, pattern,pos)) 

unnested_2 <- df_unnested %>%
  mutate(nsyl=nchar(pattern)) %>%
  group_by(line_id) %>%
  mutate(token_id=row_number(),pattern=str_split(pattern, "")) %>%
  unnest(c(pattern)) %>%
  group_by(line_id, token_id) %>% 
  mutate(syl_id=row_number())

unnested_2 %>%
  ungroup() %>% 
  count(pattern, nsyl, pos,syl_id) %>%
  filter(nsyl > 1, nsyl < 10, pos != "##", pattern!="N", pattern!="A", pattern=="1", pos !="XY", pos != "$.") %>% 
  arrange(pos,nsyl,syl_id) %>%
  group_by(nsyl,pos) %>%
  mutate(n=n/sum(n)) %>% 
  rename(stressed=n) %>%  
  ggplot(aes(as.character(syl_id), pos, fill=stressed)) + 
  geom_tile() + 
  facet_wrap(~nsyl,scales = "free_x",nrow = 1) + 
  theme_bw() + 
  scale_fill_viridis_c() +
  labs(x="Syllable",y="Part of speech",title="Stress profiles of parts of speech, RU") 

ggsave("pos_stress_profile_ru.png",width = 8,height = 4)

### Give features

df <- read_tsv("data/prepared/ru_lines.tsv") %>% 
  mutate(syl= str_remove_all(pos_syl, "[A-Z]"))

feature="S2 A3"
f="pos_syl"
m="iamb_4"

df %>%
  filter(label==m) %>%
  filter(str_detect(!! sym(f), feature)) %>%
  select(label, author,token,!! sym(f)) %>%
  mutate(is_first=str_detect(!! sym(f),paste0("^",feature)),
         is_last=str_detect(!! sym(f),paste0(feature, "$"))) %>% 
  #count(is_first)
  #count(is_last)
  sample_n(size = 10) 
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




############################
### Distinctive features ###
############################


# df <- read_tsv(corp[c]) %>% 
#   mutate(syl= str_remove_all(pos_syl, "[A-Z]"))
# 
# s <- sample_lines(df,n_lines = 1000,n_samples = 5,label = "foot")
# 
# mff=200
# ng=2
# f="pos_syl"
# 
# d <- vectorizer(s,mff = mff,ngram = ng,ftr = f,scale=T)
# 
# ## RF
# colnames(d) <- paste0("f_",colnames(d) %>% str_replace_all(" ", "_"))
# rownames(d) <- rownames(d) %>% str_remove("_.$")
# 
# train <- d %>% as.data.frame() 
# 
# train$meter1 <- rownames(d)
# train$meter1 <- as.factor(train$meter)
# 
# rf <- randomForest(meter1~., data=train,ntree=500)
# cm <- confusionMatrix(rf$predicted, train$meter1)
# acc <- cm$overall[1]
# acc
# 
# plot(rf)
# hist(treesize(rf),
#      main = "No. of Nodes for the Trees",
#      col = "green")
# 
# varImpPlot(rf,
#            sort = T,
#            n.var = 20,
#            main = "Top 10 - Variable Importance")
# 
# 
# cl <- c("iamb","trochee", "amphibrach")
# for(c in seq_along(cl)) {
# 
# imp <- importance(rf)
# impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)][1:50]
# op <- par(mfrow=c(2, 3))
# p_max <- vector(length = length(impvar))
# p_min <- vector(length = length(impvar))
# r <- vector(length=length(impvar))
# for (i in seq_along(impvar)) {
#   
#   x<-partialPlot(rf, train, impvar[i], cl[c], xlab=impvar[i],
#               main=paste("Partial Dependence on", impvar[i]))
#   p_max[i] <- max(x$y)
#   p_min[i] <- min(x$y)
#   lr <- lm(x$x ~ x$y)
#   r[i] <- lr$coefficients[2]
#   
#   
#   
#   
#   
# }
# par(op)
# 
# tib <- tibble(feature=impvar,p_max=p_max,p_min=p_min) %>%
#  
#   mutate(rank=row_number(),
#          delta=p_max-p_min,
#          direction=r,
#          prediction=p_max+p_min) %>% 
# #  filter(rank < 16) %>%
# #  arrange(-delta) %>% 
#     mutate(prediction = ifelse(direction > 0.1, delta, delta),
#            prediction = ifelse(direction < -0.1, -delta, prediction),
#            uncertain = ifelse(direction > -0.1 & direction < 0.1, TRUE, FALSE),
#            sign=ifelse(direction > 0.1, "pos", NA),
#            sign=ifelse(direction < -0.1, "neg", sign)) #%>% 
#  filter(rank < 5)

#labs<-tibble(feature=sort(tib$feature),label=c("Monosyllabs", "Disyllabs", "3syl", "4syl", "5syl", "6syl","7syl","8syl","9syl"))

# tmp <- tibble(feature=sort(tib$feature)) %>% mutate(feature=str_remove(feature,"f_"),
#                                              id=row_number(),
#                                              feature=str_split(feature,"_")) %>% unnest(feature)
# # 
# # 
# tmp <- tibble(feature=sort(tib$feature)) %>% mutate(feature=str_remove(feature,"f_"),
#                                                     id=row_number(),
#                                                     feature=str_split(feature, "_")) %>% 
#   unnest(feature) %>% 
#   mutate(part=row_number()) %>% 
#   mutate(feature=str_split(feature, "(?=[0-9])")) %>% 
#   unnest(feature)

# 
# feats <- tmp$feature[!str_detect(tmp$feature, "[0-9]")] %>% unique() %>% sort()
# feats
# labs0<-tibble(feature=feats,label=c("Adjective","Numeral", "Adverb", "Conjuction", "Noun", "Pronoun", "Preposition", "Verb"))
# 

# labs <- tmp %>% 
#   left_join(labs0) %>%
#   mutate(label=ifelse(is.na(label), feature,label)) %>% group_by(part,id) %>% summarize(label=paste(label,collapse=""),feature=paste0(feature,collapse="")) %>% group_by(id) %>%  summarize(label=paste(label, collapse="_"),feature=paste(feature,collapse="_")) %>%  mutate(feature=paste0("f_", feature)) %>% ungroup() %>% select(-id)

# 
# p <- tib %>% 
# #  left_join(labs) %>% 
#   mutate(feature=str_replace(feature, "f_", "")) %>% 
#   ggplot(aes(prediction,reorder(feature,-rank),fill=sign)) + geom_col() + labs(title=paste0("DE. POS+SYL bigrams: ", cl[c]), x="Prediction probability", y="(<-------- Sorted by total distinctiveness)") + theme_minimal() + theme(plot.background = element_rect(fill="white")) + scale_fill_manual(values=plt[c(4,6)])
# 
# p
# ggsave(p, filename = paste0("plots/distinctive/de/", "DE_", f, ng, "_", cl[c], ".png"),
#        width = 6,height = 7)
# 
# }
# lr <- lm(x$x~x$y)
# p[[4]]
