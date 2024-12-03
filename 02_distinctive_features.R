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


############################
### Distinctive features ###
############################


df <- read_tsv(corp[c]) %>% 
  mutate(syl= str_remove_all(pos_syl, "[A-Z]"))

s <- sample_lines(df,n_lines = 1000,n_samples = 5,label = "foot")

mff=200
ng=2
f="pos_syl"

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
acc

plot(rf)
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")

varImpPlot(rf,
           sort = T,
           n.var = 20,
           main = "Top 10 - Variable Importance")


cl <- c("iamb","trochee", "amphibrach")
for(c in seq_along(cl)) {

imp <- importance(rf)
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)][1:50]
op <- par(mfrow=c(2, 3))
p_max <- vector(length = length(impvar))
p_min <- vector(length = length(impvar))
r <- vector(length=length(impvar))
for (i in seq_along(impvar)) {
  
  x<-partialPlot(rf, train, impvar[i], cl[c], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]))
  p_max[i] <- max(x$y)
  p_min[i] <- min(x$y)
  lr <- lm(x$x ~ x$y)
  r[i] <- lr$coefficients[2]
  
  
  
  
  
}
par(op)

tib <- tibble(feature=impvar,p_max=p_max,p_min=p_min) %>%
 
  mutate(rank=row_number(),
         delta=p_max-p_min,
         direction=r,
         prediction=p_max+p_min) %>% 
#  filter(rank < 16) %>%
#  arrange(-delta) %>% 
    mutate(prediction = ifelse(direction > 0.1, delta, delta),
           prediction = ifelse(direction < -0.1, -delta, prediction),
           uncertain = ifelse(direction > -0.1 & direction < 0.1, TRUE, FALSE),
           sign=ifelse(direction > 0.1, "pos", NA),
           sign=ifelse(direction < -0.1, "neg", sign)) #%>% 
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


p <- tib %>% 
#  left_join(labs) %>% 
  mutate(feature=str_replace(feature, "f_", "")) %>% 
  ggplot(aes(prediction,reorder(feature,-rank),fill=sign)) + geom_col() + labs(title=paste0("DE. POS+SYL bigrams: ", cl[c]), x="Prediction probability", y="(<-------- Sorted by total distinctiveness)") + theme_minimal() + theme(plot.background = element_rect(fill="white")) + scale_fill_manual(values=plt[c(4,6)])

p
ggsave(p, filename = paste0("plots/distinctive/de/", "DE_", f, ng, "_", cl[c], ".png"),
       width = 6,height = 7)

}
lr <- lm(x$x~x$y)
p[[4]]
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


df <- read_tsv("data/prepared/cs_lines.tsv") %>% 
  mutate(syl= str_remove_all(pos_syl, "[A-Z]"))

df %>%
  #filter(author != "Pushkin A.S.") %>%
  count(label,sort=T)

counts <- df %>% filter(label %in% c("trochee_4", "iamb_4", "trochee_5", "iamb_5")) %>% filter(str_detect(pattern, "1$")) %>%  count(label,pattern, pos, sort=T) %>% group_by(label) %>% mutate(p=n/sum(n)) %>%  top_n(100)


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
  theme_linedraw() + labs(y="relative frequency", title="Rhythm/Syntax formulas: German!!")

ggsave("formulas_de.png")

examples_df <- NULL
for(i in 1:nrow(counts)) {
  e1 <- df %>% filter(label==counts$label[i] & pattern==counts$pattern[i] & pos==counts$pos[i]) %>% group_by(label) %>% sample_n(10)
  examples_df <- rbind(examples_df,e1)
}

edf <- examples_df %>% select(poem_id,label,author,token,pattern,pos)
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

df_unnested %>% group_by(line_id) %>% mutate(position=cumsum(nchar(pattern))) %>% filter(token=="a") %>% ungroup() %>%  count(position) %>% mutate(n=n/sum(n)) %>% ggplot(aes(position,n)) + geom_col() + theme_minimal() + labs(x="Positions of Czech 'and' in a line of iamb-5")


### POS STRESS PATTERNS

df <- read_tsv("data/prepared/de_lines.tsv") %>% 
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

unnested_2 %>% ungroup() %>% count(pattern, nsyl, pos,syl_id) %>% filter(nsyl > 1, nsyl < 10, pos != "##", pattern!="N", pattern!="A", pattern=="1", pos !="XY", pos != "$.") %>% arrange(pos,nsyl,syl_id) %>% group_by(nsyl,pos) %>% mutate(n=n/sum(n)) %>% rename(stressed=n) %>%  ggplot(aes(as.character(syl_id), pos, fill=stressed)) + geom_tile() + facet_wrap(~nsyl,scales = "free_x",nrow = 1) + theme_bw() + scale_fill_viridis_c() + labs(x="Syllable",y="Part of speech",title="Stress profiles of parts of speech, DE") 

ggsave("pos_stress_de.png",width = 6,height = 6)
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

