#######################
### Simple features ###
#######################

library(paletteer)
library(patchwork)
library(tidyverse)

source("00_scripts.R")

## Read CS data
df <- read_tsv("data/prepared_foot/cs_lines.tsv") %>% 
  mutate(syl= str_remove_all(pos_syl, "[A-Z]"))


### sample & vectorize a little bit of data
set.seed(28)
s <- sample_lines(df,n_lines = 100,n_samples = 40,label = "foot")
d <- vectorizer(s,mff = NA,ngram = 1,ftr = "syl",scale=T)

### 4 meters by word length
p2 <- draw_scatter(d,xpos = 1,ypos = 2,plt=plt[c(3,5,4,6)],filter = F) + labs(x="Monosyllables",y="Disyllables",title = "b.")

### sample data for iamb vs. trochee
set.seed(301)
s <- sample_lines(df,n_lines = 100,n_samples = 60,label = "foot")
d <- vectorizer(s,mff = NA,ngram = 1,ftr = "token",scale=T)

### dotplot!
p1<-tibble(x=d[,1],
       label0=rownames(d)) %>% 
  mutate(label=str_remove(label0,"_.*")) %>% 
  filter(label %in% c("iamb", "trochee")) %>% 
  ggplot(aes(x, fill=label)) + geom_dotplot(binwidth=0.175,stackgroups = T, binpositions = "all") + 
  scale_fill_manual(values=plt[c(4,6)]) + theme_minimal() + #+ ylim(0,0.25)  + 
  theme(axis.title = element_text(size=10), plot.title = element_text(size=14), axis.text.y=element_blank(), plot.background = element_rect(fill="white",color=NA),legend.title=element_blank()) + 
  labs(x="Scaled frequency of 'a' ('and') ", y=NULL, title="a.")  + guides(fill="none")
p1

p1+p2
ggsave("plots/DH25/simple_feature.png",width = 9,height = 4)



##############################
### Classification Results ###
##############################


resdf<-read_csv("res_foot_rf.csv",col_names = c("lang","feature","sample_size","ngram","acc")) 


## classification by foot
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


## classification by meter
p2 <- resdf %>% mutate(ngram=as.character(ngram)) %>% 
  group_by(lang,feature,sample_size,ngram) %>%
  summarize(lo=quantile(acc,0.025),
            hi=quantile(acc,0.975),
            acc=mean(acc)) %>% 
  ggplot(aes(sample_size, acc)) + 
  geom_line(aes(linetype=ngram,color=feature),linewidth=0.8) + 
  #  geom_ribbon(aes(ymin=lo,ymax=hi,group=interaction(feature,ngram)),fill="grey", alpha=0.3) +
  facet_wrap(~lang) + 
  labs(x="Sample size (lines)",y="Accuracy",title = "b. Classifying by metrical form (8-17 classes)") + 
  scale_color_paletteer_d("basetheme::clean") + 
  theme_minimal() + 
  geom_hline(data=xlines,aes(yintercept=baseline),linetype=3) + 
  theme(strip.text = element_blank(),
        plot.background = element_rect(fill="white",color=NA,),plot.title = element_text(size=10),legend.position = "bottom") +
  scale_y_continuous(breaks = seq(0,1,by=0.1))

## combine (god bless patchwork)
p1/p2

ggsave("plots/DH25/classify_foot_rf.png",height=6,width=8)
