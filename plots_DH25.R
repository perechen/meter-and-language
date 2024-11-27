#############
### Intro ###
#############

df <- read_tsv("data/prepared_foot/cs_lines.tsv") %>% 
  mutate(syl= str_remove_all(pos_syl, "[A-Z]"))


set.seed(28)
s <- sample_lines(df,n_lines = 100,n_samples = 40,label = "foot")

d <- vectorizer(s,mff = NA,ngram = 1,ftr = "syl",scale=T)

#draw_scatter(d,xpos = 2,ypos = 1,plt=plt[c(4,6)]) + labs(x="Monosyllables",y="Disyllables")
#ggsave("plots/monosyllables/monosyl_ru.jpeg",height = 6,width = 8)

##########################
### 4 meters syllables ###
##########################

p2 <- draw_scatter(d,xpos = 1,ypos = 2,plt=plt[c(3,5,4,6)],filter = F) + labs(x="Monosyllables",y="Disyllables",title = "b.")

#ggsave("plots/monosyllables/monosyl_all.jpeg",height = 6,width = 8)

set.seed(301)
s <- sample_lines(df,n_lines = 100,n_samples = 60,label = "foot")
d <- vectorizer(s,mff = NA,ngram = 1,ftr = "token",scale=T)


p1<-tibble(x=d[,1],
       label0=rownames(d)) %>% 
  mutate(label=str_remove(label0,"_.*")) %>% 
  filter(label %in% c("iamb", "trochee")) %>% 
  ggplot(aes(x, fill=label)) + geom_dotplot(binwidth=0.175,stackgroups = T, binpositions = "all") + 
  scale_fill_manual(values=plt[c(4,6)]) + theme_minimal() + #+ ylim(0,0.25)  + 
  theme(axis.title = element_text(size=10), plot.title = element_text(size=14), axis.text.y=element_blank(), plot.background = element_rect(fill="white",color=NA),legend.title=element_blank()) + 
  labs(x="Scaled frequency of 'a' ('and') ", y=NULL, title="a.")  + guides(fill="none")
p1
library(patchwork)

p1+p2
ggsave("simple_feature.png",width = 9,height = 4)
### 