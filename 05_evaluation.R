library(tidyverse)
library(caret)

f <- list.files("results/conf_matrices/",full.names = T)

df <- tibble(path=f) %>%
  mutate(path2 = path,
         path2 = str_replace(path2, "pos_syl", "possyl"),
         feat=str_remove(path2,"^.*?//")) %>% 
  separate(feat, c("lang", "feature","size","ngram","type","i"))

sum_matrix_df <- function(df,l="cs",f="token",s=100,ng=1,type="foot") {
  
cm_group <- df %>% 
  filter(lang==l,
         feature==f,
         size==s,
         ngram==ng,
         type==type)

cms <- lapply(cm_group$path, read.table)

## sum confusion matrices
for(i in 1:length(cms)) {
  m0 <- cms[[i]]
  if(i == 1) {
    m <- m0
  }
  if(i > 1) {
    m <- m + m0
  }
}

cm_df <- as_tibble(m) %>%
  mutate(expected=rownames(m),
         lang=l,
         feature=f,
         size=s,
         ngram=ng,
         type=type) %>% 
  pivot_longer(cols = 1:nrow(m),names_to = "predicted") %>% 
  mutate(expected = factor(expected,levels = rev(rownames(m)))) 
return(cm_df)

}


feat <- "possyl"
lang <- "de"
l_lab <- "German"
ngr <- 2

s <- as.numeric(df$size) %>% unique()
l <- vector(mode="list",length=8)



for(i in 1:length(s)) {
  
l[[i]] <- sum_matrix_df(df,s = s[i],f = feat,ng=ngr,l=lang) 
}

l %>% bind_rows() %>% 
  mutate(is_bright=ifelse(value>700, T,F)) %>% 
  ggplot(aes(expected,predicted)) + 
  facet_wrap(~size,nrow = 1) +
  geom_tile(aes(fill=value)) + 
  coord_flip() + 
  scale_fill_viridis_c(option = "A") + 
  geom_text(aes(label=value,color=is_bright),size=1.5) + 
  scale_color_manual(values=c("white","black")) + guides(fill="none",color="none") + theme_light() + theme(axis.text.x = element_text(angle=45,vjust = 1,hjust = 1)) + labs(title=paste0(l_lab,", ",feat, ", ", ngr,"gram"))

ggsave(paste0("plots/CM_plots/forms/cm_",lang,"_",feat,"_n", ngr,".png"),width = 16,height = 3)

## F1 

calculate_f1 <- function(x) {
  t<-read.table(x) %>% as.matrix()
  f1 <- caret::confusionMatrix(t,mode = "prec_recall")
  macro_f1 <- mean(f1$byClass[,"F1"])
  return(macro_f1)
}

f1_v <- sapply(df$path, calculate_f1)

df_f1 <- df %>% mutate(f1 = f1_v,
                       f1 = ifelse(is.na(f1), 0, f1))

df_f1 %>% 
  group_by(lang,feature,size,ngram,type) %>% 
  summarize(f1=mean(f1)) %>% 
  ggplot(aes(as.numeric(size),f1, color=feature,linetype=ngram)) +
  geom_line() + 
  facet_wrap(~lang)
