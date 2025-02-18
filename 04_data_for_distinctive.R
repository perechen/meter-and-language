# main
library(tidyverse)
library(tidytext)
library(stringi)

vectorizer2 <- function(x,mff=50,ftr="token",ngram=1,scale=T) {
  
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
  
  if(mff > ncol(dtm)) {
    mff <- ncol(dtm)
  }
  
  dtm_s <- dtm[,1:mff]/rowSums(dtm[,1:mff])
  dtm_s[which(is.na(dtm_s))] <- 0
  
  if(scale) {
    dtm_s <- scale(dtm_s)
  }
  
  return(dtm_s)
}

sample_stratified <- function(x,n_lines=1000,max=30,label=NA) {
  
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
  sample_pool <- pool %>% 
    count(label, sort=T) %>%
    mutate(n_samples = floor(floor(n/n_lines)/10)*10,
           n_samples = ifelse(n_samples > max, max, n_samples)) %>% 
    arrange(label)
  
  groups <- pool %>% group_by(label) %>% group_split()
  
  sample_out <- vector(mode="list",length = nrow(sample_pool))
  
  for(i in 1:length(groups)) {
    
    sample_out[[i]] <- groups[[i]] %>% 
      sample_n(n_lines*sample_pool$n_samples[i]) %>% # take all that is required
      slice_sample(prop=1) %>% # shuffle within group
      mutate(sample_no=ceiling(row_number()/n_lines)) %>% # annotate buckets 
      ungroup() 
    
    
  }
  
  return(sample_out %>% bind_rows())
}


corp <- list.files("data/prepared/",full.names = T)
langs <- c("cs","de","ru")
path <- "data/dump_features/"


label <- c(NA, "foot")

for(c in 1:length(corp)) {
  df <- read_tsv(corp[c]) %>% 
  mutate(syl= str_remove_all(pos_syl, "[A-Z]"))

for(l in label) {
s <- sample_stratified(df,
                  n_lines = 500,
                  max = 50,
                  label = l)

ngram <- c(1,2)
features <- c("token", "pos", "syl", "pos_syl")
# ng
for(f in features) {
  for(ng in ngram) {
    if (f=="token") {
      mff=100
    } else {mff=1000}
  
message(paste0("Now at: ",langs[c], "_", 
               ifelse(is.na(l), "meter","foot"), "_", f, "_", ng))

d <- vectorizer2(s,mff = mff,ngram = ng,ftr = f,scale=T)

out <- bind_cols(tibble(sample_id=rownames(d),
                        class_label=str_remove(sample_id, "_[0-9]*?$")), 
                 as_tibble(d))

write_csv(out,paste0("data/dump_features/", langs[c], "/",langs[c], "_", 
                     ifelse(is.na(l), "meter","foot"), "_", f, "_", ng, ".csv"))
  }
}
}
}
