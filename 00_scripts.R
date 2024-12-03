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

