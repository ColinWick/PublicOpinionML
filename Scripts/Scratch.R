
removers <- srv_ideo %>% select(-ideo_label) 
removers[!is.na(removers)] <- 0
removers[is.na(removers)] <- 1
removers %>% group_by(year) %>% data.frame() %>% rownames_to_column("var") %>% rename("value" = 2) %>% arrange(desc(value))

drops <- cbind(unlist(lapply(srv_use[srv_use$year==2008,],function(x) sum(is.na(x)))),
      unlist(lapply(srv_use[srv_use$year==2012,],function(x) sum(is.na(x)))),
      unlist(lapply(srv_use[srv_use$year==2016,],function(x) sum(is.na(x))))) %>% data.frame() %>% rownames_to_column("var")

drops <- unlist(lapply(srv_use,function(x) sum(is.na(x)))) %>% data.frame() %>% rownames_to_column("var")
drops
drops <- drops[drops[,2] > 6000,][,1]

table(srv$born_again)


mynames <- names(x)
i <-1
years <- seq(2008,2016,4)
drops_year <- drops$var[drops[,i+1] > 1000]
drops_year

x <- srv_use %>% filter(year == years[1]) %>% select(-drops_year,-ideo_label,-year,-ideo_score) 
x <- x[complete.cases(x),] %>% select(ideovar$name[ideovar$name %in% names(x)])
pca_ideo <- prcomp(x, rank=5, scale=TRUE)
pca_ideo$rotation %>%
  as.data.frame %>%
  rownames_to_column('topic') %>%
  select(topic, PC1) %>%
  arrange(desc(PC1))
