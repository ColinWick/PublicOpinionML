

srv_ideo1 <- srv_ideo %>%
  merge(srv_yearonly,by=0,all.x = T) %>% select(-Row.names)
srv_ideo_scl <- srv_ideo1 %>% filter(year == 2012)

ideo_score <- srv_ideo_scl$ideo_score_std

srv_ideo_scl <- srv_ideo_scl %>% select(-year,-FT_Jews,-either_extreme,-which_party_give,-vote_party,-left_extreme,-right_extreme,-FT_DEMPARTY,-FT_REPPARTY,-PARTY_ID,-ideo_score,-ideo_score_std)
srv_ideo_scl[is.na(srv_ideo_scl)] <- 0
srv_ideo_scl <- srv_ideo_scl %>% scale(center = T,scale = T) 
srv_ideo_scl <- sweep(x = srv_ideo_scl,MARGIN = 1,FUN="-",STATS = ideo_score)
srv_ideo_closeness <- srv_ideo_scl %>% data.frame() %>% rownames_to_column("X") %>%
  pivot_longer(-X) %>%
  mutate(edge = ifelse(!is.na(value),1,0)) %>%
  filter(edge != 0 & abs(value) <.2) %>% unique() #%>% select(name,X)

srv_ideo_close = split(x = srv_ideo_closeness$name,f=srv_ideo_closeness$X) 
srv_ideo_close <- as(srv_ideo_close,"transactions")



topic_assoc = apriori(srv_ideo_close, 
                      parameter=list(support=.05, confidence=.05, maxlen=10))

sub1 = subset(topic_assoc, subset=confidence > 0.1 & support > 0.1)

plot(head(topic_assoc, 20, by='lift'), method="graph" )

graph.edgelist(srv_ideo_close)
