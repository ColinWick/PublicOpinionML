library(tidyverse)
library(survey)
library(randomForest)
library(splines)
library(pdp)
library(mice)

source("Scripts/Feeling Themometer Centering.R")


srv <- haven::read_dta("Data/19482019timeseries/anes_timeseries_cdf.dta") 

#srv <- srv %>% filter(VCF0004 == 2016 & VCF0014 == 1 & VCF0013==1)
#data.frame(colSums(is.na(srv))) %>% 
#  mutate(var = rownames(data.frame(colSums(is.na(srv))))) %>% 
#  rename("count"=1) %>% arrange(count)

varlist <- read.csv("Data/19482019timeseries/varlist_all.csv") %>%
  filter(use == "X") 

srv <- srv %>%
  select(varlist$var) #%>%

names(srv) <- varlist$name

srv <- srv %>%
  filter(year %in% seq(2000,2016,4)) %>%
  filter(complete_pre == 1) %>%
  mutate(FT_Feminists == ifelse(!is.na(FT_Feminists),FT_Feminists,FT_Womens_Libbers)) %>%
  select(-FT_Womens_Libbers)

data.frame(colSums(is.na(srv))) %>% rename("count"=1) %>% arrange(count)


srv_use <- srv %>%
  select()

srv_yearonly <- srv %>%
  select(year)

##################
#
# Creating workable demographic variables
#
##################

demovar <- varlist %>%
  filter(type == "DEMO")

srv_use <- srv %>%
  select(demovar$name) %>%
  apply(FUN = survey_na,MARGIN = c(1,2)) %>% data.frame() %>%
  mutate(STATEFIPS = factor(STATEFIPS),
         age = as.numeric(age),
         college = case_when(educ %in% c(5,6) ~ 1,
                             educ %in% c(1:4) ~ 0,
                             TRUE ~ 0),
         black = ifelse(race == 2,1,0),
         white = ifelse(race == 1,1,0),
         asian = ifelse(race == 3,1,0),
         hisp = ifelse(race == 5 | hisp == 1,1,0),
         married = ifelse(married == 1,1,0),
         female = ifelse(gender == 2,1,0),
         inc_firstthird = ifelse(income <= 2,1,0),
         inc_upperthird= ifelse(income >= 4,1,0),
         lgbt = ifelse(sexual_orientation %in% c(2,3),1,0),
         lowerclass = ifelse(class %in% c(0,1,2,3),1,0),
         middleclass = ifelse(class %in% c(4,5,6),1,0),
         own_home = ifelse(own_home == 1,1,0),
         jewish = ifelse(religion == 3,1,0),
         catholic = ifelse(religion == 2,1,0),
         protestant = ifelse(religion == 1,1,0),
         nonreligious = ifelse(religion == 4,1,0)) %>%
  select(-income,-sexual_orientation,-race,-gender,-religion,-educ,-class) %>%
  merge(srv_use,by=0,all.y=T) %>% select(-Row.names)

##################
#
# Centering attitude variables and setting "left" < 0 and "right" > 0
#
##################

attvar <- varlist %>%
  filter(type == "ATTITUDE")

srv_att <- srv %>%
  select(attvar$name[!is.na(attvar$scale)]) %>% data.frame()

#srv_att <- apply(MARGIN = c(1,2),FUN = refused_attitude,X = srv_att) %>% data.frame() 
#srv_att <- apply(MARGIN = c(1,2),FUN = survey_na_att,X = srv_att) %>% data.frame()

n <- ncol(srv_att)

for(i in c(1:n)){
  flip_i = attvar$flip[i]
  scale_i = attvar$scale[i]
  #srv_att[,i] <- DK_neutral(x = srv_att[,i],flip = flip_i,scale = scale_i)
  srv_att[,i] <- scale(FT_center(x = srv_att[,i],flip = flip_i,scale = scale_i))
}

### This chunk was rooting out sources of huge numbers of "missing values" which mainly included house and senate opinion

#srv_att_sum <- data.frame(summary(srv_att[!complete.cases(srv_att),]))
#srv_att_sum %>% cbind(str_split(srv_att_sum$Freq,pattern = ":",simplify = T))  %>%
#  pivot_wider(names_from = `1`,values_from = `2`,id_cols = "Var2",names_repair = "unique") %>%  
#  rename_with(.fn = trimws) %>% rename("NAs" = `NA's`) %>% select(Var2,NAs) %>% 
#  mutate(NAs = as.numeric(trimws(NAs))) %>% arrange(desc(NAs)) %>% data.frame

#srv_att_c <- srv_att[complete.cases(srv_att),]

srv_att <- srv_att %>%
  merge(srv_yearonly,by=0,all.x = T) %>% select(-Row.names)

        
        srv_att_c <- srv_att[srv_att$year==2016,]
        srv_att_c <- srv_att_c[,colSums(is.na(srv_att_c))<nrow(srv_att_c)]
        srv_att_c <- srv_att_c[complete.cases(srv_att_c),] %>% select(-year)
        srv_att
        srv_attpcs = prcomp(srv_att_c, scale=TRUE,rank. = 4)
        summary(srv_attpcs)
        
        loadings_summary = srv_attpcs$rotation %>%
          as.data.frame() %>%
          rownames_to_column('Question')
        
        loadings_summary %>%
          select(Question, PC2) %>%
          arrange(desc(PC2))

srv_att$att_score <- srv_att %>% rowSums(na.rm=T) 
srv_att$att_score_std <- srv_att %>% rowSums(na.rm=T) %>% scale()

srv_use <- merge(srv_use,srv_att,by=0,all.x=T) %>% select(-Row.names)

##########################
#
# Import and clean "ACTION" variables
#
##########################

actionvar <- varlist %>%
  filter(type == "ACTION")

actionvar$name[!is.na(actionvar$scale)]

srv_use <- srv %>%
  select(actionvar$name[is.na(actionvar$scale)]) %>% 
   mutate(
    tried_influence = ifelse(tried_influence == 1,1,0),
    attend_rally = ifelse(attend_rally == 1,1,0),
    wear_button = ifelse(wear_button == 1,1,0),
    work_party = ifelse(work_party == 1,1,0),
    give_money_campaign = ifelse(give_money_campaign == 1,1,0),
    give_money_party = ifelse(give_money_party == 1,1,0),
    campaign_participation = as.numeric(campaign_participation),
    community_meeting = ifelse(community_meeting==1,1,0),
    community_work = ifelse(community_work==1,1,0),
    days_last_week_discuss = as.numeric(days_last_week_discuss)) %>% 
   select(-days_last_week_discuss) %>% 
   data.frame() %>% merge(srv_use,by=0,all.y=T) %>% select(-Row.names)

###########
#
# Import and clean INPUT 
#
###########

inputvar <- varlist %>%
  filter(type == "INPUT")

srv_use <- srv %>% select(inputvar$name) %>% data.frame() %>%
  mutate(media_TV = ifelse(media_TV == 1,1,0),
         media_radio = ifelse(media_radio == 1,1,0),
         media_paper = ifelse(media_paper == 1,1,0),
         media_internet = ifelse(media_internet == 1,1,0),
         access_internet = ifelse(access_internet == 1,1,0),
         religion_important = ifelse(religion_important == 1,1,0),
         delay_medical_cost = ifelse(delay_medical_cost == 1,1,0),
         better_off_econ1 = ifelse(better_off_econ1 %in% c(8,9),3,better_off_econ1),
         better_off_ny_econ = ifelse(better_off_ny_econ %in% c(8,9),3,better_off_ny_econ),
         churchgoer = ifelse(church_attendance %in% c(1:3),1,0),
         born_again = ifelse(born_again == 1,1,0),
         health_ins = ifelse(health_ins == 1,1,0),
         money_stocks = ifelse(money_stocks == 1,1,0)) %>% select(-church_attendance) %>%
  merge(srv_use,by=0,all.y=T) %>% select(-Row.names)


###########
#
# Import and clean Politicization 
#
###########

polvar <- varlist %>%
  filter(type == "POL")

polvar$name
srv_use <- srv %>% select(polvar$name) %>% data.frame() %>%
  mutate(
  class_conscious = ifelse(class_conscious %in% c(1,3),1,0),
  approve_fedgov = scale(approve_fedgov),
  how_much_campaign = ifelse(how_much_campaign == 9,0,how_much_campaign),
  interest_public_affairs = ifelse(interest_public_affairs==9,0,interest_public_affairs),
  politics_complicated = case_when(politics_complicated == 1~1,
                                   politics_complicated == 2~-1,
                                   TRUE ~ 0),
  trust_media = ifelse(trust_media < 3,1,0),
  contacted_by_party = ifelse(contacted_by_party == 1,1,0),
  contacted_by_other_nonparty = ifelse(contacted_by_other_nonparty == 1,1,0),
  satisfied_democracy = ifelse(satisfied_democracy %in% c(1,2),1,0),
  how_often_pay_attention = factor(how_often_pay_attention)) %>% 
  merge(srv_use,by=0,all.y=T) %>% select(-Row.names)

###########
#
# Import and clean Politicization 
#
###########

ideovar <- varlist %>%
  filter(type == "IDEO") %>%
  filter(name != "FT_Womens_Libbers")

srv_ideo <- srv %>%
  select(ideovar$name[!is.na(ideovar$scale)]) %>% data.frame()


n <- ncol(srv_ideo)

for(i in c(1:n)){
  flip_i = ideovar$flip[i]
  scale_i = ideovar$scale[i]
  #srv_ideo[,i] <- DK_neutral(x = srv_ideo[,i],flip = flip_i,scale = scale_i)
  srv_ideo[,i] <- FT_center(x = srv_ideo[,i],flip = flip_i,scale = scale_i)
}

srv_ideo <- srv_ideo %>%
  merge(srv_yearonly,by=0,all.x = T) %>% select(-Row.names)

srv_ideo$ideo_score <- apply(FUN = scale,MARGIN = 2,X = srv_ideo) %>% data.frame() %>% select(-year) %>% rowSums(na.rm=T)

srv_ideo$ideo_score_std <- apply(FUN = scale,MARGIN = 2,X = srv_ideo) %>% data.frame() %>% select(-year) %>% rowSums(na.rm=T) %>% scale()

srv_ideo %>% 
ggplot()+
ggridges::geom_density_ridges(aes(x=ideo_score,group=year,y=year))+
  ggtitle("Relative ideological consistency of the electorate","composite variable, by year")

rm(list = ls()[!ls() == "srv_use"])
srv_use <- merge(srv_use,srv_ideo,by=0,all.x=T) %>% select(-Row.names)
