library(tidyverse)
library(survey)
library(randomForest)
library(splines)
library(pdp)

source("Scripts/Feeling Themometer Centering.R")


srv <- haven::read_dta("Data/19482019timeseries/anes_timeseries_cdf.dta") 

varlist <- read.csv("Data/19482019timeseries/varlist_all.csv") %>%
  filter(use == "X") 

srv <- srv %>%
  select(varlist$var) #%>%

names(srv) <- varlist$name

srv <- srv %>%
  filter(year %in% seq(2008,2016,4)) %>%
#  filter(year == 2016) %>%
  filter(complete_pre == 1) 
srv <- srv %>%  mutate(FT_Feminists = ifelse(!is.na(FT_Feminists),FT_Feminists,FT_Womens_Libbers)) %>%
  select(-FT_Womens_Libbers)

drops <- unlist(lapply(srv,function(x) sum(is.na(x)))) %>% data.frame() %>% rownames_to_column("var")
#drops <- drops[drops[,2] > 2000,][,1]
drops <- drops[drops[,2] > 5000,][,1]
srv <- srv %>% select(-drops)

varlist <- varlist %>% filter(!(name %in% drops))

srv_use <- srv %>%
  select()

srv_tech <- srv %>%
  select(varlist$name[varlist$type == "technical"])

##################
#
# Creating workable demographic variables
#
##################

demovar <- varlist %>%
  filter(type == "DEMO")

srv$knowl_house_maj[is.na(srv$knowl_house_maj)] <- 1
srv$money_stocks[is.na(srv$money_stocks)] <- 2
srv$discuss[is.na(srv$discuss)] <- 5


srv_use <- srv %>%
  select(demovar$name) %>%
  apply(FUN = survey_na,MARGIN = c(1,2)) %>% data.frame() %>%
  mutate(STATEFIPS = factor(STATEFIPS),
         age = as.numeric(age),
         college = case_when(educ %in% c(5,6) ~ 1,
                             educ %in% c(1:4) ~ 0,
                             TRUE ~ 0),
         black = ifelse(race == 2,1,0),
         hisp = ifelse(race == 5 | hisp == 1,1,0),
         married = ifelse(married == 1,1,0),
         female = ifelse(gender == 2,1,0),
         inc_firstthird = ifelse(income <= 2,1,0),
         inc_upperthird= ifelse(income >= 4,1,0),
         lgbt = ifelse(sexual_orientation %in% c(2,3),1,0),
         middleclass = ifelse(class %in% c(4,5,6),1,0),
         own_home = ifelse(own_home == 1,1,0),
         jewish = ifelse(religion == 3,1,0),
         catholic = ifelse(religion == 2,1,0),
         protestant = ifelse(religion == 1,1,0),
         nonreligious = ifelse(religion == 4,1,0),
         money_stocks = ifelse(money_stocks == 1,1,0),
         class_conscious = ifelse(class_conscious %in% c(1,3),1,0),
         knowl = ifelse(knowl_house_maj == 2,1,0),
         PARTY_ID = as.factor(ifelse(PARTY_ID==0,4,PARTY_ID)),
         str_PARTY = ifelse(PARTY_ID %in% c(1,2,6,7),1,0)) %>%
  select(-income,-sexual_orientation,-race,-gender,-religion,-educ,-class,-knowl_house_maj) 

srv_use$agesq <- srv_use$age^2
srv_use$PARTY_ID <- as.factor(srv_use$PARTY_ID)

##########################
#
# Import and clean "ATTITUDE" variables
#
##########################

attvar <- varlist %>%
  filter(type == "ATT")

srv_use <- srv %>%
  select(attvar$name) %>%
  apply(FUN = survey_na,MARGIN = c(1,2)) %>% data.frame() %>%
  mutate(trust_others = ifelse(trust_others %in% c(1:3),1,0),
         satisfied_life = ifelse(satisfied_life %in% c(1:3),1,0),
         understand_issues = ifelse(understand_issues %in% c(1:3),1,0),
         too_complicated = ifelse(too_complicated %in% c(1:3),1,0)) %>%
  merge(srv_use,by=0,all.y=T) %>% select(-Row.names)


##########################
#
# Import and clean "ACTION" variables
#
##########################

actionvar <- varlist %>%
  filter(type == "ACTION")
srv_use

srv_use <- srv %>%
  select(actionvar$name[is.na(actionvar$scale)]) %>% 
   mutate(
    tried_influence = ifelse(tried_influence == 1,1,0),
    attend_rally = ifelse(attend_rally == 1,1,0),
    wear_button = ifelse(wear_button == 1,1,0),
    work_party = ifelse(work_party == 1,1,0),
    give_money_campaign = ifelse(give_money_campaign == 1,1,0),
    give_money_party = ifelse(give_money_party == 1,1,0),
    campaign_participation = as.numeric(campaign_participation)-1,
    community_meeting = ifelse(community_meeting==1,1,0),
    community_work = ifelse(community_work==1,1,0),
    discuss = ifelse(discuss == 1,1,0)) %>%
   data.frame() %>% merge(srv_use,by=0,all.y=T) %>% select(-Row.names)


###########
#
# Import and clean Politicization 
#
###########

#polvar <- varlist %>%
#  filter(type == "POL")

#polvar$name
#srv_use <- srv %>% select(polvar$name) %>% data.frame() %>%
#  mutate(
#  
#  approve_fedgov = scale(approve_fedgov),
#  how_much_campaign = ifelse(how_much_campaign == 9,0,how_much_campaign),
#  contacted_by_party = ifelse(contacted_by_party == 1,1,0),
#  contacted_by_other_nonparty = ifelse(contacted_by_other_nonparty == 1,1,0),
#  satisfied_democracy = ifelse(satisfied_democracy %in% c(1,2),1,0),
#  how_often_pay_attention = factor(how_often_pay_attention)) %>% 
#  merge(srv_use,by=0,all.y=T) %>% select(-Row.names)

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

srv_ideo$Consistency <- apply(MARGIN = 1,FUN = sd_na,X = srv_ideo)
srv_ideo$ideo_score <- srv_ideo %>% rowSums(na.rm=T)
srv_ideo$Position <- scale(srv_ideo$ideo_score,center = T,scale = T)

srv_ideo$year = srv$year

srv_ideo$Position <- srv_ideo$Position[,1]
srv_ideo$ideo_ext <- srv_ideo$ideo_ext[,1]
srv_ideo$ideo_right <- srv_ideo$ideo_right[,1]
srv_ideo$ideo_left <- srv_ideo$ideo_left[,1]

srv_ideo %>% 
ggplot()+
ggridges::geom_density_ridges(aes(x=Position,group=year,y=year))+
  ggtitle("Relative ideological consistency of the electorate","composite variable, by year")

srv_ideo <- srv_ideo %>%
  mutate(ideo_label = case_when(Position < -1.3 ~ "left",
                                Position > 1.3 ~ "right",
                                TRUE ~ "moderate"),
         ideo_ext = ifelse(abs(Position) > 1.3,1,0),
         Extremity = abs(Position))



#rm(list = ls()[!ls() == "srv_use"])
srv_use <- merge(srv_use,srv_ideo,by=0,all.x=T) %>% data.frame() %>% select(-Row.names)

srv_use <- srv_use %>% 
  select(-names(srv_ideo),Consistency,Extremity,ideo_ext,ideo_label,ideo_score,Position) %>%
  merge(srv_tech,by=0) %>% select(-Row.names)

srv_use$Action_Score <- srv_use %>%
  select(actionvar$name) %>% rowSums(na.rm = T)
srv_use <- srv_use %>% mutate(year = factor(year),
                              STATEFIPS = factor(STATEFIPS),
                              census_region = factor(census_region))
srv_use <- srv_use[complete.cases(srv_use),]

