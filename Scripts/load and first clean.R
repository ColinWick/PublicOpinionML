library(tidyverse)
library(survey)
library(randomForest)
library(splines)
library(pdp)


srv <- haven::read_dta("Data/2016panel/anes_timeseries_2016.dta") 

varlist <- read.csv("Data/2016panel/varlist.csv") %>%
  filter(use == "X")

srv <- srv %>%
  select(varlist$var) %>%
  filter(!(V162370 %in% c(-5,-6)))

names(srv) <- varlist$name

srv <- srv %>%
  column_to_rownames("respondent_id")

srv_use <- srv %>%
  select()
  

##################
#
# Creating workable demographic variables
#
##################

demovar <- varlist %>%
  filter(type == "DEMO")

srv_use <- srv %>%
  select(demovar$name)%>%
  apply(FUN = survey_na,MARGIN = c(1,2)) %>% data.frame() %>%
  mutate(STATE_FIPS = factor(STATE_FIPS),
         age = as.numeric(age),
         college = case_when(educ > 9 ~ 1,
                             educ %in% c(0:9) ~ 0,
                             TRUE ~ NA_real_),
         veteran = factor(veteran),
         black = ifelse(race == 2,1,0),
         white = ifelse(race == 1,1,0),
         asian = ifelse(race == 3,1,0),
         hisp = ifelse(race == 5 | hisp == 1,1,0),
         married = ifelse(married == 1,1,0),
         female = ifelse(gender == 2,1,0),
         inc_lt_50k = ifelse(income < 15,1,0),
         inc_gt_125k = ifelse(income >= 25,1,0),
         lgbt = ifelse(sexual_orientation %in% c(2,3),1,0)) %>%
  select(-income,-sexual_orientation,-race,-gender) %>%
  merge(srv_use,by=0,all.y=T) %>% select(-Row.names)

##################
#
# Centering attitude variables and setting "left" < 0 and "right" > 0
#
##################

attvar <- varlist %>%
  filter(type == "ATTITUDE")

source("Scripts/Feeling Themometer Centering.R")

srv_use <- srv %>%
  select(attvar$name) %>%
  apply(FUN = survey_na,MARGIN = c(1,2)) %>% data.frame() %>%
  mutate(vote_2012_choice = case_when(vote_2012_choice == 1 ~ -1,
                                      vote_2012_choice == 2 ~ 1,
                                      TRUE ~ 0),
         candidate_primary = case_when(candidate_primary == 1 ~ -1,
                                       candidate_primary == 2 ~ -2,
                                       candidate_primary == 3 ~ -1,
                                       candidate_primary %in% c(6:8) ~ 1,
                                       candidate_primary %in% c(4,5) ~ 2,
                                       TRUE ~ 0),
         which_campaign_money = case_when(which_campaign_money == 1 ~ -1,
                                          which_campaign_money == 2 ~ 1,
                                          TRUE ~ 0),
         which_party_money = case_when(which_party_money == 1 ~ -1,
                                          which_party_money == 2 ~ 1,
                                          TRUE ~ 0)) %>%
  select(vote_2012_choice,candidate_primary,which_campaign_money,which_party_money) %>%
  merge(srv_use,by=0,all.y=T) %>% select(-Row.names)

srv_att <- srv %>%
  select(attvar$name[!is.na(attvar$scale)],-FT_gunaccess) %>%
  mutate(lib_con_selfplace = ifelse(lib_con_selfplace == 99,3.5,lib_con_selfplace))
srv_att <- apply(MARGIN = c(1,2),FUN = refused_attitude,X = srv_att) %>% data.frame() 
srv_att <- apply(MARGIN = c(1,2),FUN = survey_na_att,X = srv_att) %>% data.frame()



n <- ncol(srv_att)
for(i in c(1:n)){
  flip_i = attvar$flip[i]
  scale_i = attvar$scale[i]
  srv_att[,i] <- DK_neutral(x = srv_att[,i],flip = flip_i,scale = scale_i)
  srv_att[,i] <- FT_center(x = srv_att[,i],flip = flip_i,scale = scale_i)
}


### This chunk was rooting out sources of huge numbers of "missing values" which mainly included house and senate opinion

#srv_att_sum <- data.frame(summary(srv_att[!complete.cases(srv_att),]))
#srv_att_sum %>% cbind(str_split(srv_att_sum$Freq,pattern = ":",simplify = T))  %>%
#  pivot_wider(names_from = `1`,values_from = `2`,id_cols = "Var2",names_repair = "unique") %>%  
#  rename_with(.fn = trimws) %>% rename("NAs" = `NA's`) %>% select(Var2,NAs) %>% 
#  mutate(NAs = as.numeric(trimws(NAs))) %>% arrange(desc(NAs)) %>% data.frame

#srv_att_c <- srv_att[complete.cases(srv_att),]

srv_att$ideology_score <- srv_att %>%
  rowSums() 
srv_att$ideology_score_std <- srv_att %>%
  rowSums() %>% scale


ggcorrplot::ggcorrplot(cor(srv_att,use = "pairwise.complete"))
srv_att_c <- srv_att[complete.cases(srv_att),] 
srv_attpcs = prcomp(srv_att_c, scale=TRUE, rank=7)
summary(srv_attpcs)

loadings_summary = srv_attpcs$rotation %>%
  as.data.frame() %>%
  rownames_to_column('Question')

loadings_summary %>%
  select(Question, PC1) %>%
  arrange(desc(PC1))

srv_use <- merge(srv_use,srv_att,by=0,all.x=T) %>% select(-Row.names)

##########################
#
# Import and clean "ACTION" variables
#
##########################

actionvar <- varlist %>%
  filter(type == "ACTION")

srv_action <- srv %>%
  select(actionvar$name) %>% 
  mutate(
    vote_2012 = ifelse(vote_2012 == 1,1,0),
    registered_vote = ifelse(registered_vote %in% c(1,2),1,0),
    party_reg = ifelse(party_reg %in% c(1,2,5),1,0),
    vote_in_primary = ifelse(vote_in_primary == 1,1,0),
    justify_use_violence = ifelse(justify_use_violence == 1,0,1),
    justify_violence_str = ifelse(justify_use_violence > 3,1,0),
    attend_rally = ifelse(attend_rally == 1,1,0),
    wear_button = ifelse(wear_button == 1,1,0),
    work_party = ifelse(work_party == 1,1,0),
    give_money_campaign = ifelse(give_money_campaign == 1,1,0),
    give_money_party = ifelse(give_money_party == 1,1,0),
    protested = ifelse(protested == 1,1,0),
    signed_petition = ifelse(signed_petition == 1,1,0),
    money_to_religious = ifelse(money_to_religious == 1,1,0),
    money_to_pol_org = ifelse(money_to_pol_org==1,1,0),
    dislike_supremecourt = ifelse(FT_SupremeCourt < 30,1,0),
    boycott_political = ifelse(boycott_political == 1,1,0),
    community_work = ifelse(community_work == 1,1,0),
    attend_meeting_community = ifelse(attend_meeting_community == 1,1,0),
    volunteer_work = ifelse(volunteer_work == 1,1,0),
    contacted_fed_EO = ifelse(contacted_fed_EO == 1,1,0),
    contacted_local_EO = ifelse(contacted_local_EO == 1,1,0),
    frequently_discuss = round(scale(ifelse(days_last_week_discuss > 0,days_last_week_discuss,0)),3)+1,
    num_orgs_member = ifelse(num_orgs_member %in% c(20:100),20,num_orgs_member),
    num_orgs_member = round(scale(ifelse(num_orgs_member < 0,0,num_orgs_member),1),3)+1) %>%
  select(-FT_SupremeCourt,-days_last_week_discuss)

srv_action$action_score <- srv_action %>%
  rowSums() 

srv_use <-  merge(srv_use,srv_action,by=0,all.x=T) %>% select(-Row.names)

###########
#
# Import and clean INPUT 
#
###########

inputvar <- varlist %>%
  filter(type == "INPUT")

srv %>%
  select(inputvar$name) %>% 
  mutate(days_media_consumption = scale(ifelse(days_media_consumption > -1,days_media_consumption,0)),
         senate_race = ifelse(senate_race == 1,1,0),
         gov_race = ifelse(gov_race == 1,1,0),
         health_ins = ifelse(health_ins == 1,1,0),
         goodhealth = ifelse(health > 2,1,0),
         jewish = ifelse(voter_religion == 6,1,0),
         catholic = ifelse(voter_religion == 4,1,0),
         protestant = ifelse(voter_religion %in% c(1,2,3,5),1,0),
         evangelical = ifelse(voter_religion == 2,1,0),
         atheist = ifelse(voter_atheist == 1,1,0),
         spiritual = ifelse(voter_spiritual_not_religious == 1,1,0),
         working = ifelse()
         ) %>% select(-health,-voter_religion,-voter_atheist,-voter_spiritual_not_religious,-voter_evangelical)
  names()
