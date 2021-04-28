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
srv_att


n <- ncol(srv_att)
for(i in c(1:n)){
  flip_i = attvar$flip[i]
  scale_i = attvar$scale[i]
  srv_att[,i] <- DK_neutral(x = srv_att[,i],flip = flip_i,scale = scale_i)
  srv_att[,i] <- FT_center(x = srv_att[,i],flip = flip_i,scale = scale_i)
}

srv_use <- merge(srv_use,srv_att,by=0,all.x=T) %>% select(-Row.names)

### This chunk was rooting out sources of huge numbers of "missing values" which mainly included house and senate opinion

#srv_att_sum <- data.frame(summary(srv_att[!complete.cases(srv_att),]))
#srv_att_sum %>% cbind(str_split(srv_att_sum$Freq,pattern = ":",simplify = T))  %>%
#  pivot_wider(names_from = `1`,values_from = `2`,id_cols = "Var2",names_repair = "unique") %>%  
#  rename_with(.fn = trimws) %>% rename("NAs" = `NA's`) %>% select(Var2,NAs) %>% 
#  mutate(NAs = as.numeric(trimws(NAs))) %>% arrange(desc(NAs)) %>% data.frame

#srv_att_c <- srv_att[complete.cases(srv_att),]


# srv_attpcs = prcomp(srv_att_c, scale=TRUE, rank=7)
# srv_attpcs 

##########################
#
# Import and clean "ACTION" variables
#
##########################

