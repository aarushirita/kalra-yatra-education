#-----------------------------------------------------------------------------------------------------------------------------------------
#                                       Summary Statistics Tables
#----------------------------------------------------------------------------------------------------------------------------------------

rm(list=ls())

library(dplyr)
library(tidyverse)
library(foreign)
library(matlib)
library(devtools)
library(AER)

#functions and pathnames
source("C:/Users/aarus/Google Drive/communal_violence/b/aarushi_paths.R")
source("C:/Users/aarus/Google Drive/communal_violence/b/functions.R")
source_gist(4676064)

#load dataset
load(paths[['sr_data']])

#-----------------------------------------------------------------------------------
#                   filter relevant observations
#-----------------------------------------------------------------------------------

data_sumstat <- sr_data %>%
  #pick yatra states
  filter(state_name != "jammu kashmir",
         state_name != "himachal pradesh",
         state_name != "punjab",
         state_name != "uttarakhand",
         state_name != "sikkim",
         state_name != "manipur",
         state_name != "tripura",
         state_name != "assam",
         state_name != "west bengal",
         state_name != "odisha",
         state_name != "goa",
         state_name != "kerala",
         state_name != "tamil nadu",
         state_name != "meghalaya",
         state_name != "mizoram",
         state_name != "arunachal pradesh",
         state_name != "nagaland",
         state_name != "dadra nagar haveli",
         state_name != "daman diu",
         state_name != "lakshadweep",
         state_name != "puducherry",
         state_name != "andaman nicobar islands",
         state_name != "chandigarh",
         state_name != "karnataka",
         #do not use uttar pradesh for balance tables
         state_name != "uttar pradesh",
         #consider males in sample                  
         #gender == "1",
         #dropping STs from the sample
         social_group != "1",
         #droppping SCs from sample
         social_group != "2") %>%
  mutate(cohort = as.factor(cohort),
         muslim = as.factor(muslim)
  ) %>%
  filter(cohort != 0,
         cohort != 6,
         yatra_distance <= 250) 

#-----------------------------------------------------------------------------------
#               Table 1: Distribution of Education by Cohort and Religion
#-----------------------------------------------------------------------------------

cohort_edu <- data_sumstat %>%
              group_by(muslim, cohort) %>%
              summarise(
                        per_illeterate = (sum(edu_level == 0, na.rm = TRUE) / sum(!is.na(edu_level)) * 100),
                        per_primary = (sum(edu_level >= 3, na.rm = TRUE) / sum(!is.na(edu_level)) * 100),
                        per_middle = (sum(edu_level >= 4, na.rm = TRUE) / sum(!is.na(edu_level)) * 100),
                        per_secondary = (sum(edu_level >= 5, na.rm = TRUE) / sum(!is.na(edu_level)) * 100),
                        per_college = (sum(edu_level >= 7, na.rm = TRUE) / sum(!is.na(edu_level)) * 100)
                        ) %>%
              ungroup() 

cohort_edu <- t(cohort_edu[order(cohort_edu$cohort) , ])
write.csv(cohort_edu, str_c(paths[['a']], 'table_cohort_edu.csv'))
