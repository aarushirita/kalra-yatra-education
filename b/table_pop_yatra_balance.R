#--------------------------------------------------------------------------------------------------------------
#                                Balance Tables
#--------------------------------------------------------------------------------------------------------------

rm(list=ls())

library(dplyr)
library(tidyverse)
library(foreign)
library(matlib)
library(devtools)
library(gtsummary)


#functions and pathnames
source("C:/Users/aarus/Google Drive/communal_violence/b/aarushi_paths.R")
source("C:/Users/aarus/Google Drive/communal_violence/b/functions.R")
source_gist(4676064)

#load dataset
load(paths[['sr_data']])

#filter relevant observations
data_balance_t <- sr_data %>%
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
                              gender == "1",
  #dropping STs from the sample
                              social_group != "1",
  #droppping SCs from sample
                              social_group != "2",
  #consider yatra distance less than 250 km
                              yatra_distance <= 250)
                       
#Discrete Balance Tables
#divide into 5 groups of 50 kms each
data_balance_t <- data_balance_t %>%
                       mutate(y1 = if_else(yatra_distance <= 50, 1, 0),
                              y2 = if_else(yatra_distance <= 100 & yatra_distance > 50,  2, 0),
                              y3 = if_else(yatra_distance <= 150 & yatra_distance > 100, 3, 0),
                              y4 = if_else(yatra_distance <= 200 & yatra_distance > 150, 4, 0),
                              y5 = if_else(yatra_distance <= 250 & yatra_distance > 200, 5, 0),
                              yatra = as.factor(y1 + y2 + y3 + y4 + y5)) %>%
                       select(-c(y1, y2, y3, y4, y5)) %>%
                       as.data.frame()

#------------------------------------------------------------------------
#           Table 3: Population by distance from yatra
#------------------------------------------------------------------------

population <- data_balance_t %>% 
              select(c("pc11_pca_tot_p_u",
                       "pc01_pca_tot_p_u",
                       "pc91_pca_tot_p_u",
                       "yatra")) %>%
               distinct()

population_balance <- tbl_summary(data = population,
                                  by = yatra,
                                  statistic = list(all_continuous() ~ "{mean} ({sd})"),
                                  #digits = list(age ~ c(1, 2)),
                                  type = list(yatra ~ "categorical"),
                                  missing = "no") %>%
                       as_tibble()
write.csv(population_balance, file = str_c(paths[['a']], 'table_population_balance.csv'))