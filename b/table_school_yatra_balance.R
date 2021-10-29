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
#           Table 4: Schools by distance from yatra
#------------------------------------------------------------------------

schools <- data_balance_t %>%
  select(c("pc11_td_p_sch",
           "pc01_td_p_sch",
           "pc91_td_p_sch",
           "pc11_td_s_sch",
           "pc01_td_s_sch",
           "pc91_td_s_sch",
           "pc11_td_college",
           "pc01_td_college",
           "pc91_td_college",
           "pc11_pca_tot_p_u",
           "pc01_pca_tot_p_u",
           "pc91_pca_tot_p_u",
           "yatra")) %>%
  distinct()

schools <- schools %>%
  mutate(p_sch_91 = (pc91_td_p_sch / pc91_pca_tot_p_u) * 10000,
         p_sch_01 = (pc01_td_p_sch / pc01_pca_tot_p_u) * 10000,
         p_sch_11 = (pc11_td_p_sch / pc11_pca_tot_p_u) * 10000,
         s_sch_91 = (pc91_td_s_sch / pc91_pca_tot_p_u) * 10000,
         s_sch_01 = (pc01_td_s_sch / pc01_pca_tot_p_u) * 10000,
         s_sch_11 = (pc11_td_s_sch / pc11_pca_tot_p_u) * 10000,
         colle_91 = (pc91_td_college/ pc91_pca_tot_p_u) * 10000,
         colle_01 = (pc01_td_college/ pc01_pca_tot_p_u) * 10000,
         colle_11 = (pc11_td_college/ pc91_pca_tot_p_u) * 10000) %>%
  select("p_sch_91",
         "p_sch_01",
         "p_sch_11",
         "s_sch_91",
         "s_sch_01",
         "s_sch_11",
         "colle_91",
         "colle_01",
         "colle_11",
         "yatra")  

schools_balance <- tbl_summary(data = schools,
                               by = yatra,
                               statistic = list(all_continuous() ~ "{mean} ({sd})"),
                               type = list(yatra ~ "categorical"),
                               missing = "no") %>%
  as_tibble()
write.csv(schools_balance, file = str_c(paths[['a']], 'table_schools_balance.csv'))