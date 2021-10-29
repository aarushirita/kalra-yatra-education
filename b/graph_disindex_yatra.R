#----------------------------------------------------------------------------------
#                             Graph Reduced Form
#---------------------------------------------------------------------------------

rm(list=ls())

library(dplyr)
library(tidyverse)
library(foreign)
library(matlib)
library(devtools)
library(AER)
library(fixest)

#functions and pathnames
source("C:/Users/aarus/Google Drive/communal_violence/b/aarushi_paths.R")
source("C:/Users/aarus/Google Drive/communal_violence/b/functions.R")
source_gist(4676064)

#load dataset
load(paths[['sr_data']])

data_balance_g <- sr_data %>%
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



#----------------------------------------------------------------------------------
#                   Figure 16: Segregation by Yatra Distance
#---------------------------------------------------------------------------------

disindex_yatra <- data_balance_g %>%
  distinct(yatra_distance,
           dis_index,
           pc11_pca_tot_p_u) %>%
  ggplot(aes(x=yatra_distance, 
             y=dis_index,
             weight=pc11_pca_tot_p_u,
             size=pc11_pca_tot_p_u)) +
  xlab("Distance of town from Rath Yatra Route (in km)") +
  ylab("Dissimilarity Index") +
  geom_smooth(method='lm',
              formula=y ~ x,
              se = T,
              color='dark green') +
  stat_summary_bin(aes(size = pc11_pca_tot_p_u),
                   fun = 'weighted.mean',
                   bins=40,
                   color='dark green',
                   geom = 'point',
                   alpha=0.6)     +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14)) 
png(str_c(paths[['a']], "graph_disindex.png"))
print(disindex_yatra)
dev.off()