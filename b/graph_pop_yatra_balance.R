#----------------------------------------------------------------------------------
#                   Graph Balance Tables
#---------------------------------------------------------------------------------

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
         #consider yatra distance less than 250 km
         yatra_distance <= 250) 

#------------------------------------------------------------------------
#           Figure 6: Balance of Population at city level
#-----------------------------------------------------------------------

colors <- c("2010" = "brown3",
            "2000" = "steelblue4",
            "1990" = "olivedrab4")

pop_yatra <-  data_balance_g %>%
  distinct(yatra_distance,
         pc11_pca_tot_p_u,
         pc01_pca_tot_p_u,
         pc91_pca_tot_p_u) %>%
  filter(yatra_distance <= 100) %>%
ggplot(aes(x = yatra_distance)) +
  geom_smooth(aes(y = pc11_pca_tot_p_u,
                  color = "2010"),
              method = 'loess', 
              formula = y ~ x,
              se = T) +
  stat_summary_bin(aes(y = pc11_pca_tot_p_u,
                       color = "2010"),
                   fun = 'mean',
                   binwidth = 1,
                   geom = 'point',
                   alpha=0.6) +
  geom_smooth(aes(y = pc01_pca_tot_p_u,
                  color = "2000"),
              method = 'loess', 
              formula = y ~ x,
              se = T) +
  stat_summary_bin(aes(y = pc01_pca_tot_p_u,
                       color = "2000"),
                   fun = 'mean',
                   binwidth = 1,
                   geom = 'point',
                   alpha=0.6) +
  geom_smooth(aes(y = pc91_pca_tot_p_u,
                  color = "1990"),
              method = 'loess', 
              formula = y ~ x,
              se = T) +
  stat_summary_bin(aes(y = pc91_pca_tot_p_u,
                       color = "1990"),
                   fun = 'mean',
                   binwidth = 1,
                   geom = 'point',
                   alpha=0.6) +
  labs(x = "Distance of town from Rath Yatra Route (in km)", 
       y = "Urban Population") +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.title = element_text(size =14),
        legend.text = element_text(size = 12)) +
  scale_color_manual(name = "Years",
                     values = colors) 
png(str_c(paths[['a']], "graph_pop_yatra.png"))
print(pop_yatra)
dev.off()  
