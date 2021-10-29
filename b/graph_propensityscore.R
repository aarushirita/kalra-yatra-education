#------------------------------------------------------------------------------
#          Propensity Score Matching with Continuous Treatment
#------------------------------------------------------------------------------

rm(list=ls())

library(dplyr)
#library(plyr)
library(tidyverse)
library(haven)
library(matlib)
library(devtools)
library(AER)
library(fixest)
library(gtsummary)
library(segmented)


#functions and pathnames
source("C:/Users/aarus/Google Drive/communal_violence/b/aarushi_paths.R")
source("C:/Users/aarus/Google Drive/communal_violence/b/functions.R")
source_gist(4676064)

#load dataset
load(paths[['sr_data']])

#-----------------------------------------------------------------------------------
#                   filter relevant observations
#-----------------------------------------------------------------------------------
data_balance_p <- sr_data %>%
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
                         social_group != "2"
         #consider all the variation to construct GPS
                         ) 

#--------------------------------------------------------------------------
#                 Propensity Scores (Hirano and Imbens)
#--------------------------------------------------------------------------

pr_num <- lm(yatra_distance ~ 1, data = data_balance_p)

num <- dnorm(x = data_balance_p$yatra_distance,
             mean = fitted.values(pr_num),
             sd = summary(pr_num)$sigma)

pr_den <- lm(yatra_distance ~
               pc91_pca_tot_p_u,
             data = data_balance_p)

spline_den <- segmented(pr_den,
                        npsi = 1)

den <- dnorm(x = data_balance_p$yatra_distance,
             mean = fitted.values(spline_den),
             sd = summary(spline_den)$sigma)


data_balance_p <- data_balance_p %>%
                  mutate(ipw_s = num / den)

#--------------------------------------------------------------------------
#                Figure 9: Stabilized Inverse Probability Weights
#--------------------------------------------------------------------------

#Distribution of Stablized IPWs
ipw_dist <- data_balance_p %>%
              filter(yatra_distance <= 250) %>%
              group_by(dist_id,
                       yatra_distance) %>%
              summarise(ipw_s = mean(ipw_s)) %>%
              ggplot(aes(x = ipw_s)) +
              geom_histogram(color = "darkblue",
                             fill = "lightblue",
                             binwidth = 0.0125) +
              labs(x = "Estimated Stablized IPW", 
                   y = "Count") +
              theme(axis.title.y = element_text(size = 14),
                    axis.title.x = element_text(size = 14))
png(str_c(paths[['a']], "graph_ipw_dist.png"))
print(ipw_dist)
dev.off()

data_balance_p <- data_balance_p  %>%
                  filter(yatra_distance <= 250) 

#save data for regressions in stata
write_dta(data_balance_p, paths[['sr_score']])

#--------------------------------------------------------------------------
#                Figure 10: IPWs by Distance from Yatra
#--------------------------------------------------------------------------
ipw_yatra <- data_balance_p %>%
             ggplot(aes(x     = yatra_distance,
                        y     = ipw_s,
                        color = state_name,
                        group = state_name)) +
             geom_point(alpha = 0.6) +
             labs(x = "Distance of town from Rath Yatra Route (in km)", 
                  y = "Estimated Stabilized IPW") +
             scale_color_discrete(name = "States") +
             theme(axis.title.y = element_text(size = 14),
                   axis.title.x = element_text(size = 14),
                   legend.title = element_text(size =14),
                   legend.text = element_text(size = 12)) +
             scale_y_continuous(limits = c(0, 4)) 
png(str_c(paths[['a']], "graph_ipw_yatra.png"))
print(ipw_yatra)
dev.off()