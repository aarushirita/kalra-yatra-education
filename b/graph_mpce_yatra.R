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

#-----------------------------------------------------------------------------------------
#               Figure 8: Balance of real MOCE for Muslim Households by Distance
#-----------------------------------------------------------------------------------------

mpce               <- data_balance_g %>%
  filter(round != "nss62",
         round != "nss66") %>%
  mutate(round = as.factor(round))

levels(mpce$round) <- c("1987-88", 
                        "1999-00", 
                        "2007-08", 
                        "2011-12")

mpce_yatra_muslim <- ggplot(data = mpce,
                            aes(x = yatra_distance,
                                y = mpce_real * muslim,
                                colour = round,
                                group = round,
                                weight = pc11_pca_tot_p_u,
                                size = pc11_pca_tot_p_u)) +
  geom_smooth(method = 'loess', 
              formula = y ~ x,
              se = F) +
  stat_summary_bin(fun = 'mean',
                   bins=40,
                   geom = 'point',
                   alpha=0.6) +
  labs(x = "Distance of town from Rath Yatra Route (in km)", 
       y = "Average MPCE (in 1983 Rupees) for Muslim households") +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.title = element_text(size =14),
        legend.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0, 75))
png(str_c(paths[['a']], "graph_mpce_yatra_muslim.png"))
print(mpce_yatra_muslim)
dev.off()
