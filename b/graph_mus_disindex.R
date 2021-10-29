#------------------------------------------------------------------------------
#               Distribution of Control Variables (Data Section)
#------------------------------------------------------------------------------

rm(list=ls())

library(dplyr)
library(tidyverse)
library(haven)
library(matlib)
library(devtools)
library(fastDummies)
library(AER)
library(fixest)
library(ggplot2)

#functions and pathnames
source("C:/Users/aarus/Google Drive/communal_violence/b/aarushi_paths.R")
source("C:/Users/aarus/Google Drive/communal_violence/b/functions.R")
source_gist(4676064)

#load dataset
load(paths[['sr_data']])

#-----------------------------------------------------------------------------------
#           Figure 5: Distribution of Muslim Enterprises and Dissimilaity Index
#-----------------------------------------------------------------------------------

# Keep Yatra states 
data_dist <- sr_data %>%
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
         # Relationship should not hold for UP
         state_name != "uttar pradesh")

seg_dist <- data_dist %>%
  distinct(dis_index,
           dist_id, 
           muslim_prop) %>%
  ggplot() +
  geom_histogram(aes(x = dis_index,
                     fill = "Dissimilarity Index"),
                 color = "darkgreen",
                 alpha = 0.7,
                 binwidth = 2) +
  geom_histogram(aes(x = muslim_prop,
                     fill = "Proportion of Muslim Enterprises"),
                 color = "darkblue",
                 alpha = 0.7,
                 binwidth = 2) +
  labs(x = "Proportion of Muslim Enterprises \
                      Dissimilarity Index", 
       y = "Count") +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size =12),
        legend.title = element_text(size =14)) +
  scale_fill_manual(name = "Key",
                    values = c("Proportion of Muslim Enterprises" = "lightblue",
                               "Dissimilarity Index" = "olivedrab3")) 
png(str_c(paths[['a']], "graph_seg_dist.png"))
print(seg_dist)
dev.off()
