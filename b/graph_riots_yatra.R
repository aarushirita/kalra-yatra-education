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
library(modelr)

#functions and pathnames
source("C:/Users/aarus/Google Drive/communal_violence/b/aarushi_paths.R")
source("C:/Users/aarus/Google Drive/communal_violence/b/functions.R")
source_gist(4676064)

#load dataset
load(paths[['sr_data']])

#-----------------------------------------------------------------------------------
#              Figure 2: Distribution of Riots by Yatra Distance
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
         #state_name != "karnataka",
         # Relationship should not hold for UP
         state_name != "uttar pradesh") 

#residualize riots wrt population

riots_resid <- lm(count_riots_90 ~ pc91_pca_tot_p_u,
                  data = data_dist)

data_dist <- add_residuals(data = data_dist,
                           model = riots_resid,
                           var = "resid")

riots_yatra <- data_dist %>%
  filter(yatra_distance <= 150) %>%
  ggplot(aes(yatra_distance, 
             resid, 
             color = state_name), 
         weight = pc91_pca_tot_p_u) +
  xlab("Distance of town from Rath Yatra Route (in km)") +
  ylab("Number of Riots") +
  geom_point(aes(size=pc11_pca_tot_p_u),
             alpha = 0.5) +
  labs(color="State",
       size="Urban Population") +
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.title = element_text(size =14),
        legend.text = element_text(size = 12)) +
  scale_y_continuous(limit = c(0, 18))
png(str_c(paths[['a']], "graph_riots_yatra.png"))
print(riots_yatra)
dev.off()