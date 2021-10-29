#-----------------------------------------------------------------------------------------------------------------------------------------
#                                       Summary Statistics Graphs
#----------------------------------------------------------------------------------------------------------------------------------------

rm(list=ls())

library(dplyr)
library(tidyverse)
library(foreign)
library(matlib)
library(devtools)

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
         social_group != "2",
         edu_level < 8) %>%
    filter(cohort != 0,
         cohort != 6,
         yatra_distance <= 250)

#------------------------------------------------------------------------
#         Figure 4: Distribution of Years of Education, by cohort (male)
#------------------------------------------------------------------------

#Muslims
muslim_cohort <- data_sumstat %>%
                 mutate(cohort = as.factor(cohort)) %>%
                 filter(muslim == 1) %>%
                 group_by(cohort, 
                          edu_level) %>%
                 summarise(n = n()) %>%
                 filter(is.na(edu_level) == F) %>%
                 ungroup()

#Cohorts
levels(muslim_cohort$cohort) = c("Born Before 1950",
                                 "Born 1951-60",
                                 "Born 1961-70",
                                 "Born 1971-80",
                                 "Born 1981-90")

hist_mus <-      muslim_cohort %>%
                 group_by(cohort) %>%
                 ggplot(aes(x = edu_level, 
                            y = n,
                            fill = cohort,
                            color = cohort)) +
                 geom_bar(position = 'fill',
                          stat = 'identity',
                          alpha = 0.7) + 
                 xlab("Years of Schooling, Muslim") +
                 theme(axis.title.x = element_text(size=14),
                       axis.text.x = element_text(angle = 45, size = 14),
                       legend.title = element_text(size =14),
                       legend.text = element_text(size = 12)) + 
                 labs(color = "Cohort",
                      fill = "Cohort")  +
  scale_x_discrete(breaks = c(0, 
                              1, 
                              2, 
                              3, 
                              4, 
                              5, 
                              6, 
                              7),
                   labels = c('Illeterate', 
                              'Literate', 
                              'Below Primary', 
                              'Primary', 
                              'Middle', 
                              'Secondary', 
                              'Higher Secondary', 
                              'Diploma')) 
png(str_c(paths[['a']], "graph_edu_mus.png"))
print(hist_mus)
dev.off()

#Non-Muslims
nonmuslim_cohort <- data_sumstat %>%
                    mutate(cohort = as.factor(cohort)) %>%
                    filter(muslim == 0) %>%
                    group_by(cohort, 
                             edu_level) %>%
                    summarise(n = n()) %>%
                    filter(is.na(edu_level) == F) %>%
                    ungroup()

#Cohorts
levels(nonmuslim_cohort$cohort) = c("Born Before 1950",
                                    "Born 1951-60",
                                    "Born 1961-70",
                                    "Born 1971-80",
                                    "Born 1981-90")

hist_nonmus <- nonmuslim_cohort %>%
               group_by(cohort) %>%
               ggplot(aes(x = edu_level, 
                          y = n,
                          fill = cohort,
                          color = cohort)) +
               geom_bar(position = 'fill',
                        stat = 'identity',
                        alpha = 0.7) + 
               xlab("Years of Schooling, Non-Muslim") +
               theme(axis.title.x = element_text(size = 14),
                     axis.text.x = element_text(angle = 45, size = 14),
                     legend.title = element_text(size =14),
                     legend.text = element_text(size = 12)) + 
               labs(color = "Cohort",
                    fill = "Cohort")  +
               scale_x_discrete(breaks = c(0, 
                                           1, 
                                           2, 
                                           3, 
                                           4, 
                                           5, 
                                           6, 
                                           7),
                                labels = c('Illeterate', 
                                           'Literate', 
                                           'Below Primary', 
                                           'Primary', 
                                           'Middle', 
                                           'Secondary', 
                                           'Higher Secondary', 
                                           'Diploma'))
png(str_c(paths[['a']], "graph_edu_nonmus.png"))
print(hist_nonmus)
dev.off()