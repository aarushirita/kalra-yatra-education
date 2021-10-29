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

data_balance_g <- data_balance_g %>%
  mutate(cohortn1 = if_else(birth_year <= 1975, 1, 0),
         cohortn2 = if_else(birth_year >= 1976 & birth_year <= 1983, 2, 0),
         cohortn3 = if_else(birth_year >= 1984 & birth_year <= 1991, 3, 0),
         cohortn = cohortn1 + 
           cohortn2 + 
           cohortn3) %>%
  select(-c("cohortn1", "cohortn2", "cohortn3")) %>%
  filter(cohortn != 0)

#-------------------------------------------------------------------------
#         Figure A1: Primary Education as a function of Yatra
#------------------------------------------------------------------------

edu_mus <- data_balance_g %>%
           #filter(round != "nss62",
          #      cohort != 6,
            #      cohort != 1) %>%
           mutate(cohortn = as.factor(cohortn),
                  muslim = as.factor(muslim))

#inverse weighting by population of each religion in each cohort in district
edu_mus <- edu_mus %>%
           group_by(cohortn, 
                    muslim,
                    dist_id) %>%
           mutate(pop = n()) %>%
           ungroup()

levels(edu_mus$cohortn) = c("Born Before 1976",
                           "Born 1976-83",
                           "Born 1984-90")

pri_yatra_mus         <- ggplot(data = edu_mus,
                                aes(x = yatra_distance,
                                    y = pri,
                                    colour = muslim,
                                    group = muslim,
                                    weight = (1 / pop))) +
                         geom_smooth(method = 'lm', 
                                     formula = y ~ x,
                                     se = F) +
                         stat_summary_bin(fun = 'mean',
                                          binwidth = 10,
                                          geom = 'point',
                                          alpha=0.6) +
                         facet_wrap(~cohortn)  +
                         scale_colour_discrete(name = 'Social Groups',
                         breaks = c(0, 1),
                         labels = c('Non-Muslim',
                                   'Muslim')) +
                         labs(x = "Distance of town from Rath Yatra Route (in km)", 
                              y = "Probability of attaining Primary Education") +
                         theme(axis.title.y = element_text(size = 14),
                               axis.title.x = element_text(size = 14),
                               legend.title = element_text(size =14),
                               legend.text = element_text(size = 12))
png(str_c(paths[['a']], "graph_li_pri_mus.png"))
print(pri_yatra_mus)
dev.off()  

#loess based estimators
pri_yatra_mus         <- ggplot(data = edu_mus,
                                aes(x = yatra_distance,
                                    y = pri,
                                    colour = muslim,
                                    group = muslim,
                                    weight = (1 / pop))) +
  geom_smooth(method = 'loess', 
              formula = y ~ x,
              se = F) +
  stat_summary_bin(fun = 'mean',
                   binwidth = 10,
                   geom = 'point',
                   alpha=0.6) +
  facet_wrap(~cohortn)  +
  scale_colour_discrete(name = 'Social Groups',
                        breaks = c(0, 1),
                        labels = c('Non-Muslim',
                                   'Muslim')) +
  labs(x = "Distance of town from Rath Yatra Route (in km)", 
       y = "Probability of attaining Primary Education") +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.title = element_text(size =14),
        legend.text = element_text(size = 12))
png(str_c(paths[['a']], "graph_lo_pri_mus.png"))
print(pri_yatra_mus)
dev.off() 

#yearly reduced form estimators
edu_mus <- data_balance_g %>%
  filter(round != "nss62",
         birth_year >= 1976,
         birth_year <= 1991) %>%
  mutate(cohort = as.factor(birth_year),
         muslim = as.factor(muslim))

#inverse weighting by population of each religion in each cohort in district
edu_mus <- edu_mus %>%
  group_by(birth_year, 
           muslim,
           dist_id) %>%
  mutate(pop = n()) %>%
  ungroup()

levels(edu_mus$cohort) = c("Born 1951-60",
                           "Born 1961-70",
                           "Born 1971-80",
                           "Born 1981-90")

priy_yatra_mus         <- ggplot(data = edu_mus,
                                aes(x = yatra_distance,
                                    y = pri,
                                    colour = muslim,
                                    group = muslim,
                                    weight = (1 / pop))) +
  geom_smooth(method = 'lm', 
              formula = y ~ x,
              se = F) +
  stat_summary_bin(fun = 'mean',
                   binwidth = 10,
                   geom = 'point',
                   alpha=0.6) +
  facet_wrap(~birth_year)  +
  scale_colour_discrete(name = 'Social Groups',
                        breaks = c(0, 1),
                        labels = c('Non-Muslim',
                                   'Muslim')) +
  labs(x = "Distance of town from Rath Yatra Route (in km)", 
       y = "Probability of attaining Primary Education") +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.title = element_text(size =14),
        legend.text = element_text(size = 12))
png(str_c(paths[['a']], "graph_li_priy_mus.png"))
print(priy_yatra_mus)
dev.off()  

#-------------------------------------------------------------------------
#         Figure A.1: Secondary Education as a function of Yatra
#------------------------------------------------------------------------

sec_yatra_mus         <- ggplot(data = edu_mus,
                                aes(x = yatra_distance,
                                    y = sec,
                                    colour = muslim,
                                    group = muslim,
                                    weight = (1 / pop))) +
                         geom_smooth(method = 'lm', 
                                     formula = y ~ x,
                                     se = F) +
                         stat_summary_bin(fun = 'mean',
                                          binwidth = 10,
                                          geom = 'point',
                                          alpha=0.6) +
                         facet_wrap(~cohort)  +
                         scale_colour_discrete(name = 'Social Groups',
                                               breaks = c(0, 1),
                                               labels = c('Non-Muslim',
                                                          'Muslim')) +
                         labs(x = "Distance of town from Rath Yatra Route (in km)", 
                              y = "Probability of attaining Secondary Education") +
                         theme(axis.title.y = element_text(size = 14),
                               axis.title.x = element_text(size = 14),
                               legend.title = element_text(size =14),
                               legend.text = element_text(size = 12))
png(str_c(paths[['a']], "graph_li_sec_mus.png"))
print(sec_yatra_mus)
dev.off() 

#loess based estimator
sec_yatra_mus         <- ggplot(data = edu_mus,
                                aes(x = yatra_distance,
                                    y = sec,
                                    colour = muslim,
                                    group = muslim,
                                    weight = (1 / pop))) +
  geom_smooth(method = 'loess', 
              formula = y ~ x,
              se = F) +
  stat_summary_bin(fun = 'mean',
                   binwidth = 10,
                   geom = 'point',
                   alpha=0.6) +
  facet_wrap(~cohort)  +
  scale_colour_discrete(name = 'Social Groups',
                        breaks = c(0, 1),
                        labels = c('Non-Muslim',
                                   'Muslim')) +
  labs(x = "Distance of town from Rath Yatra Route (in km)", 
       y = "Probability of attaining Secondary Education") +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.title = element_text(size =14),
        legend.text = element_text(size = 12))
png(str_c(paths[['a']], "graph_lo_sec_mus.png"))
print(sec_yatra_mus)
dev.off()  

#-------------------------------------------------------------------------
#         Figure A.2: Secondary Education as a function of Yatra
#------------------------------------------------------------------------

col_yatra_mus         <- ggplot(data = edu_mus,
                                aes(x = yatra_distance,
                                    y = coll,
                                    colour = muslim,
                                    group = muslim,
                                    weight = (1/ pop))) +
                         geom_smooth(method = 'lm', 
                                     formula = y ~ x,
                                     se = F) +
                         stat_summary_bin(fun = 'mean',
                                          binwidth = 10,
                                          geom = 'point',
                                          alpha = 0.6) +
                         facet_wrap(~cohort)  +
                         scale_colour_discrete(name = 'Social Groups',
                                               breaks = c(0, 1),
                                               labels = c('Non-Muslim',
                                                          'Muslim')) +
                         labs(x = "Distance of town from Rath Yatra Route (in km)", 
                              y = "Probability of attaining College Education") +
                         theme(axis.title.y = element_text(size = 14),
                               axis.title.x = element_text(size = 14),
                               legend.title = element_text(size =14),
                               legend.text = element_text(size = 12))
png(str_c(paths[['a']], "graph_li_col_mus.png"))
print(col_yatra_mus)
dev.off() 

#loess based estimator
col_yatra_mus         <- ggplot(data = edu_mus,
                                aes(x = yatra_distance,
                                    y = coll,
                                    colour = muslim,
                                    group = muslim,
                                    weight = (1/ pop))) +
  geom_smooth(method = 'loess', 
              formula = y ~ x,
              se = F) +
  stat_summary_bin(fun = 'mean',
                   binwidth = 10,
                   geom = 'point',
                   alpha = 0.6) +
  facet_wrap(~cohort)  +
  scale_colour_discrete(name = 'Social Groups',
                        breaks = c(0, 1),
                        labels = c('Non-Muslim',
                                   'Muslim')) +
  labs(x = "Distance of town from Rath Yatra Route (in km)", 
       y = "Probability of attaining College Education") +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.title = element_text(size =14),
        legend.text = element_text(size = 12))
png(str_c(paths[['a']], "graph_lo_col_mus.png"))
print(col_yatra_mus)
dev.off() 