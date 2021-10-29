#----------------------------------------------------------------------------------
#                             Graph Robustness checks
#---------------------------------------------------------------------------------

rm(list=ls())

library(dplyr)
library(tidyverse)
library(haven)
library(matlib)
library(devtools)
library(fastDummies)
library(AER)
library(fixest)

#functions and pathnames
source("C:/Users/aarus/Google Drive/communal_violence/b/aarushi_paths.R")
source("C:/Users/aarus/Google Drive/communal_violence/b/functions.R")
source_gist(4676064)

#load dataset
load(paths[['sr_data']])
load(paths[['clean_robustness_check']])

#--------------------------------------------------------------------------------
#                        SC Segregation, by Yatra route
#-------------------------------------------------------------------------------

data_sc_seg <- sr_data %>%
               left_join(clean_robust_check,
                         by = "dist_id") %>%
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
                      state_name != "uttar pradesh") %>%
               distinct(yatra_distance,
                        sc_dis_index,
                        pc11_pca_tot_p_u) %>%
               mutate(sc_dis_index = sc_dis_index * 100) %>%
               filter(yatra_distance <= 250)

#-----------------------------------------------------------------------------------------
#             Figure 13: Correlation between Caste Segregation and Yatra
#-----------------------------------------------------------------------------------------

sc_disindex_yatra <- data_sc_seg %>%
                     ggplot(aes(x = yatra_distance, 
                                y = sc_dis_index,
                            weight=pc11_pca_tot_p_u,
                            size=pc11_pca_tot_p_u)) +
                     xlab("Distance of town from Rath Yatra Route (in km)") +
                     ylab("Dissimilarity Index") +
                     geom_smooth(method = 'lm',
                                 formula = y ~ x,
                                 se = T,
                                 color='maroon4') +
                    stat_summary_bin(aes(size = pc11_pca_tot_p_u),
                                     fun = 'weighted.mean',
                                     bins = 40,
                                     color='maroon4',
                                     geom = 'point',
                                     alpha=0.6)     +
                    theme(axis.title.y = element_text(size = 14),
                          axis.title.x = element_text(size = 14))
png(str_c(paths[['a']], "graph_sc_disindex.png"))
print(sc_disindex_yatra)
dev.off()

#-------------------------------------------------------------------------
#         Figure A.15: Analyse SC Education with Yatra continuous
#------------------------------------------------------------------------

edu_sc <- sr_data %>%
          left_join(clean_robust_check,
                    by = "dist_id") %>%
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
         #do not use uttar pradesh for balance tables
                 state_name != "uttar pradesh",
                 state_name != "karnataka",
                 round != "nss62",
                 cohort != 6,
                 cohort != 0,
                 cohort != 1) %>%
          mutate(cohort = as.factor(cohort),
                 sc = if_else(social_group == "2", 1, 0)) 

edu_sc <- edu_sc %>%
          mutate(sc_dis_index = sc_dis_index * 100,
                 sc = as.factor(sc)) %>%
          filter(yatra_distance <= 250)

edu_sc <- edu_sc %>%
  mutate(cohortn1 = if_else(birth_year <= 1975, 1, 0),
         cohortn2 = if_else(birth_year >= 1976 & birth_year <= 1983, 2, 0),
         cohortn3 = if_else(birth_year >= 1984 & birth_year <= 1991, 3, 0),
         cohortn = cohortn1 + 
           cohortn2 + 
           cohortn3) %>%
  select(-c("cohortn1", "cohortn2", "cohortn3")) %>%
  filter(cohortn != 0)

edu_sc <- edu_sc %>%
  #filter(round != "nss62",
  #      cohort != 6,
  #      cohort != 1) %>%
  mutate(cohortn = as.factor(cohortn),
         muslim = as.factor(muslim))

#inverse weighting by population of each caste group in each cohort in district
edu_sc <- edu_sc %>%
          group_by(cohortn, 
                   sc,
                   dist_id) %>%
          mutate(pop = n()) %>%
          ungroup()

levels(edu_sc$cohortn) = c("Born Before 1976",
                            "Born 1976-83",
                            "Born 1984-90")

pri_yatra_sc         <- ggplot(data = edu_sc,
                                aes(x = yatra_distance,
                                    y = pri,
                                    colour = sc,
                                    group = sc,
                                    weight = (1 / pop))) +
                        geom_smooth(method = 'lm', 
                                    formula = y ~ x,
                                    se = F) +
                        stat_summary_bin(fun = 'mean',
                                         binwidth = 10,
                                         geom = 'point',
                                         alpha = 0.6) +
                        facet_wrap(~cohortn)  +
                        scale_colour_discrete(name = 'Social Groups',
                                              breaks = c(0, 1),
                                              labels = c('Other Castes',
                                                         'Scheduled Castes')) +
                        labs(x = "Distance of town from Rath Yatra Route (in km)", 
                             y = "Probability of attaining Primary Education") +
                        theme(axis.title.y = element_text(size = 14),
                              axis.title.x = element_text(size = 14),
                              legend.title = element_text(size =14),
                              legend.text = element_text(size = 12))
png(str_c(paths[['a']], "graph_li_pri_sc.png"))
print(pri_yatra_sc)
dev.off()

pri_yatra_sc         <- ggplot(data = edu_sc,
                               aes(x = yatra_distance,
                                   y = pri,
                                   colour = sc,
                                   group = sc,
                                   weight = (1 / pop))) +
  geom_smooth(method = 'loess', 
              formula = y ~ x,
              se = F) +
  stat_summary_bin(fun = 'mean',
                   binwidth = 10,
                   geom = 'point',
                   alpha = 0.6) +
  facet_wrap(~cohortn)  +
  scale_colour_discrete(name = 'Social Groups',
                        breaks = c(0, 1),
                        labels = c('Other Castes',
                                   'Scheduled Castes')) +
  labs(x = "Distance of town from Rath Yatra Route (in km)", 
       y = "Probability of attaining Primary Education") +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.title = element_text(size =14),
        legend.text = element_text(size = 12))
png(str_c(paths[['a']], "graph_lo_pri_sc.png"))
print(pri_yatra_sc)
dev.off()

sec_yatra_sc         <- ggplot(data = edu_sc,
                               aes(x = yatra_distance,
                                   y = sec,
                                   colour = sc,
                                   group = sc,
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
                                              labels = c('Other Castes',
                                                         'Scheduled Castes')) +
                        labs(x = "Distance of town from Rath Yatra Route (in km)", 
                             y = "Probability of attaining Secondary Education") +
                        theme(axis.title.y = element_text(size = 14),
                              axis.title.x = element_text(size = 14),
                              legend.title = element_text(size =14),
                              legend.text = element_text(size = 12))
png(str_c(paths[['a']], "graph_li_sec_sc.png"))
print(sec_yatra_sc)
dev.off()

col_yatra_sc         <- ggplot(data = edu_sc,
                               aes(x = yatra_distance,
                                   y = coll,
                                   colour = sc,
                                   group = sc,
                                   weight = (1 / pop))) +
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
                                              labels = c('Other Castes',
                                                         'Scheduled Castes')) +
                        labs(x = "Distance of town from Rath Yatra Route (in km)", 
                             y = "Probability of attaining College Education") +
                        theme(axis.title.y = element_text(size = 14),
                              axis.title.x = element_text(size = 14),
                              legend.title = element_text(size =14),
                              legend.text = element_text(size = 12))
png(str_c(paths[['a']], "graph_li_col_sc.png"))
print(col_yatra_sc)
dev.off()

#save data for regressions in stata
write_dta(edu_sc, paths[['sr_sc']])

#-------------------------------------------------------------------------
#             Accounting for Migration
#------------------------------------------------------------------------

data_mig <- sr_data %>%
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
         #consider yatra distance less than 250 km
                   yatra_distance <= 250) %>%
            filter(cohort != 0) 

data_mig_round <- data_mig %>%
                  group_by(dist_id,
                           muslim,
                           round) %>%
                  summarise(mus_pop = n()) %>%
                  filter(muslim == 1) %>%
                  spread(key = round,
                         value = mus_pop) %>%
                  mutate(mus_pop_diff = (nss68 - nss43), 
                         na.rm = T) %>%
                  ungroup() %>%
                  select(c("dist_id",
                           "mus_pop_diff")) 

data_mig <- data_mig %>%
            left_join(data_mig_round,
                      by = "dist_id")

#save data for regressions in stata
write_dta(data_mig, paths[['sr_mig']])

#-------------------------------------------------------------------------
#          Figure 20: Plot coefficients controlling for Migration
#------------------------------------------------------------------------

#load regression results

ill <-  t(read.csv(str_c(paths[['a']], 'table_re_edu_level_0_mig.csv'))) %>%
  as.data.frame() %>%
  select(c("V6", "V7")) %>%
  rename("distance_muslim" = "V6",
         "se" = "V7") %>%
  filter(se != "=") %>%
  mutate(cohort = c(1, 2, 3, 4, 5, 6, 7, 8)) %>%
  lapply(gsub, 
         pattern = "=\\(", 
         replacement = "") %>%
  lapply(gsub, 
         pattern = "\\)", 
         replacement = "")  %>%
  lapply(gsub, 
         pattern = "=", 
         replacement = "") %>%
  sapply(as.numeric)

pri <-  t(read.csv(str_c(paths[['a']], 'table_re_pri_mig.csv'))) %>%
  as.data.frame() %>%
  select(c("V6", "V7")) %>%
  rename("distance_muslim" = "V6",
         "se" = "V7") %>%
  filter(se != "=") %>%
  mutate(cohort = c(1, 2, 3, 4, 5, 6, 7, 8)) %>%
  lapply(gsub, 
         pattern = "=\\(", 
         replacement = "") %>%
  lapply(gsub, 
         pattern = "\\)", 
         replacement = "")  %>%
  lapply(gsub, 
         pattern = "=", 
         replacement = "") %>%
  sapply(as.numeric) 

sec <-  t(read.csv(str_c(paths[['a']], 'table_re_sec_mig.csv'))) %>%
  as.data.frame() %>%
  select(c("V6", "V7")) %>%
  rename("distance_muslim" = "V6",
         "se" = "V7") %>%
  filter(se != "=") %>%
  mutate(cohort = c(1, 2, 3, 4, 5, 6, 7, 8)) %>%
  lapply(gsub, 
         pattern = "=\\(", 
         replacement = "") %>%
  lapply(gsub, 
         pattern = "\\)", 
         replacement = "")  %>%
  lapply(gsub, 
         pattern = "=", 
         replacement = "") %>%
  sapply(as.numeric) 

coll <- t(read.csv(str_c(paths[['a']], 'table_re_coll_mig.csv'))) %>%
  as.data.frame() %>%
  select(c("V6", "V7")) %>%
  rename("distance_muslim" = "V6",
         "se" = "V7") %>%
  filter(se != "=") %>%
  mutate(cohort = c(1, 2, 3, 4, 5, 6, 7, 8)) %>%
  lapply(gsub, 
         pattern = "=\\(", 
         replacement = "") %>%
  lapply(gsub, 
         pattern = "\\)", 
         replacement = "")  %>%
  lapply(gsub, 
         pattern = "=", 
         replacement = "") %>%
  sapply(as.numeric) 


ill_mig <- data.frame(matrix(unlist(ill), 
                             nrow = 8)) %>%
  mutate(cohort = as.factor(X3)) %>%
  ggplot() +
  geom_point(mapping = aes(x = cohort, 
                           y = X1), 
             color = "red") +
  geom_errorbar(mapping = aes(x = cohort, 
                              ymin = X1 - 1.96 * X2,
                              ymax = X1 + 1.96 * X2),
                width = 0.3) +
  geom_abline(aes(intercept = 0, 
                  slope = 0), 
              color = "grey") +
  scale_x_discrete(name = "Cohorts",
                   labels = c('1976-77', 
                              '1978-79', 
                              '1980-81', 
                              '1982-83', 
                              '1984-85', 
                              '1986-87', 
                              '1988-89', 
                              '1990-91')) +
  scale_y_continuous(name = "Coefficients") +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(angle = 45))  
png(str_c(paths[['a']], "graph_coeff_ill_mig.png"))
print(ill_mig)
dev.off()

pri_mig <- data.frame(matrix(unlist(pri), 
                             nrow = 8)) %>%
  mutate(cohort = as.factor(X3)) %>%
  ggplot() +
  geom_point(mapping = aes(x = cohort, 
                           y = X1), 
             color = "red") +
  geom_errorbar(mapping = aes(x = cohort, 
                              ymin = X1 - 1.96 * X2,
                              ymax = X1 + 1.96 * X2),
                width = 0.3) +
  geom_abline(aes(intercept = 0, 
                  slope = 0), 
              color = "grey") +
  scale_x_discrete(name = "Cohorts",
                   labels = c('1976-77', 
                              '1978-79', 
                              '1980-81', 
                              '1982-83', 
                              '1984-85', 
                              '1986-87', 
                              '1988-89', 
                              '1990-91')) +
  scale_y_continuous(name = "Coefficients") +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(angle = 45)) 
png(str_c(paths[['a']], "graph_coeff_pri_mig.png"))
print(pri_mig)
dev.off()

sec_mig <- data.frame(matrix(unlist(sec), 
                             nrow = 8)) %>%
  mutate(cohort = as.factor(X3)) %>%
  ggplot() +
  geom_point(mapping = aes(x = cohort, 
                           y = X1), 
             color = "red") +
  geom_errorbar(mapping = aes(x = cohort, 
                              ymin = X1 - 1.96 * X2,
                              ymax = X1 + 1.96 * X2),
                width = 0.3) +
  geom_abline(aes(intercept = 0, 
                  slope = 0), 
              color = "grey") +
  scale_x_discrete(name = "Cohorts",
                   labels = c('1976-77', 
                              '1978-79', 
                              '1980-81', 
                              '1982-83', 
                              '1984-85', 
                              '1986-87', 
                              '1988-89', 
                              '1990-91')) +
  scale_y_continuous(name = "Coefficients") +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(angle = 45))  
png(str_c(paths[['a']], "graph_coeff_sec_mig.png"))
print(sec_mig)
dev.off()

col_mig <- data.frame(matrix(unlist(coll), 
                             nrow = 8)) %>%
  mutate(cohort = as.factor(X3)) %>%
  ggplot() +
  geom_point(mapping = aes(x = cohort, 
                           y = X1), 
             color = "red") +
  geom_errorbar(mapping = aes(x = cohort, 
                              ymin = X1 - 1.96 * X2,
                              ymax = X1 + 1.96 * X2),
                width = 0.3) +
  geom_abline(aes(intercept = 0, 
                  slope = 0), 
              color = "grey") +
  scale_x_discrete(name = "Cohorts",
                   labels = c('1976-77', 
                              '1978-79', 
                              '1980-81', 
                              '1982-83', 
                              '1984-85', 
                              '1986-87', 
                              '1988-89', 
                              '1990-91')) +
  scale_y_continuous(name = "Coefficients") +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(angle = 45)) 
png(str_c(paths[['a']], "graph_coeff_col_mig.png"))
print(col_mig)
dev.off()

#-------------------------------------------------------------------------
#                      Plot Non-Yatra States
#------------------------------------------------------------------------

data_nonyatra <- sr_data %>%
  #pick yatra states
  filter(state_name == "jammu kashmir" ||
         state_name == "himachal pradesh" ||
         state_name == "punjab" ||
         state_name == "uttarakhand" ||
         state_name == "sikkim" ||
         state_name == "manipur" ||
         state_name == "tripura" ||
         state_name == "assam" ||
         state_name == "west bengal" ||
         state_name == "odisha" ||
         state_name == "goa" ||
         state_name == "kerala" ||
         state_name == "tamil nadu" ||
         state_name == "meghalaya" ||
         state_name == "mizoram" ||
         state_name == "arunachal pradesh" ||
         state_name == "nagaland" ||
         state_name == "dadra nagar haveli" ||
         state_name == "daman diu" ||
         state_name == "lakshadweep" ||
         state_name == "puducherry" ||
         state_name == "andaman nicobar islands" ||
         state_name == "chandigarh" ||
         state_name == "karnataka" ||
         #do not use uttar pradesh for balance tables
         state_name == "uttar pradesh") %>%
         #consider males in sample                 
         filter(gender == "1",
         #dropping STs from the sample
         social_group != "1",
         #droppping SCs from sample
         social_group != "2",
         #consider yatra distance between 250 to 500 to avoid spillovers
         yatra_distance >= 250,
         yatra_distance <= 500) 

disindex_nonyatra <- data_nonyatra %>%
                     distinct(dist_id,
                              yatra_distance,
                              dis_index,
                              pc11_pca_tot_p_u) %>%
                     ggplot(aes(x = yatra_distance, 
                                y = dis_index)) +
                                #weight = pc11_pca_tot_p_u)) +
                     xlab("Distance of town from Rath Yatra Route (in km)") +
                     ylab("Dissimilarity Index") +
                     geom_smooth(method = 'lm',
                                 formula = y ~ x,
                                 se = T,
                                 color='turquoise4') +
                    stat_summary_bin(fun = 'mean',
                                     bins = 40,
                                     color='turquoise4',
                                     geom = 'point',
                                     alpha=0.6)     +
                    theme(axis.title.y = element_text(size = 14),
                          axis.title.x = element_text(size = 14))
png(str_c(paths[['a']], "graph_nonyatra_disindex.png"))
print(disindex_nonyatra)
dev.off()