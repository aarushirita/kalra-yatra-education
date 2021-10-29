#------------------------------------------------------------------------------
#                     Graphs for Placebo Yatra
#------------------------------------------------------------------------------

rm(list=ls())

library(dplyr)
library(tidyverse)
library(haven)
library(matlib)
library(devtools)
library(AER)
library(fixest)

#functions and pathnames
source("C:/Users/aarus/Google Drive/communal_violence/b/aarushi_paths.R")
source("C:/Users/aarus/Google Drive/communal_violence/b/functions.R")
source_gist(4676064)

#load dataset
load(paths[['clean_placebo']])

#------------------------------------------------------------------------------
#                        Figure 11: Riots and Planned Yatra
#------------------------------------------------------------------------------

riots_yatra_p <- ggplot(data = data_up,
                        aes(x = p_yatra_distance,
                            y = count_riots_90,
                            group = state_name,
                            color = state_name)) + 
                 geom_point(aes(size = pc11_pca_tot_p_u),
                            alpha = 0.7)  +
                 scale_y_continuous(limits = c(0, 50)) +
                 geom_label(
                   data = data_up %>% filter(district_name == 'muzaffarpur' | district_name == 'patna'),
                   aes(label = district_name,
                       angle = 30),
                   color = "black",
                   fill = "#69b3a2") +
                 labs(x = "Distance of town from Planned Yatra Route (in km)", 
                      y = "Number of Riots") +
                 scale_color_discrete(name = "States") +
                 scale_size(name = "Urban Population") +
                 theme(axis.title.y = element_text(size = 14),
                       axis.title.x = element_text(size = 14),
                       legend.title = element_text(size =14),
                       legend.text = element_text(size = 12))
png(str_c(paths[['a']], "graph_riots_yatra_p.png"))
print(riots_yatra_p)
dev.off()

#-------------------------------------------------------------------------
#         Figure A.8: Analyse Education with Planned Yatra continuous
#------------------------------------------------------------------------

data_up <- data_up %>%
        mutate(cohortn1 = if_else(birth_year <= 1975, 1, 0),
               cohortn2 = if_else(birth_year >= 1976 & birth_year <= 1983, 2, 0),
               cohortn3 = if_else(birth_year >= 1984 & birth_year <= 1991, 3, 0),
               cohortn = cohortn1 + 
                       cohortn2 + 
                       cohortn3) %>%
        select(-c("cohortn1", "cohortn2", "cohortn3")) %>%
        filter(cohortn != 0)

edu_mus <- data_up %>%
        #filter(round != "nss62",
         #      cohort != 6,
          #     cohort != 1) %>%
        mutate(cohort = as.factor(cohortn),
               muslim = as.factor(muslim))

#inverse weighting by population of each religion in each cohort in district
edu_mus <- edu_mus %>%
        filter(state_name == "uttar pradesh") %>%
        group_by(cohortn, 
                 muslim,
                 dist_id) %>%
        mutate(pop = n()) %>%
        ungroup()

levels(edu_mus$cohortn) = c("Born Before 1976",
                            "Born 1976-83",
                            "Born 1984-90")

levels(edu_mus$cohort) = c("Born 1951-60",
                           "Born 1961-70",
                           "Born 1971-80",
                           "Born 1981-90")

pri_yatra_mus_p       <- ggplot(data = edu_mus,
                                aes(x = p_yatra_distance,
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
        facet_wrap(~cohort,)  +
        scale_colour_discrete(name = 'Social Groups',
                              breaks = c(0, 1),
                              labels = c('Non-Muslim',
                                         'Muslim')) +
        labs(x = "Distance of town from Planned Yatra Route (in km)", 
             y = "Probability of attaining Primary Education") +
        theme(axis.title.y = element_text(size = 14),
              axis.title.x = element_text(size = 14),
              legend.title = element_text(size =14),
              legend.text = element_text(size = 12))
png(str_c(paths[['a']], "graph_li_pri_mus_p.png"))
print(pri_yatra_mus_p)
dev.off() 

pri_yatra_mus_p       <- ggplot(data = edu_mus,
                                aes(x = p_yatra_distance,
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
        facet_wrap(~cohort,)  +
        scale_colour_discrete(name = 'Social Groups',
                              breaks = c(0, 1),
                              labels = c('Non-Muslim',
                                         'Muslim')) +
        labs(x = "Distance of town from Planned Yatra Route (in km)", 
             y = "Probability of attaining Primary Education") +
        theme(axis.title.y = element_text(size = 14),
              axis.title.x = element_text(size = 14),
              legend.title = element_text(size =14),
              legend.text = element_text(size = 12))
png(str_c(paths[['a']], "graph_lo_pri_mus_p.png"))
print(pri_yatra_mus_p)
dev.off()

sec_yatra_mus_p       <- ggplot(data = edu_mus,
                                aes(x = p_yatra_distance,
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
        labs(x = "Distance of town from Planned Yatra Route (in km)", 
             y = "Probability of attaining Secondary Education") +
        theme(axis.title.y = element_text(size = 14),
              axis.title.x = element_text(size = 14),
              legend.title = element_text(size =14),
              legend.text = element_text(size = 12))
png(str_c(paths[['a']], "graph_li_sec_mus_p.png"))
print(sec_yatra_mus_p)
dev.off()  

col_yatra_mus_p       <- ggplot(data = edu_mus,
                                aes(x = p_yatra_distance,
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
        labs(x = "Distance of town from Planned Yatra Route (in km)", 
             y = "Probability of attaining College Education") +
        theme(axis.title.y = element_text(size = 14),
              axis.title.x = element_text(size = 14),
              legend.title = element_text(size =14),
              legend.text = element_text(size = 12))
png(str_c(paths[['a']], "graph_li_col_mus_p.png"))
print(col_yatra_mus_p)
dev.off()  

#-------------------------------------------------------------------------------
#  Figures 12, A.8- A.10: Plot coefficients from Random Effects Regressions
#-------------------------------------------------------------------------------

#load regression results
ill <-  t(read.csv(str_c(paths[['a']], 'table_re_edu_level_0_p.csv'))) %>%
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

pri <-  t(read.csv(str_c(paths[['a']], 'table_re_pri_p.csv'))) %>%
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

sec <-  t(read.csv(str_c(paths[['a']], 'table_re_sec_p.csv'))) %>%
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

coll <- t(read.csv(str_c(paths[['a']], 'table_re_col_p.csv'))) %>%
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


ill_re_p <- data.frame(matrix(unlist(ill), 
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
png(str_c(paths[['a']], "graph_coeff_ill_p.png"))
print(ill_re_p)
dev.off()

pri_re_p <- data.frame(matrix(unlist(pri), 
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
png(str_c(paths[['a']], "graph_coeff_pri_p.png"))
print(pri_re_p)
dev.off()

sec_re_p <- data.frame(matrix(unlist(sec), 
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
png(str_c(paths[['a']], "graph_coeff_sec_p.png"))
print(sec_re_p)
dev.off()

col_re_p <- data.frame(matrix(unlist(coll), 
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
png(str_c(paths[['a']], "graph_coeff_col_p.png"))
print(col_re_p)
dev.off()

#----------------------------------------------------------------------------------
#                 Figure 17: Segregation by Placebo Yatra Distance
#---------------------------------------------------------------------------------

disindex_yatra_p <- data_up %>%
                    filter(state_name == "uttar pradesh") %>%
                    distinct(p_yatra_distance,
                    dis_index,
                    pc11_pca_tot_p_u) %>%
                    ggplot(aes(x = p_yatra_distance, 
                               y = dis_index,
                               weight = pc11_pca_tot_p_u,
                               size = pc11_pca_tot_p_u)) +
                    xlab("Distance of town from Planned Yatra Route (in km)") +
                    ylab("Dissimilarity Index") +
                    geom_smooth(method = 'lm',
                                formula = y ~ x,
                                se = T,
                                color = 'dark blue') +
                    stat_summary_bin(aes(size = pc11_pca_tot_p_u),
                                         fun = 'weighted.mean',
                                         bins = 40,
                                         color='dark blue',
                                         geom = 'point',
                                         alpha=0.6)     +
                    theme(axis.title.y = element_text(size = 14),
                          axis.title.x = element_text(size = 14)) 
png(str_c(paths[['a']], "graph_disindex_p.png"))
print(disindex_yatra_p)
dev.off()