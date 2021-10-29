#----------------------------------------------------------------------------------
#                   Graph IV Coefficients (with Random Effects)
#---------------------------------------------------------------------------------

rm(list=ls())

library(dplyr)
library(tidyverse)
library(foreign)
library(matlib)
library(devtools)
library(fastDummies)
library(AER)
library(fixest)

#functions and pathnames
source("C:/Users/aarus/Google Drive/communal_violence/b/aarushi_paths.R")
source("C:/Users/aarus/Google Drive/communal_violence/b/functions.R")
source_gist(4676064)

#-------------------------------------------------------------------------
#          Plot coefficients from Random Effects Regressions
#------------------------------------------------------------------------

#load regression results

ill <-  t(read.csv(str_c(paths[['a']], 'table_re_iv_edu_level_0.csv'))) %>%
        as.data.frame() %>%
        select(c("V2", "V3")) %>%
        rename("distance_muslim" = "V2",
               "se" = "V3") %>%
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

pri <-  t(read.csv(str_c(paths[['a']], 'table_re_iv_pri.csv'))) %>%
        as.data.frame() %>%
        select(c("V2", "V3")) %>%
        rename("distance_muslim" = "V2",
               "se" = "V3") %>%
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

sec <-  t(read.csv(str_c(paths[['a']], 'table_re_iv_sec.csv'))) %>%
        as.data.frame() %>%
        select(c("V2", "V3")) %>%
        rename("distance_muslim" = "V2",
               "se" = "V3") %>%
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

coll <- t(read.csv(str_c(paths[['a']], 'table_re_iv_coll.csv'))) %>%
        as.data.frame() %>%
        select(c("V2", "V3")) %>%
        rename("distance_muslim" = "V2",
               "se" = "V3") %>%
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


ill_re_iv <- data.frame(matrix(unlist(ill), 
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
png(str_c(paths[['a']], "graph_coeff_ill_iv.png"))
print(ill_re_iv)
dev.off()

pri_re_iv <- data.frame(matrix(unlist(pri), 
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
png(str_c(paths[['a']], "graph_coeff_pri_iv.png"))
print(pri_re_iv)
dev.off()

sec_re_iv <- data.frame(matrix(unlist(sec), 
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
png(str_c(paths[['a']], "graph_coeff_sec_iv.png"))
print(sec_re_iv)
dev.off()

col_re_iv <- data.frame(matrix(unlist(coll), 
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
png(str_c(paths[['a']], "graph_coeff_col_iv.png"))
print(col_re_iv)
dev.off()