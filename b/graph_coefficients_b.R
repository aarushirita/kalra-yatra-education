#----------------------------------------------------------------------------------
#               Graph Coefficients Reduced Form (1976-90)
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

#load regression results
illy <-  t(read.csv(str_c(paths[['a']], 'table_re_edu_level_0y.csv'))) %>%
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

priy <-  t(read.csv(str_c(paths[['a']], 'table_re_priy.csv'))) %>%
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

secy <-  t(read.csv(str_c(paths[['a']], 'table_re_secy.csv'))) %>%
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

colly <-  t(read.csv(str_c(paths[['a']], 'table_re_colly.csv'))) %>%
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

#-------------------------------------------------------------------------
#     Figure A.3: Plot coefficients for random effects on illiteracy
#------------------------------------------------------------------------

illy_re <- data.frame(matrix(unlist(illy), 
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
png(str_c(paths[['a']], "graph_coeff_illy.png"))
print(illy_re)
dev.off()

#-------------------------------------------------------------------------
#     Figure 13: Plot coefficients for random effects on primary
#------------------------------------------------------------------------

priy_re <- data.frame(matrix(unlist(priy), 
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
png(str_c(paths[['a']], "graph_coeff_priy.png"))
print(priy_re)
dev.off()

#-------------------------------------------------------------------------
#     Figure A.4: Plot coefficients for random effects on secondary
#------------------------------------------------------------------------

secy_re <- data.frame(matrix(unlist(secy), 
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
png(str_c(paths[['a']], "graph_coeff_secy.png"))
print(secy_re)
dev.off()

#-------------------------------------------------------------------------
#     Figure A.5: Plot coefficients for random effects on college
#------------------------------------------------------------------------

colly_re <- data.frame(matrix(unlist(colly), 
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
png(str_c(paths[['a']], "graph_coeff_colly.png"))
print(colly_re)
dev.off()