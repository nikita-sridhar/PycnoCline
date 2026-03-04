#PYCNOCLINE - EXPERIMENT 1
#Authored by: Nikita Sridhar

#This script loads libraries, cleans and manipulates data required for analyses.

#load libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(sjstats) 
library(ggeffects)
library(emmeans)
library(glmmTMB)
library(readxl)
library(patchwork)
library(car)
library(ggsignif)
library(DHARMa)
library(ggpattern)
library(flextable)
library(betareg)
library(ggpubr)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(ggplot2)


#load files
kelp_raw <- read_csv(here::here("./data/raw/2023_Pycnocline_RawData - Kelp.csv"))
behavior_raw <- read_csv(here::here("data/raw/2023_Pycnocline_RawData - Behavior.csv"))


#Step 0 - Clean raw data
source(here::here("./scripts/00_cleaning.R"))
#Step 1 - Manipulate cleaned data to get ready for stats
source(here::here("./scripts/01_manipulation.R"))


#Save files
#write.csv(kelp_clean, file = "data/processed data/cleaned data/kelp_clean.csv")
#write.csv(behavior_clean, file = "data/processed data/cleaned data/behavior_clean.csv")
#write.csv(kelp, file = "data/processed data/manipulated data/kelp.csv")
#write.csv(urch_behavior, file = "data/processed data/manipulated data/urch_behavior.csv")

#clear environment
#rm(list = ls()); gc()
