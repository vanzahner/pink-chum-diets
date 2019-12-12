# Spatial analysis code for MSc thesis on juvenile pink and chum salmon diets #

rm(list=ls())
#remove other R stuff

library(readr)
#read in files
library(ggplot2)
#graphs
library(dplyr)
#data wrangling

setwd("/Users/Vanessa/Desktop/Nov desktop/R Projects/msc_project")
#set working directory

spat_data <- read_csv("processed/spatial_pink_chum_diets.csv")

spat_biomass_data <- spat_data %>%
  group_by(ufn, fish_species, sample_date, sample_site, taxa_group, taxa_detail_calc, semsp_id,
           year, sampling_week, bolus_weight, weight, fork_length, size_class, digestion_state) %>%
  summarise(Biomass=sum(relative_biomass))
#simplify dataset and combine any redundancies

unique(spat_biomass_data$taxa_detail_calc)
#167 taxa groups (way too many?)

spat_biomass_data %>%
  ungroup() %>%
  count(taxa_detail_calc)

#pick up here tomorrow: change taxa when n=1 and repeat for temporal and run analyses.