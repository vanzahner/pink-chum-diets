# Spatial analysis code for MSc thesis on juvenile pink and chum salmon diets #

##### SET UP #####

rm(list=ls())
#remove other R stuff

library(readr)
#read in files
library(ggplot2)
#graphs
library(tidyverse)
#data wrangling

setwd("/Users/Vanessa/Desktop/msc_project")
#set working directory

spat_data <- read_csv("processed/spatial_pink_chum_diets.csv")
#read in spatial diet data

#load in file with old and new taxa names to be assigned
spat_names<-read.csv("data/spatial_category_change.csv") 

#for loop doesn't like data as factors
spat_data$taxa_detail_calc <- as.character(spat_data$taxa_detail_calc) 
spat_names$old_category <- as.character(spat_names$old_category)
spat_names$new_category <- as.character(spat_names$new_category)

#for loop that will go through all the organism names in the data spreadsheet 
#and for each one it will go to the names spreadsheet and reassign the name accordingly
for (n in spat_names$old_category) {
  spat_data$taxa_detail_calc[which(spat_data$taxa_detail_calc %in% n)] <- spat_names$new_category[which(spat_names$old_category == n)]
}

spat_biomass_data <- spat_data %>%
  group_by(ufn, fish_species, sample_date, sample_site, taxa_detail_calc, semsp_id,
           year, sampling_week, bolus_weight, weight, fork_length, microscope_hours) %>%
  summarise(Biomass=sum(prey_weight))
#simplify dataset and combine any redundancies

unique(spat_biomass_data$taxa_detail_calc)
#166 taxa groups --> Simplified to 118. (what about lrg/sml calanoids?) and n=8 empties

spat_numbers_taxa <- spat_biomass_data %>%
  ungroup() %>%
  count(taxa_detail_calc)

spat_data_wide <- spat_biomass_data %>%
  ungroup() %>% 
  select(ufn, fish_species, sample_site, bolus_weight, weight, fork_length, taxa_detail_calc, Biomass, microscope_hours) %>% 
  group_by(ufn, fish_species, sample_site) %>% 
  spread(key=taxa_detail_calc, value=Biomass, fill = 0)

sum(spat_data_wide$microscope_hours)
#432 hours at the microscope for spatial alone... average time per stomach of 3.6 hours!

##### NMDS/Cluster #####

