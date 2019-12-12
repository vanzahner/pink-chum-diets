# Temporal analysis code for MSc thesis on juvenile pink and chum salmon diets #

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

temp_data <- read_csv("processed/temporal_pink_chum_diets.csv")

#load in file with old and new taxa names to be assigned
temp_names<-read.csv("data/temporal_category_change.csv") 

#for loop doesn't like data as factors
temp_data$taxa_detail_calc <- as.character(temp_data$taxa_detail_calc) 
temp_names$old_category <- as.character(temp_names$old_category)
temp_names$new_category <- as.character(temp_names$new_category)

#for loop that will go through all the organism names in the data spreadsheet 
#and for each one it will go to the names spreadsheet and reassign the name accordingly
for (n in temp_names$old_category) {
  temp_data$taxa_detail_calc[which(temp_data$taxa_detail_calc %in% n)] <- temp_names$new_category[which(temp_names$old_category == n)]
}

temp_biomass_data <- temp_data %>%
  group_by(ufn, fish_species, sample_date, sample_site, taxa_detail_calc, semsp_id,
           year, sampling_week, bolus_weight, weight, fork_length, microscope_hours) %>%
  summarise(Biomass=sum(prey_weight))
#simplify dataset and combine any redundancies

unique(temp_biomass_data$taxa_detail_calc)
#179 taxa groups (way too many?) --> Simplified to 131. n=7 empties (like 100% empty.)

temp_numbers_taxa <- temp_biomass_data %>%
  ungroup() %>%
  count(taxa_detail_calc)

temp_data_wide <- temp_biomass_data %>%
  ungroup() %>% 
  select(ufn, fish_species, sample_site, bolus_weight, weight, fork_length, taxa_detail_calc, Biomass, microscope_hours) %>% 
  group_by(ufn, fish_species, sample_site) %>% 
  spread(key=taxa_detail_calc, value=Biomass, fill = 0)

sum(temp_data_wide$microscope_hours)
#634 hours at the microscope for temporal alone... average time per stomach of 3.0 hours!

##### NMDS/Cluster #####

