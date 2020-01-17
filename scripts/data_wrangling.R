# Data wrangling code for MSc thesis on juvenile pink and chum salmon diets #

rm(list=ls())
#remove other R stuff

library(readr)
#read in files
library(tidyverse)
#data wrangling

setwd("/Users/Vanessa/Desktop/msc_project")
#set working directory

raw_data <- read_csv("data/pink_chum_diets_raw_data.csv")
#read in raw data file

metadata <- read_csv("data/pink_chum_fish_info_filtered_data.csv")
#new version - edited hakai_id to be ufn (NEED DATA DICTIONARY AND CHANGELOG!*)

seinedata <- read_csv("data/pink_chum_seine_raw_data.csv")
#seine data for lat long info (column names fixed, lat and long were mixed up!)

fishdata <- left_join(raw_data, metadata, by=c("ufn", "semsp_id"))
#join tables to merge the meta data with the diet data!

fish <- left_join(fishdata, seinedata) %>%
  filter(taxa_detail_calc!="Goop")
#transformed dataset with all 312 fish (spatial + temporal)

site_order <- c("J02", "J08", "J06", "D11", "D09", "D07", "J07")
fish$sample_site <- factor(fish$sample_site, levels = site_order)
#reorder sites from the default of alphabetical to west to east, like on the map

species_order <- c("Pink", "Chum")
fish$fish_species <- factor(fish$fish_species, levels = species_order)
#reorder species from the default of alphabetical to pink then chum, for graph reasons

temp_fish <- filter(fish, analysis!="Spatial")
#make datafile for only temporal analysis fish

spat_fish <- filter(fish, analysis!="Temporal")
#make datafile for only spatial analysis fish

write_csv(spat_fish, path="processed/spatial_pink_chum_diets.csv")
write_csv(temp_fish, path="processed/temporal_pink_chum_diets.csv")
#write csv files for initial transformation and saving of diet data
