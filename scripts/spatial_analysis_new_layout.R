#updated spatial analysis code:

#last modified june 5, 2020

#purpose is all spatial data + analysis (diets, zoops, and environment)

##### LOAD LIBRARIES #####

library(tidyverse)
#data wrangling/graphs/read in data
library(ggdendro)
#dendrograms
library(vegan)
#analysis
library(RColorBrewer)
#graph colors
library(clustsig)
#testing cluster grouping significance
library(dietr)
#selectivity indices
library(here)
#project oriented workflow

##### ENVIRONMENTAL DATA #####

# Read in data file:

spat_data_raw <- read_csv(here("processed", "spatial_data", "spatial_survey_ysi.csv"))
#read in spatial environmental data

# Reorder spatial sites from alphabetical to migration route order:

spat_site_order <- c("J02", "J08", "J06", "D11", "D09", "D07")

spat_envr_data$site_id <- factor(spat_envr_data$site_id, levels = spat_site_order)



##### ZOOP DATA #####



##### SALMON DATA PREP #####

# Read in main data file for spatial diets:

spat_data_raw <- read_csv(here("processed", "spatial_data", "spatial_pink_chum_diets.csv"))
#read in spatial diet data

# Reorder salmon species as factors for creating graphs:

species_order <- c("Pink", "Chum") #make vector for rearranging species
spat_data_raw$fish_species <- factor(spat_data_raw$fish_species, levels = species_order)
#reorder species from alphabetical to pink salmon first before chum salmon

spat_data_raw$site_id <- factor(spat_data_raw$site_id, levels=spat_site_order)
#reorder sites (West to East) for the diet dataset

# Merge rare (< 3 stom.) taxonomic groups to higher prey levels:

spat_diet_data <- spat_data_raw
#make a copy before modifying taxonomic groups

