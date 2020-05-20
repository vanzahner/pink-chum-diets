#updated data wrangling code:

#last modified may 20, 2020

#purpose is: transform raw data into spatial and temporal data for analysis

library(tidyverse)
#multiple libraries (reading, manipulating and displaying data)
library(here)
#project workflow

here()
#check working directory "msc_project"

##### ENVIRONMENTAL DATA #####

ysi_data <- read_csv(here("data","ysi.csv"))
#read in data file for temperature and salinity, paired with salmon surveys

ysi_filtered <- ysi_data %>%
  filter(site_id %in% c("D07", "J07", "D09", "D11", 
                        "J02", "J06", "J08") & 
           survey_date>"2015-05-20"&
           survey_date<"2016-07-06")
#need to filter out to only what's relevant to samples

survey_data <- read_csv(here("data","survey_data.csv"))
#read in salmon survey data file for secchi measurements and other data

# ... continue adding in environmental wrangling then zoops then combine

# need to try to streamline with Hakai data and limit manual data changing.

##### ZOOP DATA #####



##### SALMON DATA #####

# Read in salmon data files: 

diet_data <- read_csv(here("data","pink_chum_diets_raw_data.csv"))
#read in juvenile pink and chum salmon diets raw data file

fish_meta_data <- read_csv(here("data","pink_chum_fish_info_filtered_data.csv"))
#read in fish data file (weights, lengths, etc.)

seine_data <- read_csv(here("data","pink_chum_seine_raw_data.csv"))
#seine data for lat long info and seine ID to connect with zoops ?

# Join together salmon data files:

intermediate_fish_data <- left_join(diet_data, fish_meta_data,
                                    by=c("ufn", "semsp_id"))
#join tables to merge the meta data with the diet data

all_salmon_data <- left_join(intermediate_fish_data, seine_data) %>%
  filter(taxa_detail_calc!="Goop") #delete stomach goop, is not a food item
#transformed (merged) dataset with all 312 fish (spatial + temporal)

# Reorder site and salmon species as factors for creating graphs:

site_order <- c("J02", "J08", "J06", "D11", "D09", "D07", "J07")
all_salmon_data$sample_site <- factor(all_salmon_data$sample_site, levels = site_order)
#reorder sites for spatial to be same as on the map; temporal = D07, J07

species_order <- c("Pink", "Chum")
all_salmon_data$fish_species <- factor(all_salmon_data$fish_species, levels = species_order)
#reorder species from alphabetical to pink salmon first before chum salmon

# Create individual dataframes for spatial and temporal chapters/analysis:

spat_fish_data <- filter(all_salmon_data, analysis!="Temporal")
#make datafile for only spatial analysis fish (by excluding temporal)

temp_fish_data <- filter(all_salmon_data, analysis!="Spatial")
#make datafile for only temporal analysis fish (by excluding spatial)

# Save spatial and temporal salmon diet datasets for further analysis:

write_csv(spat_fish_data, here("processed", "spatial_pink_chum_diets.csv"))
write_csv(temp_fish_data, here("processed", "temporal_pink_chum_diets.csv"))
#write csv files for initial transformation and saving of diet data

