#updated data wrangling code:

#last modified june 5, 2020

#purpose is: transform raw data into spatial and temporal data for analysis

#load libraries:
library(tidyverse)
#multiple libraries (reading, manipulating and displaying data)
library(here)
#project workflow

##### ENVIRONMENTAL DATA #####

# Read in environmental data:

ysi_data <- read.csv(url("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/ysi.csv"), stringsAsFactors = FALSE)
#read in data file for temperature and salinity, paired with salmon surveys

survey_data <- read.csv(url("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/survey_data.csv"), stringsAsFactors = FALSE)
#read in salmon survey data file for secchi measurements and other data

# Combine datasets:

survey_ysi <- left_join(survey_data, ysi_data, by=c("survey_date", "site_id"))
#combine survey data and ysi data

# Create individual dataframes for spatial and temporal chapters/analysis:

spat_envr_data <- semi_join(survey_ysi, spatial_info, by=c("site_id", "survey_date"))
#make datafile for only spatial analysis ocean conditions

temp_envr_data <- semi_join(survey_ysi, temporal_info, by=c("site_id", "survey_date"))
#make datafile for only temporal analysis ocean conditions

# Save spatial and temporal envr. datasets for further analysis:

write_csv(spat_envr_data, here("processed", "spatial_data", "spatial_survey_ysi.csv"))
write_csv(temp_envr_data, here("processed", "temporal_data", "temporal_survey_ysi.csv"))
#write csv files for initial transformation and saving of envr data

##### ZOOP DATA #####

# Read in zooplankton data:

zoop_data <- read_csv(here("data", "zoop_data_combo.csv"))
#read in data file that has both taxonomic and wet weight zoop data

zoop_tax <- read.csv(url("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/zoop_tax.csv"), stringsAsFactors = FALSE)
#read in zoop tax data to get tow ID

zoop_tow <- read.csv(url("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/zoop_tows.csv"), stringsAsFactors = FALSE)
#read in zoop tow meta data to put it all together

# Combine zoop data sets:

tow_tax <- left_join(zoop_tax, zoop_tow, by="tow_id")
#join Hakai github datasets together

all_zoop_data <- left_join(zoop_data, tow_tax, by="sample_id")
#join together Hakai and processed data (filtering out whats needed)

# Create individual dataframes for spatial and temporal chapters/analysis:

spat_zoop_data <- semi_join(all_zoop_data, spatial_info, by=c("site_id", "survey_date"))
#make datafile for only spatial analysis zooplankton

temp_zoop_data <- semi_join(all_zoop_data, temporal_info, by=c("site_id", "survey_date"))
#make datafile for only temporal analysis zooplankton

# Save spatial and temporal zoop datasets for further analysis:

write_csv(spat_zoop_data, here("processed", "spatial_data", "spatial_zoop_data.csv"))
write_csv(temp_zoop_data, here("processed", "temporal_data", "temporal_zoop_data.csv"))
#write csv files for initial transformation and saving of zoop data

##### SALMON DATA #####

# Data relationships:
#diet and lab data, connect by ufn (important for metadata, not connecting)
#diet/lab and field data, connect by ufn, adds seine_id
#diet/lab/field and seine data, connect by seine_id, adds survey_id
#diet/lab/field/seine and survey data, connect by survey_id
#connecting them all together gets site_id and survey_date (all metadata)

# Read in salmon data files: 

diet_data <- read.csv(here("data","pink_chum_diets_raw_data.csv"), stringsAsFactors = FALSE)
#read in juvenile pink and chum salmon diets raw data file

fish_lab_data <- read.csv(url("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/fish_lab_data.csv"), stringsAsFactors = FALSE)  

fish_field_data <- read.csv(url("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/fish_field_data.csv"), stringsAsFactors = FALSE) %>%
  select(-species) #drop fish species column (it's redundant)
#unless column is renamed "fish_species" in Hakai data,
#then fish_species can be deleted from raw diet dataset
#since there's a conflicting species column regarding prey

fish_meta_data <- left_join(fish_lab_data, fish_field_data, by="ufn")
#combine fish meta data files (weights, lengths, etc.)

seine_data <- read.csv(url("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/seine_data.csv"), stringsAsFactors = FALSE)
#seine data for lat long info and seine ID to connect with zoops ?

# Modify diet data to include an automatically calculated prey category:

updated_diet_data <- diet_data %>%
  mutate(taxa_info=(if_else((species==""),
                    (if_else((genus==""),
                    (if_else((family==""),
                    (if_else((infraorder==""),
                    (if_else((suborder==""),
                    (if_else((order==""),
                    (if_else((subclass==""),
                    (if_else((class==""),
                    (if_else((subphylum==""),
                    (if_else((phylum==""),
                    kingdom, phylum)), subphylum)), class)), subclass)),
                    order)), suborder)), infraorder)), family)), genus)),
                    paste(genus, species, sep="_"))),
         prey_info = (ifelse((life_stage==""), taxa_info,
                             paste(taxa_info, life_stage, sep="_")))) %>%
  filter(prey_info!="Goop") #delete stomach goop, it's not a prey item
#use taxonomic columns to get a final prey column of taxa + life stage

# Join together salmon data files:

intermediate_fish_data <- left_join(updated_diet_data, fish_meta_data,
                                    by=c("ufn"))
#join tables to merge the meta data with the diet data

filter_salmon_data <- left_join(intermediate_fish_data, seine_data, by="seine_id")
#transformed dataset - deleted stomach goop since it's not a food item

all_salmon_data <- left_join(filter_salmon_data, survey_data, by=c("survey_id"))
#dataset with all 312 fish (spatial + temporal)

# Create individual dataframes for spatial and temporal chapters/analysis:

spatial_info <- data.frame(site_id=c("D07", "D09", "D11", "J06", "J08", "J02"),
                           survey_date=c("2016-06-16", "2016-06-14", "2016-06-08",
                                         "2016-06-11", "2016-06-10", "2016-06-09"), stringsAsFactors = FALSE)

temporal_info <- data.frame(site_id=c("D07", "J07", "D07", "D07", "D07", "J07", "J07", "D07", "D07", "J07", "D07", "J07", "J07"),
                            survey_date=c("2015-05-21", "2015-06-02", "2015-06-05", "2015-06-07", "2015-06-13", "2015-06-14", "2015-06-29",
                                          "2016-05-19", "2016-06-03", "2016-06-03", "2016-06-16", "2016-06-20", "2016-07-05"), stringsAsFactors = FALSE)
#create dataframes for filtering out spatial sites/dates and temporal too

spat_fish_data <- semi_join(all_salmon_data, spatial_info, by=c("site_id", "survey_date"))
#make datafile for only spatial analysis fish

temp_fish_data <- semi_join(all_salmon_data, temporal_info, by=c("site_id", "survey_date"))
#make datafile for only temporal analysis fish

# Save spatial and temporal salmon diet datasets for further analysis:

write_csv(spat_fish_data, here("processed", "spatial_data", "spatial_pink_chum_diets.csv"))
write_csv(temp_fish_data, here("processed", "temporal_data", "temporal_pink_chum_diets.csv"))
#write csv files for initial transformation and saving of diet data

