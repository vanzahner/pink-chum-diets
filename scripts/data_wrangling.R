# Data wrangling code for MSc thesis on juvenile pink and chum salmon diets #

rm(list=ls())
#remove other R stuff

library(readr)
#read in files
library(dplyr)
#data wrangling

setwd("/Users/Vanessa/Desktop/Nov desktop/R Projects/msc_project")
#set working directory

raw_data <- read_csv("data/pink_chum_diets_raw_data.csv")
#read in raw data file

metadata <- read_csv("data/pink_chum_fish_info_filtered_data.csv")
#new version - edited hakai_id to be ufn (NEED DATA DICTIONARY AND CHANGELOG!*)

seinedata <- read_csv("data/pink_chum_seine_raw_data.csv")
#seine data for lat long info (column names fixed, lat and long were mixed up!)

fishdata <- left_join(raw_data, metadata, by=c("ufn", "semsp_id"))
#join tables to merge the meta data with the diet data!

fish <- left_join(fishdata, seinedata)
#transformed dataset with all 312 fish (spatial + temporal)

temp_fish <- filter(fish, analysis!="Spatial")
#make datafile for only temporal analysis fish

spat_fish <- filter(fish, analysis!="Temporal")
#make datafile for only spatial analysis fish

write_csv(spat_fish, path="processed/spatial_pink_chum_diets.csv")
write_csv(temp_fish, path="processed/temporal_pink_chum_diets.csv")
#write csv files for initial transformation and saving of diet data