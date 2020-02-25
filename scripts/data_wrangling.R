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

temp_fish <- filter(fish, analysis!="Spatial")
#make datafile for only temporal analysis fish

spat_fish <- filter(fish, analysis!="Temporal")
#make datafile for only spatial analysis fish

write_csv(spat_fish, path="processed/spatial_pink_chum_diets.csv")
write_csv(temp_fish, path="processed/temporal_pink_chum_diets.csv")
#write csv files for initial transformation and saving of diet data

#ENV AND ZOOP DATA#

ysi_data <- read_csv("data/ysi.csv")

ysi_filtered <- ysi_data %>%
  filter(site_id %in% c("D07", "J07", "D09", "D11", 
                        "J02", "J06", "J08") & 
           survey_date>"2015-05-20"&
           survey_date<"2016-07-06")
#need to filter out to only what's relevant to samples (then secchi next)

zoop_data <- read_csv("data/zoop_comp_data_combined.csv")
#need to resolve issues about missing data (JSPK 1154, which is for July 5, 2016 J07)

zoop_data_ww <- read_csv("data/zoop_data_ww.csv")
#J02 (JSPK1118) has taxa data but no wet weight. ignore all ww since J02 most important

survey_data <- read_csv("data/survey_data.csv")
#for secchi!

fish_rename <- rename(fish, survey_id=jsp_survey_id)

survey_fish <- left_join(fish_rename, survey_data, by="survey_id")

survey_rename <- rename(survey_fish, site_id=sample_site)

survey_ysi <- left_join(survey_rename, ysi_filtered, by=c("survey_date", "site_id"))

survey_filtered <- survey_ysi %>%
  select(site_id, sample_date, year, analysis, seine_id, date, work_area, site_id.x, survey_id,
         sampling_week, gather_long.x, gather_lat.x, set_time, survey_date, site_id.y, precip, cloud_cover,
         sea_state, wind_speed, wind_direction, secchi, gather_lat.y, gather_long.y, line_out_depth, collected,
         temperature, salinity) %>%
  unique()

survey_filtered$line_out_depth <- as.character(survey_filtered$line_out_depth)

site_order <- c("J02", "J08", "J06", "D11", "D09", "D07")
survey_filtered$site_id <- factor(survey_filtered$site_id, levels = site_order)
#reorder sites from the default of alphabetical to west to east, like on the map

survey_filtered %>%
  filter(analysis!="Temporal") %>% 
  ggplot(aes(site_id, secchi))+
  geom_boxplot(aes())+
  theme_bw()+
  theme(panel.grid=element_blank())+
  labs(title="Secchi Depth (Spatial)")
#line out depth exact same for 0 and 1 m.

survey_filtered %>%
  filter(analysis!="Temporal") %>% 
  ggplot(aes(site_id, temperature))+
  geom_boxplot(aes(color=line_out_depth))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  labs(title="Temperature (Spatial)")

survey_filtered %>%
  filter(analysis!="Temporal") %>% 
  ggplot(aes(site_id, salinity))+
  geom_boxplot(aes(color=line_out_depth))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  labs(title="Salinity (Spatial)")

zoop_data_ww$site <- factor(zoop_data_ww$site, levels = site_order)

zoop_data_ww$sieve <- as.character(zoop_data_ww$sieve)

sieve_order <- c("250", "1000", "2000")
zoop_data_ww$sieve <- factor(zoop_data_ww$sieve, levels = sieve_order)

zoop_data_ww %>%
  filter(sampleID %in% c("JSPK1122", "JSPK1123", "QPK734", "QPK751")) %>%
  ggplot(aes(site, biomass))+
  geom_boxplot(aes(color=sieve))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  labs(title="Zoop Biomass (Spatial)")

zoop_names <- read_csv("data/zoop_names.csv")

#for loop doesn't like data as factors
zoop_data$labID <- as.character(zoop_data$labID) 
zoop_names$old_category <- as.character(zoop_names$old_category)
zoop_names$new_category <- as.character(zoop_names$new_category)
#for loop that will go through all the organism names in the data spreadsheet 
#and for each one it will go to the names spreadsheet and reassign the name accordingly
for (n in zoop_names$old_category) {
  zoop_data$labID[which(zoop_data$labID %in% n)] <- zoop_names$new_category[which(zoop_names$old_category == n)]
}

zoop_data$site <- factor(zoop_data$site, levels = site_order)

zoop_data %>%
  filter(sampleID %in% c("JSPK1122", "JSPK1123", "QPK734", "QPK751", "JSPK1118")) %>%
  ggplot(aes(site, abundance))+
  geom_bar(aes(fill=labID), stat="identity", position="fill")+
  theme_bw()+
  theme(panel.grid=element_blank())+
  labs(title="Zoop Composition (Spatial)")

zoop_data %>%
  filter(sampleID %in% c("JSPK1122", "JSPK1123", "QPK734", "QPK751", "JSPK1118")) %>%
  ggplot(aes(site, abundance))+
  geom_bar(aes(fill=labID), stat="identity", position="fill")+
  theme_bw()+
  theme(panel.grid=element_blank())+
  labs(title="Zoop Composition (Spatial)")+
  facet_wrap(~sieve)
