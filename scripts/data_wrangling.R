#updated data wrangling code:

#last modified october 10, 2020

#purpose is: transform raw data into spatial and temporal data for analysis

##### SET UP ##### 

#load libraries:
library(tidyverse)
#multiple libraries (reading, manipulating and displaying data)
library(here)
#project workflow

# Create individual dataframes for spatial and temporal chapters/analysis:

spatial_info <- data.frame(site_id=c("D07", "D09", "D11", "J06", "J08", "J02"),
                           survey_date=c("2016-06-16", "2016-06-14", "2016-06-08",
                                         "2016-06-11", "2016-06-10", "2016-06-09"), stringsAsFactors = FALSE)

temporal_info <- data.frame(site_id=c("D07", "J07", "D07", "D07", "D07", "J07", "J07", "D07", "D07", "J07", "D07", "J07", "J07"),
                            survey_date=c("2015-05-21", "2015-06-02", "2015-06-05", "2015-06-07", "2015-06-13", "2015-06-14", "2015-06-29",
                                          "2016-05-19", "2016-06-03", "2016-06-03", "2016-06-16", "2016-06-20", "2016-07-05"), stringsAsFactors = FALSE)

# Read in data that's needed for more than one code chunk:

survey_data_raw <- read.csv(url("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/survey_data.csv"), stringsAsFactors = FALSE)
#read in salmon survey data file for secchi measurements and other data

survey_data_raw$survey_date <- as.Date(survey_data_raw$survey_date)
#change from character to date to calculate yday

survey_data <- mutate(survey_data_raw, year=str_sub(survey_date, end=4),
                      yday=lubridate::yday(survey_date),
                      week=lubridate::week(survey_date))
#create more manageable date categories to work with for the temporal analysis

survey_data$survey_date <- as.character(survey_data$survey_date)
#change back into character so it can merged with other datasets

##### ENVIRONMENTAL DATA #####

# Read in environmental ysi data:

ysi_data <- read.csv(url("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/ysi.csv"), stringsAsFactors = FALSE)
#read in data file for temperature and salinity, paired with salmon surveys

# Combine datasets:

survey_ysi <- left_join(survey_data, ysi_data, by=c("survey_date", "site_id"))
#combine survey data and ysi data

spat_envr_data <- semi_join(survey_ysi, spatial_info, by=c("site_id", "survey_date"))
#make datafile for only spatial analysis ocean conditions

temp_envr_data <- semi_join(survey_ysi, temporal_info, by=c("site_id", "survey_date"))
#make datafile for only temporal analysis ocean conditions

# Save spatial and temporal envr. datasets for further analysis:

write_csv(spat_envr_data, here("processed", "spatial_data", "spatial_survey_ysi.csv"))
write_csv(temp_envr_data, here("processed", "temporal_data", "temporal_survey_ysi.csv"))
#write csv files for initial transformation and saving of envr data

##### ZOOP DATA #####

# Data relationships for zooplankton:
#weight and comp data, connect by sample_id and sieve (merges 2 datasets)
#weight/comp  and tax data, connect by sample_id, adds tow_id
#weight/comp/tax and tow data, connect by tow_id
#connecting them all together gets site_id and survey_date (all metadata)

# Read in zooplankton data:

zoop_ww_data <- read.csv(here("data", "zoop_wet_weight_raw_data.csv"), stringsAsFactors = FALSE)
#read in data file that has size fractionated wet weight zoop data

zoop_comp_data <- read.csv(here("data", "zoop_taxonomic_raw_data.csv"), stringsAsFactors = FALSE)
#read in data file that has zooplankton taxonomic composition data

zoop_tax_metadata <- read.csv(url("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/zoop_tax.csv"), stringsAsFactors = FALSE)
#read in zoop tax meta data to get tow ID

zoop_tow <- read.csv(url("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/zoop_tows.csv"), stringsAsFactors = FALSE)
#read in zoop tow meta data to put it all together

# Update zoop taxa information using taxonomic columns:

zoop_taxa_data <- zoop_comp_data %>%
  mutate(taxa_info=(if_else(species=="",
                    if_else(genus=="",
                    if_else(family=="",
                    if_else(infraorder=="",
                    if_else(order=="",
                    if_else(subclass=="",
                    if_else(class=="",
                    phylum, class), subclass),
                    order), infraorder), family), genus),
                    paste(genus, species, sep="_"))),
         prey_info = (ifelse((life_stage==""), taxa_info,
                      paste(taxa_info, life_stage, sep="_"))))
#use taxonomic columns to get a final zoop column of taxa + life stage

# Combine zoop data sets:

intermediate_zoop_data <- left_join(zoop_taxa_data, zoop_ww_data, by=c("sample_id", "sieve"))
#join two zooplankton data sets together

tow_tax <- left_join(zoop_tax_metadata, zoop_tow, by="tow_id")
#join two Hakai github meta data sets together

all_zoop_data <- left_join(intermediate_zoop_data, tow_tax, by="sample_id")
#join together Hakai and processed data (filtering out whats needed)

# Create individual dataframes for spatial and temporal chapters/analysis:

spat_zoop_data <- semi_join(all_zoop_data, spatial_info, by=c("site_id", "survey_date"))
#make datafile for only spatial analysis zooplankton

temp_zoop_prep <- semi_join(all_zoop_data, temporal_info, by=c("site_id", "survey_date"))

temp_zoop_prep$survey_date <- as.Date(temp_zoop_prep$survey_date)

temp_zoop_data <- mutate(temp_zoop_prep, year=str_sub(survey_date, end=4),
                         yday=lubridate::yday(survey_date))
#make datafile for only temporal analysis zooplankton

# Save spatial and temporal zoop datasets for further analysis:

write_csv(spat_zoop_data, here("processed", "spatial_data", "spatial_zoop_data.csv"))
write_csv(temp_zoop_data, here("processed", "temporal_data", "temporal_zoop_data.csv"))
#write csv files for initial transformation and saving of zoop data

##### SALMON DATA #####

# Data relationships for salmon:
#diet and lab data, connect by ufn (important for metadata, not connecting)
#diet/lab and field data, connect by ufn, adds seine_id
#diet/lab/field and seine data, connect by seine_id, adds survey_id
#diet/lab/field/seine and survey data, connect by survey_id
#connecting them all together gets site_id and survey_date (all metadata)

# Read in salmon data files: 

diet_data <- read.csv(here("data","pink_chum_diets_raw_data.csv"), stringsAsFactors = FALSE)
#read in juvenile pink and chum salmon diets raw data file

fish_lab_data <- read.csv(url("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/fish_lab_data.csv"), stringsAsFactors = FALSE)  

fish_field_data <- read.csv(url("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/fish_field_data.csv"), stringsAsFactors = FALSE) #%>%
  #select(-species) #drop fish species column (it's redundant)
#unless column is renamed "fish_species" in Hakai data,
#then fish_species can be deleted from raw diet dataset
#since there's a conflicting species column regarding prey

fish_meta_data <- left_join(fish_lab_data, select(fish_field_data, -species), by="ufn")
#combine fish meta data files (weights, lengths, etc.)

seine_data <- read.csv(url("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/seine_data.csv"), stringsAsFactors = FALSE)
#seine data for lat long info and seine ID to connect with zoops ?

# Modify diet data to include an automatically calculated prey category:

updated_diet_data <- diet_data %>%
  mutate(taxa_info=if_else(species=="",
                   if_else(genus=="",
                   if_else(family=="",
                   if_else(infraorder=="",
                   if_else(suborder=="",
                   if_else(order=="",
                   if_else(subclass=="",
                   if_else(class=="",
                   if_else(subphylum=="",
                   if_else(phylum=="",
                    kingdom, phylum), subphylum), class), subclass),
                    order), suborder), infraorder), family), genus),
                    paste(genus, species, sep="_")),
         prey_info = ifelse(life_stage=="", taxa_info,
                             paste(taxa_info, life_stage, sep="_"))) %>%
  filter(prey_info!="Goop") #delete stomach goop, it's not a prey item
#use taxonomic columns to get a final prey column of taxa + life stage

# Join together salmon data files:

intermediate_fish_data <- left_join(updated_diet_data, fish_meta_data,
                                    by=c("ufn"))
#join tables to merge the meta data with the diet data

filter_salmon_data <- left_join(intermediate_fish_data, seine_data, by="seine_id")
#transformed dataset - deleted stomach goop since it's not a food item

all_salmon_data <- left_join(filter_salmon_data, survey_data, by=c("survey_id")) %>%
  select(ufn, fish_species, site_id, everything())
#dataset with all 312 fish (spatial + temporal), bring site_id to the front

# Create individual dataframes for spatial and temporal chapters/analysis:

spat_fish_data <- semi_join(all_salmon_data, spatial_info, by=c("site_id", "survey_date"))
#make datafile for only spatial analysis fish

temp_fish_data <- semi_join(all_salmon_data, temporal_info, by=c("site_id", "survey_date"))
#make datafile for only temporal analysis fish

# Save spatial and temporal salmon diet datasets for further analysis:

write_csv(spat_fish_data, here("processed", "spatial_data", "spatial_pink_chum_diets.csv"))
write_csv(temp_fish_data, here("processed", "temporal_data", "temporal_pink_chum_diets.csv"))
#write csv files for initial transformation and saving of diet data

# note: salmon data is merged with summarized zoop+envr (but kept detailed zoop & env)

##### CPUE #####

seine_survey_data <- left_join(seine_data, survey_data, by="survey_id")

all_fish_totals <- seine_survey_data %>%
  select(seine_id, site_id, survey_date, year, yday, so=so_total, pi=pi_total, cu=cu_total, co=co_total, he=he_total, ck=ck_total)

all_fish_calc <- all_fish_totals %>%
  gather(key="species", value ="cpue", so:ck) %>% 
  group_by(seine_id, site_id, year, yday, survey_date, species) %>%
  filter(is.na(cpue)!=TRUE & yday<191
         & site_id %in% c("D07", "D09", "D11", "J08", "J06", "J02")) %>% # cut it off at july 7th (johnson etal 2019) 
  summarise(cpue=sum(cpue)) %>%
  arrange(year, species, yday) %>%
  mutate(region=str_sub(site_id, start = 1, end=1))

full_migration_fish <- all_fish_calc %>%
  group_by(year, species#, region
           ) %>%
  summarise(peak=sum(cpue)/2, .groups="keep") %>%
  arrange(year, species#, region
          )
# to get 50% "peak outmigration"

median_fish <- all_fish_calc %>%
  group_by(year, species#, region
           ) %>%
  arrange(year, species, yday) %>% 
  summarise(cum_cpue=cumsum(cpue)) %>%
  arrange(year, species#, region
          )

median_fish_dates <- cbind(median_fish, yday=all_fish_calc$yday, survey_date=all_fish_calc$survey_date)

final_fish_dates <- left_join(median_fish_dates, full_migration_fish, by=c("year", "species"#, "region"
                                                                           )) %>%
  filter(cum_cpue>=peak) %>%
  group_by(year, species#, region
           ) %>%
  summarise(survey_date=first(survey_date), yday=first(yday))

ave_fish_dates <- left_join(median_fish_dates, full_migration_fish, by=c("year", "species")) %>%
  filter(cum_cpue>=peak) %>%
  group_by(species) %>%
  summarise(yday=first(yday))

ave_fish_dates <- final_fish_dates %>%
  group_by(species) %>%
  summarise(yday=mean(yday))

# I finally figured out how to calculate the peak outmigration period! Hooray.

# next step: make an actual table (for appendix??) of peak dates + ave for pink/chum

p <- all_fish_calc %>%
  #mutate(log_cpue=log(cpue+1)) %>% 
  filter(species %in% c("pi", "cu")
         & year %in% c("2015", "2016")
         ) %>%
  ggplot(aes(yday, cpue, group=interaction(species, year)))+
  geom_bar(aes(fill=species), stat="identity", position="dodge")+
  geom_vline(xintercept = 160, color="#516959", linetype="dashed")+
  scale_fill_manual(values=c("#516959", "#d294af"))+
  facet_wrap(~year, nrow=2)
  
mean_dates <- data.frame(
  year=c("2015", "2015", "2015", "2015", "2016", "2016", "2016", "2016"),
  species=c("pi", "cu", "pi", "cu", "pi", "cu", "pi", "cu"),
  size_code=c("lil", "lil", "big", "big", "lil", "lil", "big", "big"),
  yday=c(175, 160, 171, 168, 168, 163, 171, 168),
  color_vals=c("#d294af", "#516959", "#d294af", "#516959", "#d294af", "#516959", "#d294af", "#516959"),
  thickness=c(0.5, 0.5, 0.75, 0.75, 0.5, 0.5, 0.75, 0.75))

p + geom_vline(mean_dates, aes(xintercept=yday, group=interaction(species, year),
                   color=species, size=size_code), stat="identity")

# figure this out later... only 2015 pink late (non-FR) and rest are early in 2015/2016.

#specifically, pi2015 = june 24, pi2016 = june 16, piave= 170.6/june 19,
# and cu2015 = june 9, cu2016 = june 11, cuave = 168.2/june 17

##### SIZES #####

fish_field_survey_data <- left_join(fish_field_data, seine_survey_data, by="seine_id")

fish_lab_survey_data <- left_join(fish_field_survey_data, fish_lab_data, by="ufn") %>%
  mutate(region=str_sub(seine_id, start=1, end=1))

fish_lab_survey_data$fork_length <- as.numeric(fish_lab_survey_data$fork_length)

fish_fl_lab <- filter(fish_lab_survey_data, is.na(fork_length)!=TRUE)

fish_fl_field_all <- filter(fish_lab_survey_data, is.na(fork_length_field)!=TRUE) %>%
  select(-fork_length)

fish_fl_field_relevant <- anti_join(fish_fl_field_all, fish_fl_lab) %>%
  select(fork_length=fork_length_field, everything())

fish_fl_all <- full_join(fish_fl_field_relevant, fish_fl_lab)

fish_fl_all %>%
  #mutate(fl=if_else(is.na(fork_length), fork_length_field, fork_length)) %>% 
  filter(species %in% c("PI", "CU")
  #       & is.na(fl)!=TRUE
  ) %>% 
  ggplot(aes(yday, fork_length, group=interaction(species, region, year)))+
  geom_bar(aes(fill=species), stat="identity", position="dodge")+
  scale_fill_manual(values=c("#516959", "#d294af"))+
  #facet_wrap(region~year, nrow = 2)
  facet_wrap(year~region, nrow=5)

fish_fl_all %>%
  filter(species %in% c("PI", "CU") & week<29) %>% 
  group_by(species, region, week) %>%
  summarise(ave_fl=mean(fork_length)) %>%
  ggplot(aes(week, ave_fl), group=interaction(species, region))+
  geom_bar(aes(fill=species), stat="identity", position="dodge")+
  scale_fill_manual(values=c("#516959", "#d294af"))+
  facet_wrap(~region, nrow=1)
