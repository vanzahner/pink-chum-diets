#updated spatial analysis code:

#last modified june 8, 2020

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

spat_envr_data <- read_csv(here("processed", "spatial_data", "spatial_survey_ysi.csv"))
#read in spatial environmental data

# Reorder spatial sites from alphabetical to migration route order:

spat_site_order <- c("J02", "J08", "J06", "D11", "D09", "D07")

spat_envr_data$site_id <- factor(spat_envr_data$site_id, levels = spat_site_order)

# Graph for environmental data:

spat_envr_data %>%
  filter(line_out_depth==0) %>% 
  ggplot(aes(site_id, temperature))+
  geom_line(aes(group=NA))+
  theme_bw(base_size = 12)+
  geom_line(aes(y=salinity/2, x=site_id, group=NA), color="red")+
  scale_y_continuous(sec.axis = sec_axis(~.*2, name= "Salinity (‰)"))+
  theme(axis.title.y.right = element_text(color = "red"),
        panel.grid=element_blank(),
        axis.text.y.right = element_text(color="red"),
        axis.text.y.left = element_text(color="black"),
        axis.text.x = element_text(color="black"),
        axis.ticks.x = element_blank())+
  labs(y="Temperature (°C)", x="Site")
#temperature and salinity graph combo

ggsave(here("figs", "spatial_figs", "temp_salinity_spatial.png"))

##### ZOOP DATA #####

spat_zoop_data <- read_csv(here("processed", "spatial_data", "spatial_zoop_data.csv"))

spat_zoop_data$site_id <- factor(spat_zoop_data$site_id, levels = spat_site_order)
#reorder sites for spatial to be same as on the map; temporal = D07, J07

zoop_group_data <- spat_zoop_data %>%
  mutate(prey_group=if_else(order=="Cyclopoida", "Cyclopoids",
                    if_else(order=="Calanoida", "Calanoids",
                    if_else(order=="Decapoda", "Decapods",
                    if_else(family=="Euphausiidae", "Euphausiids",
                    if_else(class=="Insecta" | class=="Arachnida", "Insects",
                    if_else(order=="Harpacticoida", "Harpacticoids",
                    if_else(phylum=="Cnidaria" | phylum=="Ctenophora", "Gelatinous",
                    if_else(genus=="Oikopleura", "Larvaceans",
                    if_else(class=="Sagittoidea", "Chaetognaths",
                    "Other"))))))))))

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

