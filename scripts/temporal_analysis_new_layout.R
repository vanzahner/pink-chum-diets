#updated temporal analysis code:

#last modified june 24, 2020

#purpose is all temporal data + analysis (diets, zoops, and environment)

##### LIBRARIES #####

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
library(kableExtra)
library(knitr)
library(formattable)
#for creating nice tables

##### ENVR + ZOOP DATA #####

# Read in environmental data file:

temp_envr_data_raw <- read.csv(here("processed", "temporal_data", "temporal_survey_ysi.csv"), stringsAsFactors = FALSE)
#read in temporal environmental data

temp_envr_data_raw$survey_date <- as.Date(temp_envr_data_raw$survey_date)
#make sure dates are treated like dates

temp_envr_data <- temp_envr_data_raw %>%
  select(survey_id, survey_date, year, yday, site_id, line_out_depth, temperature, salinity, collected)
#simplify columns + add year/usable dates (+ make a copy of raw data before modifying)

temp_envr_surface <- filter(temp_envr_data, line_out_depth==0)
#filter to surface envr data to combine with zoop and salmon data (without duplicates)

# Read in zooplankton data:

temp_zoop_data <- read.csv(here("processed", "temporal_data", "temporal_zoop_data.csv"), stringsAsFactors = FALSE)
#read in data file for zooplankton temporal data

temp_zoop_data$survey_date <- as.Date(temp_zoop_data$survey_date)
#make sure dates are treated like dates

# Zooplankton biomass data set up:

temp_zoop_ww <- temp_zoop_data %>%
  select(site_id, survey_date, year, yday, sieve, biomass, processor_notes) %>%
  unique() %>%
  mutate(size_frac=if_else(processor_notes=="gelatinous",
                           "2000 (Gelatinous)", as.character(sieve)))
#create dataframe for graphing biomass of zooplankton

temp_zoop_ww$size_frac <- factor(temp_zoop_ww$size_frac, levels = c("250", "1000", "2000", "2000 (Gelatinous)"))
#reorder size fractions to be in numerical order for graphing

temp_zoop_ww_total <- temp_zoop_ww %>%
  group_by(site_id, survey_date) %>%
  summarise(zoop_ww=sum(biomass))
#create dataframe to append together to salmon stomach + envr data sets

temp_zoop_envr <- left_join(temp_envr_surface, temp_zoop_ww_total, by=c("site_id", "survey_date"))
#join zoop and envr data together (so it can be joined to salmon data)

# Update zoop groups for relative abundance / taxa composition graph:

temp_zoop_intermediate <- temp_zoop_data %>%
  mutate(prey_group=if_else(class=="Sagittoidea" | phylum=="Mollusca" | phylum=="Echinodermata" | phylum=="Ochrophyta" | phylum=="Bryozoa", phylum,
                    if_else(genus=="Oikopleura" | class=="Actinopterygii" | class=="Polychaeta" | class=="Insecta", class,
                    if_else(phylum=="Cnidaria" | phylum=="Ctenophora", phylum, #"Cnidaria_Ctenophora",
                    if_else(family=="Caligidae", "Parasites",
                    #if_else(life_stage=="egg", "Eggs",
                    if_else(prey_info=="Copepoda_nauplius", "Calanoida",
                    if_else(order=="Calanoida" | order=="Decapoda" | order=="Amphipoda" |
                            order=="Harpacticoida" | order=="Cyclopoida", order,
                    if_else(infraorder=="Balanomorpha", infraorder,
                    if_else(family=="Euphausiidae", family,
                    if_else(family=="Podonidae", "Cladocera",
                            prey_info))))))))))#)
#update zooplankton groups for summary and graphs

zoop_group_data <- temp_zoop_intermediate %>%
  mutate(prey_group_simple=if_else(prey_group!="Calanoida" & prey_group!="Decapoda" & prey_group!="Euphausiidae" & prey_group!="Amphipoda" & prey_group!="Harpacticoida" & 
                                     prey_group!="Cnidaria_Ctenophora" & prey_group!="Appendicularia" & #prey_group!="Chaetognatha" &
                                     prey_group!="Balanomorpha" & prey_group!="Cladocera" & prey_group!="Mollusca" & prey_group!="Cyclopoida", 
                                   "Other", prey_group))
# keep prey groups that are substantial, rest = "Other" prey category

zoop_levels <- c("Calanoida", "Decapoda", "Euphausiidae", "Amphipoda", "Harpacticoida",
                 "Cnidaria_Ctenophora", "Appendicularia", #"Chaetognatha",
                 "Mollusca", "Cyclopoida", "Balanomorpha", "Cladocera", "Other")
#put the taxa groups in an order that somewhat matches the diet comp later on

zoop_colors <- c("#E31A1C", "#FDBF6F", "#FF7F00", "#B2DF8A", "#33A02C", "#A6CEE3",
                 "#1F78B4", #"#CAB2D6", 
                 "#E7298A", "#FB9A99", "#A6761D", "#E6AB02", "#6A3D9A")
#red, Lorange, orange, green, Lblue, blue, Lpurple, pink, hotpink, Y, Br, purple

zoop_group_data$prey_group_simple <- factor(zoop_group_data$prey_group_simple, levels = zoop_levels)
#reorder levels to what is a nice visualization and will match the colors

zoop_group_sum <- zoop_group_data %>%
  group_by(site_id, prey_group_simple) %>%
  summarise(total_abd=sum(abundance))
#summarise total abundance for each sample to plot relative abd of zoops

##### ENVR + ZOOP GRAPHS #####

# Graph for environmental data:

temp_envr_data %>%
  ggplot(aes(survey_date, temperature))+
  geom_line(aes(group=NA))+
  theme_bw(base_size = 12)+
  geom_line(aes(y=salinity/2, x=survey_date, group=NA), color="red")+
  scale_y_continuous(sec.axis = sec_axis(~.*2, name= "Salinity (‰)"))+
  scale_x_date(date_breaks = "11 days", date_labels = "%b %d")+ 
  facet_grid(site_id~year, scales = "free_x")+
  theme(axis.title.y.right = element_text(color = "red"),
        panel.grid=element_blank(),
        axis.text.y.right = element_text(color="red"),
        axis.text.y.left = element_text(color="black"),
        axis.text.x = element_text(color="black"),
        strip.text = element_text(size=16),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size=14), axis.text = element_text(size=12),
        legend.text = element_text(size=12), legend.title = element_text(size=14))+
  labs(y="Temperature (°C)", x="Site")
#temperature and salinity graph combo

ggsave(here("figs", "temporal_figs", "temp_salinity_temporal.png"))

# Note: graphed secchi has same trend as temperature
#warm fresh DI= high secchi; cooler/saltier DI = low secchi (mixing); cold/sal JS=high

# Biomass Graph:

temp_zoop_ww %>% 
  ggplot(aes(survey_date, biomass))+
  geom_bar(aes(fill=size_frac), width=2, stat="identity")+
  theme_bw()+
  scale_fill_brewer(palette = "Dark2")+
  scale_x_date(date_breaks = "11 days", date_labels = "%b %d")+ 
  facet_grid(site_id~year, scales = "free_x")+
  theme(panel.grid=element_blank(), strip.text = element_text(size=16),
        axis.ticks.x = element_blank(), axis.text.x = element_text(color="black"),
        axis.text.y=element_text(color="black"),
        axis.title = element_text(size=14), axis.text = element_text(size=12),
        legend.text = element_text(size=12), legend.title = element_text(size=14))+
  labs(x="Site", y="Biomass (mg/m³)", fill="Size Fraction (μm)")
#graph of zooplankton biomass (total and by size fraction)

ggsave(here("figs", "temporal_figs", "zoop_biomass_temporal.png"))
#save zoop biomass graph to folder

# ADD COMPOSITION GRAPH IN HERE AFTER SORTING OUT THE PREY GROUPS *****

##### ENVR + ZOOP TABLES #####

# TBD :)

##### SALMON DATA - READ IN #####

# Read in main data file for temporal diets:

temp_diet_raw <- read.csv(here("processed", "temporal_data", "temporal_pink_chum_diets.csv"), stringsAsFactors = FALSE)
#read in temporal diet data

temp_diet_raw$survey_date <- as.Date(temp_diet_raw$survey_date)
#make sure dates are treated like dates

# Reorder salmon species + date as factors for creating better graphs:

species_order <- c("Pink", "Chum") #make vector for rearranging species
temp_diet_raw$fish_species <- factor(temp_diet_raw$fish_species, levels = species_order)
#reorder species from alphabetical to pink salmon first before chum salmon

temp_data_combo <- left_join(temp_diet_raw, temp_zoop_envr, by=c("site_id", "survey_id", "survey_date"))
#join together the salmon and zoop/envr data

temp_diet_copy <- filter(temp_data_combo, prey_info!="Digested_food")
#make a copy of data before modifying the raw data (and remove dig. food)

##### SALMON DATA - TAXA REGROUP #####

# Merge rare (< 3 stom.) taxonomic groups to higher prey levels:

temp_diet_data <- temp_diet_copy %>%
  filter(prey_info!="Digested_food") %>% 
  group_by(ufn, fish_species, site_id, taxa_info, prey_info,
           kingdom, phylum, subphylum, class, subclass, order, suborder, infraorder, family, genus, species, life_stage) %>%
  summarise(biomass=sum(prey_weight_corr))
#create dataframe that sums any duplicates of prey groups in each stomach

temp_diet_sum <- temp_diet_data %>%
  ungroup() %>%
  group_by(kingdom, phylum, subphylum, class, subclass, order, suborder, infraorder, family, genus, species, life_stage,
           taxa_info, prey_info) %>%
  tally() %>%
  arrange(n)
#calculate how many stomachs each prey group appears in

# NEED TO CHANGE THIS TO BE RELEVANT TO TEMPORAL FREQUENCY OF PREY *****
# CAN'T IGNORE, IT'S CAUSING ISSUES IN THE CODE PIPELINE *****

temp_diet_groups <- temp_diet_sum %>%
  mutate(taxa_new=if_else(n<3 & genus!="Neotrypaea", 
                  if_else(life_stage=="Object" | life_stage=="Detritus", "",
                  if_else(subphylum=="Chelicerata" | subphylum=="Hexapoda" | genus=="Kellia", phylum,
                  if_else(class=="Ostracoda", subphylum,
                  if_else(genus=="Candacia" | genus=="Paraeuchaeta" | genus=="Eurytemora" |
                          order=="Cyclopoida" | genus=="Limacina" | genus=="Epilabidocera" |
                          order=="Harpacticoida", order,
                  if_else(genus=="Amphibalanus"  | (suborder=="Senticaudata" & genus!="Caprella"), suborder,
                  if_else(family=="Paguridae" | genus=="Eualus", infraorder,
                  if_else(genus=="Hyperia" | family=="Pinnotheridae" | family=="Podonidae" |
                          genus=="Nematoscelis", family,
                  if_else(class=="Phaeophyceae", "Detritus",
                  if_else(phylum=="Nematoda" | family=="Caligidae", "Parasites",
                  if_else(species!="", genus,
                          taxa_info)))))))))),
                  if_else(order=="Pteropoda" & life_stage=="Veliger", phylum,
                  if_else(order=="Cumacea", order,
                  if_else(family=="Pleuronectidae", family,
                  if_else(genus=="Neocalanus", genus,
                  if_else(class=="Trematoda", "Parasites",
                          taxa_info)))))),
         life_stage_new=if_else(str_detect(life_stage, "Zoea") | 
                                order=="Decapoda" & life_stage=="Megalopa" | 
                                order=="Calanoida" & (life_stage=="Nauplii" | life_stage=="Copepodite"), "Larvae",
                        if_else(family=="Euphausiidae" & life_stage=="Juvenile" |
                                taxa_new=="Arthropoda" | taxa_new=="Parasites", "",
                                life_stage)),
         prey_new=if_else(life_stage_new=="", taxa_new,
                  if_else(taxa_new=="", life_stage_new, 
                          paste(taxa_new, life_stage_new, sep="_")))) %>%
  ungroup %>% 
  select(prey_info, prey_new)
#update any prey groups that occur in less than three fish stomachs!

for (n in temp_diet_groups$prey_info) {
  temp_diet_copy$prey_info[which(temp_diet_copy$prey_info %in% n)] <- temp_diet_groups$prey_new[which(temp_diet_groups$prey_info == n)]
}
#overwrite old data with new prey groups

temp_diet_check <- temp_diet_copy %>%
  group_by(ufn, fish_species, site_id, prey_info) %>%
  summarise(biomass=sum(prey_weight_corr))
#create dataframe that sums any duplicates of prey groups in each stomach

temp_diet_filtered <- temp_diet_check %>%
  ungroup() %>%
  group_by(prey_info) %>%
  tally() %>%
  arrange(n)
#calculate how many stomachs each prey group appears in (none <3!)
#reduced number of taxa from 163 to 91, a lot more manageable now!

temp_diet_intermediate <- temp_diet_copy %>%
  mutate(prey_group=if_else(class=="Sagittoidea" | phylum=="Mollusca" | phylum=="Echinodermata" | phylum=="Ochrophyta", phylum,
                    if_else(genus=="Oikopleura" | class=="Actinopterygii" | class=="Polychaeta", class,
                    if_else(order=="Calanoida" | order=="Decapoda" 
                            | order=="Amphipoda" | order=="Cumacea" | order=="Isopoda"
                            | order=="Harpacticoida" | order=="Cyclopoida", order,
                    if_else(suborder=="Balanomorpha", suborder,
                    if_else(family=="Euphausiidae" | family=="Podonidae", family,
                    if_else(class=="Insecta" | class=="Arachnida", "Insecta_Arachnida",
                    if_else(phylum=="Cnidaria" | phylum=="Ctenophora", "Cnidaria_Ctenophora",
                    if_else(prey_info=="Copepoda", "Crustacea",
                    if_else(life_stage=="Object", life_stage,
                            prey_info))))))))))
#create more general prey groups for any needed tables, graphs, calc, etc.

##### SALMON DATA - PREP ##### 

# RESUME HERE TO TRY TO CALCULATE SIZE BIN HISTOGRAM ON FRIDAY ! *

#temp_diet_wide <- temp_diet_intermediate %>%
temp_diet_wide <- temp_diet_copy %>% 
  group_by(ufn, fish_species, site_id, prey_group) %>%
  summarise(biomass=sum(prey_weight_corr)) %>%
  spread(key=prey_group, value = biomass, fill=0) %>%
  ungroup()
#calculate wide data set with new prey groups (detailed and general!)

temp_diet_info <- select(temp_diet_wide, ufn, fish_species, site_id)
#create dataframe with UFNs, site and species for reattaching to matrices

temp_diet_matrix <- temp_diet_wide %>%
  select(Actinopterygii:Polychaeta) %>% 
  decostand(method="total")
#matrix to calculation relative biomass of 25 different prey groups

temp_diet_rel_bio <- cbind(temp_diet_info, temp_diet_matrix)
#combine ufn/site/species back onto relative biomass of prey groups data

# Calculate average relative biomass of prey for each site/date and species:

temp_diet_rel_bio %>%
  gather(key="prey", value="rel_bio", Actinopterygii:Polychaeta) %>% 
  group_by(fish_species, prey, site_id) %>%
  summarise(average=mean(rel_bio)*100) %>%
  summarise(max=max(average)) %>%
  filter(max>8) %>%
  arrange(prey)
#this calculation tells me which prey groups are on average >8% relative ww

temp_diet_all <- temp_diet_intermediate %>%
  mutate(prey_group_simple=if_else(prey_group!="Calanoida" & prey_group!="Decapoda" & prey_group!="Euphausiidae" & prey_group!="Amphipoda" & prey_group!="Harpacticoida" & 
                                     prey_group!="Insecta_Arachnida" & prey_group!="Cnidaria_Ctenophora" & prey_group!="Appendicularia" & prey_group!="Chaetognatha", "Other", prey_group))
# keep prey groups that are substantial, rest = "Other" prey category

prey_levels <- c("Calanoida", "Decapoda", "Euphausiidae", "Amphipoda", "Harpacticoida",
                 "Insecta_Arachnida", "Cnidaria_Ctenophora", "Appendicularia", "Chaetognatha", "Other")
#vector to reorder prey groups into what makes sense for diet comp bargraph

color_levels <- c("#E31A1C", "#FDBF6F", "#FF7F00", "#B2DF8A", "#33A02C", 
                  "#666666", "#A6CEE3", "#1F78B4", "#CAB2D6", "#6A3D9A")
#red, Lorange, orange, Lgreen, green, grey, Lblue, blue, Lpurple, purple

temp_diet_all$prey_group_simple <- factor(temp_diet_all$prey_group_simple, levels = prey_levels)
#reorder taxa groups into correct order for printing graphs (and tables)

temporal_diets <- select(temp_diet_all, ufn, fish_species, site_id, survey_date, food_weight_corr, prey_info, prey_group, prey_group_simple,
                        count, digestion_state, prey_weight_corr, length_avg, size_class, adipose, weight, fork_length, seine_id, survey_id,
                        temperature, salinity, collected, zoop_ww, set_time, time_searching, so_taken:he_total, precip:wind_direction, secchi)
#delete useless columns (can further simplify later), this=working dataset

temp_stomachs <- temporal_diets %>%
  select(ufn, fish_species, site_id, survey_date, food_weight_corr, weight, fork_length, fork_length, adipose, seine_id, survey_id,
         temperature, salinity, collected, zoop_ww, set_time, time_searching, so_taken:he_total, precip:wind_direction, secchi) %>%
  unique()
#metadata of salmon stomachs with no prey info (envr, zoops, fish ww, etc)

##### SIZE BINS #####

