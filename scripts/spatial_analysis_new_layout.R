#updated spatial analysis code:

#last modified june 14, 2020

#purpose is all spatial data + analysis (diets, zoops, and environment)

##### SET UP #####

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

# Vector to reorder alphabetical spatial sites  to migration route order:

spat_site_order <- c("J02", "J08", "J06", "D11", "D09", "D07")

##### ENVIRONMENTAL GRAPHS #####

# Read in data file:

spat_envr_data_raw <- read.csv(here("processed", "spatial_data", "spatial_survey_ysi.csv"), stringsAsFactors = FALSE)
#read in spatial environmental data

# Reorder spatial sites from alphabetical to migration route order:

spat_envr_data_raw$site_id <- factor(spat_envr_data_raw$site_id, levels = spat_site_order)

spat_envr_data <- spat_envr_data_raw %>%
  filter(line_out_depth==0 & survey_id!="DE317") %>%
  select(survey_id, site_id, temperature, salinity, collected)
#filter to surface data - error in 1 m depth D11 salinity and duplicate D09

# Graph for environmental data:

spat_envr_data %>%
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
        strip.text = element_text(size=16),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size=14), axis.text = element_text(size=12),
        legend.text = element_text(size=12), legend.title = element_text(size=14))+
  labs(y="Temperature (°C)", x="Site")
#temperature and salinity graph combo

ggsave(here("figs", "spatial_figs", "temp_salinity_spatial.png"))

##### ZOOPLANKTON GRAPHS #####

spat_zoop_data <- read.csv(here("processed", "spatial_data", "spatial_zoop_data.csv"), stringsAsFactors = FALSE)
#read in data file for zooplankton spatial data

spat_zoop_data$site_id <- factor(spat_zoop_data$site_id, levels = spat_site_order)
#reorder sites for spatial to be same as on the map; temporal = D07, J07

# Biomass Graph:

spat_zoop_ww <- spat_zoop_data %>%
  select(site_id, sieve, biomass, processor_notes) %>%
  unique() %>%
  mutate(size_frac=if_else(processor_notes=="gelatinous",
    "2000 (Gelatinous)", as.character(sieve)))
#create dataframe for graphing biomass of zooplankton

spat_zoop_ww$size_frac <- factor(spat_zoop_ww$size_frac, levels = c("250", "1000", "2000", "2000 (Gelatinous)"))
#reorder size fractions to be in numerical order for graphing

spat_zoop_ww_total <- spat_zoop_ww %>%
  group_by(site_id) %>%
  summarise(zoop_ww=sum(biomass))
#create dataframe to append together to salmon stomach + envr data sets

spat_zoop_envr <- left_join(spat_envr_data, spat_zoop_ww_total, by="site_id")

spat_zoop_ww %>% 
  ggplot(aes(site_id, biomass))+
  geom_bar(aes(fill=size_frac), stat="identity")+
  theme_bw()+
  scale_fill_brewer(palette = "Dark2")+
  theme(panel.grid=element_blank(), strip.text = element_text(size=16),
        axis.ticks.x = element_blank(), axis.text.x = element_text(color="black"),
        axis.text.y=element_text(color="black"),
        axis.title = element_text(size=14), axis.text = element_text(size=12),
        legend.text = element_text(size=12), legend.title = element_text(size=14))+
  labs(x="Site", y="Biomass (mg/m³)", fill="Size Fraction (μm)")
#graph of zooplankton biomass (total and by size fraction)

ggsave(here("figs", "spatial_figs", "zoop_biomass_spatial.png"))
#save zoop biomass graph to folder

# Zoop taxa composition graph:

zoop_group_data <- spat_zoop_data %>%
  mutate(prey_group=if_else(order=="Cyclopoida", "Cyclopoids",
                    if_else(order=="Calanoida", "Calanoids",
                    if_else(order=="Decapoda", "Decapods",
                    if_else(family=="Euphausiidae", "Euphausiids",
                    if_else(class=="Insecta" | class=="Arachnida", "Insects",
                    if_else(order=="Harpacticoida", "Harpacticoids",
                    if_else(phylum=="Cnidaria" | phylum=="Ctenophora",  "Gelatinous",
                    if_else(genus=="Oikopleura", "Larvaceans",
                    if_else(class=="Sagittoidea", "Chaetognaths",
                    "Other"))))))))))
#update zooplankton groups for summary and graphs

zoop_levels <- c("Cyclopoids", "Calanoids", "Decapods", "Euphausiids",
                 "Harpacticoids", "Gelatinous", "Larvaceans", "Chaetognaths", "Other")
#put the taxa groups in an order that matches the diet comp later on

zoop_colors <- c("#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00",
                  "#33A02C", "#A6CEE3", "#1F78B4", "#CAB2D6", "#6A3D9A")
#red, pink, orange, Lorange, green, blue, Lblue, purple, Lpurple

zoop_group_data$prey_group <- factor(zoop_group_data$prey_group, levels = zoop_levels)
#reorder levels

zoop_group_sum <- zoop_group_data %>%
  group_by(site_id, prey_group) %>%
  summarise(total_abd=sum(tot_count))

zoop_group_sum %>%
  ggplot(aes(site_id, total_abd))+
  geom_bar(aes(fill=prey_group), stat="identity", position="fill")+
  scale_fill_manual(values=zoop_colors)+
  theme_bw()+
  scale_y_continuous(labels= scales::percent)+
  theme(panel.grid=element_blank(), strip.text = element_text(size=16),
        axis.ticks.x = element_blank(), axis.text.x = element_text(color="black"),
        axis.text.y=element_text(color="black"),
        axis.title = element_text(size=14), axis.text = element_text(size=12),
        legend.text = element_text(size=12), legend.title = element_text(size=14))+
  labs(x="Site", y="Relative abundance", fill="Zooplankton Group")
#zoop comp graph (other=barnacles, bivalves, cladocerans, mostly.)

ggsave(here("figs", "spatial_figs", "zoop_comp_spatial.png"))
#save the zoop taxa comp. graph

##### SALMON DATA PREP #####

# Read in main data file for spatial diets:

spat_diet_raw <- read.csv(here("processed", "spatial_data", "spatial_pink_chum_diets.csv"), stringsAsFactors = FALSE)
#read in spatial diet data

# Reorder salmon species as factors for creating graphs:

species_order <- c("Pink", "Chum") #make vector for rearranging species
spat_diet_raw$fish_species <- factor(spat_diet_raw$fish_species, levels = species_order)
#reorder species from alphabetical to pink salmon first before chum salmon

spat_diet_raw$site_id <- factor(spat_diet_raw$site_id, levels=spat_site_order)
#reorder sites (West to East) for the diet dataset

spat_data_combo <- left_join(spat_diet_raw, spat_zoop_envr, by=c("site_id", "survey_id"))

spat_diet_copy <- filter(spat_data_combo, prey_info!="Digested_food")
#make a copy of data before modifying the raw data (and remove dig. food)

# Merge rare (< 3 stom.) taxonomic groups to higher prey levels:

spat_diet_data <- spat_diet_copy %>%
  filter(prey_info!="Digested_food") %>% 
  group_by(ufn, fish_species, site_id, taxa_info, prey_info,
           kingdom, phylum, subphylum, class, subclass, order, suborder, infraorder, family, genus, species, life_stage) %>%
  summarise(biomass=sum(prey_weight_corr))
#create dataframe that sums any duplicates of prey groups in each stomach

spat_diet_sum <- spat_diet_data %>%
  ungroup() %>%
  group_by(kingdom, phylum, subphylum, class, subclass, order, suborder, infraorder, family, genus, species, life_stage,
           taxa_info, prey_info) %>%
  tally() %>%
  arrange(n)
#calculate how many stomachs each prey group appears in

spat_diet_groups <- spat_diet_sum %>%
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

for (n in spat_diet_groups$prey_info) {
  spat_diet_copy$prey_info[which(spat_diet_copy$prey_info %in% n)] <- spat_diet_groups$prey_new[which(spat_diet_groups$prey_info == n)]
}
#overwrite old data with new prey groups

spat_diet_check <- spat_diet_copy %>%
  group_by(ufn, fish_species, site_id, prey_info) %>%
  summarise(biomass=sum(prey_weight_corr))
#create dataframe that sums any duplicates of prey groups in each stomach

spat_diet_filtered <- spat_diet_check %>%
  ungroup() %>%
  group_by(prey_info) %>%
  tally() %>%
  arrange(n)
#calculate how many stomachs each prey group appears in (none <3!)
#reduced number of taxa from 163 to 91, a lot more manageable now!

spat_diet_intermediate <- spat_diet_copy %>%
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
                    prey_info)))))))))
#create more general prey groups for any needed tables, graphs, calc, etc.

spat_diet_wide <- spat_diet_intermediate %>%
  group_by(ufn, fish_species, site_id, prey_group) %>%
  summarise(biomass=sum(prey_weight_corr)) %>%
  spread(key=prey_group, value = biomass, fill=0) %>%
  ungroup()
#calculate wide data set with new prey groups (detailed and general!)

spat_diet_info <- select(spat_diet_wide, ufn, fish_species, site_id)
#create dataframe with UFNs, site and species for reattaching to matrices

spat_diet_matrix <- spat_diet_wide %>%
  select(Actinopterygii:Polychaeta) %>% 
  decostand(method="total")
#matrix to calculation relative biomass of 25 different prey groups

spat_diet_rel_bio <- cbind(spat_diet_info, spat_diet_matrix)
#combine ufn/site/species back onto relative biomass of prey groups data

# Calculate average relative biomass of prey for each site and species:

spat_diet_rel_bio %>%
  gather(key="prey", value="rel_bio", Actinopterygii:Polychaeta) %>% 
  group_by(fish_species, prey, site_id) %>%
  summarise(average=mean(rel_bio)*100) %>%
  summarise(max=max(average)) %>%
  filter(max>8) %>%
  arrange(prey)
#this calculation tells me which prey groups are on average >8% relative ww

spat_diet_all <- spat_diet_intermediate %>%
  mutate(prey_group_simple=if_else(prey_group!="Calanoida" & prey_group!="Decapoda" & prey_group!="Euphausiidae" & prey_group!="Amphipoda" & prey_group!="Harpacticoida" & 
                                     prey_group!="Insecta_Arachnida" & prey_group!="Cnidaria_Ctenophora" & prey_group!="Appendicularia" & prey_group!="Chaetognatha", "Other", prey_group))
# keep prey groups that are substantial, rest = "Other" prey category

prey_levels <- c("Calanoida", "Decapoda", "Euphausiidae", "Amphipoda", "Harpacticoida",
                 "Insecta_Arachnida", "Cnidaria_Ctenophora", "Appendicularia", "Chaetognatha", "Other")
#vector to reorder prey groups into what makes sense for diet comp bargraph

color_levels <- c("#E31A1C", "#FDBF6F", "#FF7F00", "#B2DF8A", "#33A02C", 
                  "#666666", "#A6CEE3", "#1F78B4", "#CAB2D6", "#6A3D9A")
#red, Lorange, orange, Lgreen, green, grey, Lblue, blue, Lpurple, purple

spat_diet_all$prey_group_simple <- factor(spat_diet_all$prey_group_simple, levels = prey_levels)
#reorder taxa groups into correct order for printing graphs (and tables)

spatial_diets <- select(spat_diet_all, ufn, fish_species, site_id, survey_date, food_weight_corr, prey_info, prey_group, prey_group_simple,
                        count, digestion_state, prey_weight_corr, length_avg, size_class, adipose, weight, fork_length, seine_id, survey_id,
                        temperature, salinity, collected, zoop_ww, set_time, time_searching, so_taken:he_total, precip:wind_direction, secchi)
#delete useless columns (can further simplify later), this=working dataset

spat_stomachs <- spatial_diets %>%
  select(ufn, fish_species, site_id, survey_date, food_weight_corr, weight, fork_length, fork_length, adipose, seine_id, survey_id,
         temperature, salinity, collected, zoop_ww, set_time, time_searching, so_taken:he_total, precip:wind_direction, secchi) %>%
  unique()
#metadata of salmon stomachs with no prey info (envr, zoops, fish ww, etc)

# NEXT STEP: wide data ands,,,,

##### SALMON TABLES :) #####

#Sample summary

#Taxa summary

#Indices summary

#What else was there?