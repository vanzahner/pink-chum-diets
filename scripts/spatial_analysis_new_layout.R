#updated spatial analysis code:

#last modified june 23, 2020

#purpose is all spatial data + analysis (diets, zoops, and environment)

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

spat_envr_data_raw <- read.csv(here("processed", "spatial_data", "spatial_survey_ysi.csv"), stringsAsFactors = FALSE)
#read in spatial environmental data

# Reorder spatial sites from alphabetical to migration route order:

spat_site_order <- c("J02", "J08", "J06", "D11", "D09", "D07")
# Vector to reorder alphabetical spatial sites  to migration route order:

spat_envr_data_raw$site_id <- factor(spat_envr_data_raw$site_id, levels = spat_site_order)
#reorder levels in zoop dataframe for plotting

spat_envr_data <- spat_envr_data_raw %>%
  filter(line_out_depth==0 & survey_id!="DE317") %>%
  select(survey_id, survey_date, site_id, temperature, salinity, collected)
#filter to surface data - error in 1 m depth D11 salinity and duplicate D09

# Read in zooplankton data:

spat_zoop_data <- read.csv(here("processed", "spatial_data", "spatial_zoop_data.csv"), stringsAsFactors = FALSE)
#read in data file for zooplankton spatial data

spat_zoop_data$site_id <- factor(spat_zoop_data$site_id, levels = spat_site_order)
#reorder sites for spatial to be same as on the map; temporal = D07, J07

# Zooplankton biomass data set up:

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
#join zoop and envr data together (so it can be joined to salmon data)

# Update zoop groups for relative abundance / taxa composition graph:

spat_zoop_intermediate <- spat_zoop_data %>%
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

zoop_group_data <- spat_zoop_intermediate %>%
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

# Biomass Graph:

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

zoop_group_sum %>%
  ggplot(aes(site_id, total_abd))+
  geom_bar(aes(fill=prey_group_simple), stat="identity"#, position="fill"
           )+
  scale_fill_manual(values=zoop_colors)+
  theme_bw()+
  scale_y_continuous(labels=scales::comma)+
  theme(panel.grid=element_blank(), strip.text = element_text(size=16),
        axis.ticks.x = element_blank(), axis.text.x = element_text(color="black"),
        axis.text.y=element_text(color="black"),
        axis.title = element_text(size=14), axis.text = element_text(size=12),
        legend.text = element_text(size=12), legend.title = element_text(size=14))+
  labs(x="Site", y="Abundance (#/m³)", fill="Zooplankton Group")
#zoop comp graph 

ggsave(here("figs", "spatial_figs", "zoop_comp_spatial.png"))
#save the zoop taxa comp. graph

##### ENVR + ZOOP TABLES #####

# Sampling table:

zoop_table <- spat_zoop_ww %>%
  filter(site_id!="J02") %>% 
  select(site_id, sieve, biomass) %>%
  group_by(site_id, sieve) %>% 
  summarise(biomass=round(sum(biomass), digits = 2)) %>% 
  spread(sieve, biomass, fill=0) %>%
  mutate(Total=sum(`250`, `1000`, `2000`))

reverse_spat_sites <- c("D07", "D09", "D11", "J06", "J08", "J02")

spat_envr_data$site_id <- factor(spat_envr_data$site_id, levels = reverse_spat_sites)
zoop_table$site_id <- factor(zoop_table$site_id, levels = reverse_spat_sites)
#reorder levels in zoop dataframe for tables

zoop_envr_table <- left_join(spat_envr_data, zoop_table, by="site_id") %>%
  mutate(Region=c("DI", "DI", "NSoG", "QCSt", "JS", "JS"), `# Pink`=10, `# Chum`=10) %>% 
  select(Region, Site=site_id, Date=survey_date, `# Pink`, `# Chum`, `Temp. (°C)`=temperature, `Salinity (‰)`=salinity,
         `250 $\\mu$m`=`250`, `1000 $\\mu$m`=`1000`, `2000 $\\mu$m`=`2000`, Total) %>%
  arrange(Site, reverse_spat_sites)

zoop_envr_table$`250 $\\mu$m`[which(is.na(zoop_envr_table$`250 $\\mu$m`))] <- "No Data"
zoop_envr_table$`1000 $\\mu$m`[which(is.na(zoop_envr_table$`1000 $\\mu$m`))] <- "No Data"
zoop_envr_table$`2000 $\\mu$m`[which(is.na(zoop_envr_table$`2000 $\\mu$m`))] <- "No Data"
zoop_envr_table$Total[which(is.na(zoop_envr_table$Total))] <- "No Data"

kable(zoop_envr_table, "latex", booktabs=TRUE,
      escape = FALSE, align=c("l", "l", "l", "c", "c", "c", "c", "r", "r", "r", "r")) %>%
  add_header_above(c(" "=7, "Zooplankton Biomass (mg/m³)"=4)) %>% 
  save_kable(here("tables", "spatial_tables", "sampling_table.pdf"))

# Zooplankton abundance:

spat_zoop_intermediate$site_id <- factor(spat_zoop_intermediate$site_id, levels = reverse_spat_sites)
#reorder levels in zoop dataframe for tables

zoop_comp_table <- spat_zoop_intermediate %>%
  group_by(site_id, prey_group) %>%
  summarise(abd_group=round(sum(abundance), digits=2)) %>%
  spread(prey_group, abd_group, fill=0) %>%
  arrange(site_id, spat_site_order) %>%
  ungroup() %>% 
  rename(`Unknown Eggs`="Unknown_egg") %>% 
  select(-c("site_id", "Actinopterygii", "Ochrophyta", "Parasites")) %>% 
  t()
# if < 1 individual / m3 for each site = group gets filtered out

colnames(zoop_comp_table) <- spat_site_order

kable(zoop_comp_table, "latex", booktabs=TRUE, escape = FALSE) %>% 
  save_kable(here("tables", "spatial_tables", "zoop_relA_table.pdf"))

##### SALMON DATA - READ IN #####

# Read in main data file for spatial diets:

spat_diet_raw <- read.csv(here("processed", "spatial_data", "spatial_pink_chum_diets.csv"), stringsAsFactors = FALSE)
#read in spatial diet data

# Reorder salmon species as factors for creating graphs:

species_order <- c("Pink", "Chum") #make vector for rearranging species
spat_diet_raw$fish_species <- factor(spat_diet_raw$fish_species, levels = species_order)
#reorder species from alphabetical to pink salmon first before chum salmon

spat_diet_raw$site_id <- factor(spat_diet_raw$site_id, levels=spat_site_order)
#reorder sites (West to East) for the diet dataset

spat_data_combo <- left_join(spat_diet_raw, spat_zoop_envr, by=c("site_id", "survey_id", "survey_date"))
#join together the salmon and zoop/envr data

spat_diet_copy <- filter(spat_data_combo, prey_info!="Digested_food")
#make a copy of data before modifying the raw data (and remove dig. food)

##### SALMON DATA - TAXA REGROUP #####

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
                    if_else(life_stage=="Object", life_stage,
                    prey_info))))))))))
#create more general prey groups for any needed tables, graphs, calc, etc.

##### SALMON DATA - PREP ##### 

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
#use this for GFI table and graph

#create wide (?) dataframes: 

#detailed taxa by species/etc:
#spat_diet_taxa_rel_bio #for multivariate calculations/graphs, similarity

#spat_diet_taxa_pa #for Freq Occur (to find most important taxa)

#general taxa groups - all groups, no Other:
#spat_diet_groups_rel_bio #for prey comp summary table

#spat_diet_groups_rel_abd #for prey selectivity AND summary table

#spat_diet_groups_pa #for Freq Occur summary table

#general taxa groups - "Other" grouped together:
#spat_diet_other_rel_bio #for prey comp graph

##### SALMON TABLES - PREP #####

spat_gfi_table <- spat_stomachs %>%
  filter(is.na(weight)!=TRUE) %>% 
  select(fish_species, site_id, weight, food_weight_corr, fork_length) %>%
  mutate(weight_corr= weight*1000, # grams to milligrams * FIX IN RAW DATA LATER ! *
           gfi=food_weight_corr/weight_corr*100) %>% 
  group_by(fish_species, site_id) %>%
  summarise(mean_ww=round(mean(weight_corr), digits=1), se_ww=round(sd(weight_corr)/10, digits=1),
            mean_food=round(mean(food_weight_corr), digits=1), se_food=round(sd(food_weight_corr)/10, digits=1),
            mean_gfi=round(mean(gfi), digits=2), se_gfi=round(sd(gfi)/10, digits=2))

spat_length_table <- spat_stomachs %>%
  filter(is.na(fork_length)!=TRUE) %>%
  select(fish_species, site_id, fork_length) %>%
  group_by(fish_species, site_id) %>%
  summarise(mean_fl=round(mean(fork_length), digits=1), se_fl=round(sd(fork_length)/10, digits=1))

spat_empty_table <- spat_stomachs %>%
  filter(food_weight_corr==0) %>%
  group_by(fish_species, site_id) %>%
  count() %>%
  mutate(per_empty=n*10)

##### SALMON TABLES - INDICES ##### 

summed_data <- spatial_diets %>%
  filter(!prey_info %in% c("Coscinodiscophycidae", "Microplastic_chunk_Object",
                           "Object", "Parasites", "Detritus")) %>% 
  select(fish_species, site_id, prey_info, prey_weight_corr) %>%
  group_by(fish_species, site_id, prey_info) %>%
  summarise(totalw=sum(prey_weight_corr)) %>%
  spread(key=prey_info, value=totalw, fill=0) 

sites <- summed_data$site_id
salmon <- summed_data$fish_species

summed_matrix <- summed_data %>%
  ungroup() %>% 
  select(Acartia:Tortanus_discaudatus) %>% 
  decostand(method="total")

proportional_sums <- cbind(sites, salmon, summed_matrix)

calculate_overlap <- function(dataset, site) {
  dataset %>%
    filter(sites==site) %>%
    select(-c(sites, salmon)) %>%
    summarise_all(min) %>%
    rowSums()
  }

D07sim <- calculate_overlap(proportional_sums, "D07")
D09sim <- calculate_overlap(proportional_sums, "D09")
D11sim <- calculate_overlap(proportional_sums, "D11")
J06sim <- calculate_overlap(proportional_sums, "J06")
J08sim <- calculate_overlap(proportional_sums, "J08")
J02sim <- calculate_overlap(proportional_sums, "J02")

per_overlap <- data.frame(site_id=c("J02", "J08", "J06", "D11", "D09", "D07"),
                          overlap=c(J02sim, J08sim, J06sim, D11sim, D09sim, D07sim))

per_overlap$site_id <- factor(per_overlap$site_id, levels = reverse_spat_sites)

duplicateddata <- data.frame(site_id=rep(c("J02", "J08", "J06", "D11", "D09", "D07"), 2),
                             overlap=c(round(J02sim*100, digits = 1), round(J08sim*100, digits = 1), round(J06sim*100, digits = 1), round(D11sim*100, digits = 1), "33.0", round(D07sim*100, digits = 1), "", "", "", "", "", ""))
#D09 sim = 33.00698 and rounded = 33.0 but round doesn't include zeros. so code is 33.0

# merge peroverlap, spat_empty_table (replace NAs), spat_length_table, spat_gfi_table:

gfi_fl_table <- left_join(spat_gfi_table, spat_length_table, by=c("fish_species", "site_id"))

gfi_empty_table <- left_join(gfi_fl_table, spat_empty_table, by=c("fish_species", "site_id"))

gfi_overlap_table <- bind_cols(gfi_empty_table, duplicateddata)

gfi_overlap_table$n[which(is.na(gfi_overlap_table$n)==TRUE)] <- 0
gfi_overlap_table$per_empty[which(is.na(gfi_overlap_table$per_empty)==TRUE)] <- 0

gfi_overlap_table$site_id <- factor(gfi_overlap_table$site_id, levels = reverse_spat_sites)

gfi_all_data_table <- gfi_overlap_table %>%
  mutate(fl= paste(mean_fl, se_fl, sep=" ± "),
         fishw= paste(comma(mean_ww, digits=1), se_ww, sep=" ± "),
         food= paste(mean_food, se_food, sep=" ± "),
         GFI= paste(mean_gfi, se_gfi, sep=" ± ")) %>%
  select(Species=fish_species, Site=site_id, `Fish FL (mm)`=fl, `Fish WW (mg)`=fishw,
         #`Food WW (mg)`=food,
         GFI=GFI, `# Empty`=n, #`% Empty Stom.`=per_empty,
         `Overlap`=overlap) %>%
  unique() %>%
  arrange(Site, c("D07", "D07", "D09", "D09", "D11", "D11", "J06", "J06", "J08", "J08", "J02", "J02"))

kable(gfi_all_data_table, "latex", booktabs=TRUE, align=c(rep("l", 4), rep("c", 3)),
      linesep= c('', '\\addlinespace')) %>% 
  save_kable(here("tables", "spatial_tables", "index_table.pdf"))

##### SALMON TABLES - DIET COMP #####

group_biomass <- spatial_diets %>%
  filter(food_weight_corr!=0) %>% 
  group_by(ufn, fish_species, site_id, prey_group) %>%
  summarise(prey_weight_sum=sum(prey_weight_corr))
#summarize biomass for each fish

group_bio_wide <- group_biomass %>%
  ungroup() %>%
  select(ufn, fish_species, site_id, prey_group, prey_weight_sum) %>% 
  group_by(ufn, fish_species, site_id) %>% 
  spread(key=prey_group, value = prey_weight_sum, fill=0)
#wide data set (might not need it, but it's a good double check that n=120!)  

group_bio_mat <- group_bio_wide %>%
  ungroup() %>% 
  select(-c(ufn, fish_species, site_id, Detritus, Echinodermata, Ochrophyta, Parasites, Podonidae, Isopoda)) %>%
  decostand(method="total")

group_bio_per <- group_bio_mat*100

group_bio_data <- group_bio_per %>%
  mutate(site=group_bio_wide$site_id, fish=group_bio_wide$fish_species) %>%
  gather(key="taxa", value="rel_bio", Actinopterygii:Polychaeta) %>% 
  group_by(site, fish, taxa) %>%
  summarise(rel_bio=round(mean(rel_bio), digits=1)) %>%
#  ungroup() %>% 
#  select(-site) %>% 
  spread(taxa, rel_bio) %>%
  t()

group_bio_dataframe <- data.frame(group_bio_data)

diet_table <- group_bio_dataframe[3:nrow(group_bio_dataframe), ]

#colnames(group_bio_data) <- c("D07", "D07", "D09", "D09", "D11", "D11", "J06", "J06", "J08", "J08", "J02", "J02")
#colnames(diet_table) <- c("J02", "J02", "J08", "J08", "J06", "J06", "D11", "D11", "D09", "D09", "D07", "D07")

colnames(diet_table) <- rep(c("PI", "CU"), 6)

kable(diet_table, "latex", booktabs=TRUE, linesep="") %>%
  add_header_above(c(" "=1, "J02"=2, "J08"=2, "J06"=2, "D11"=2, "D09"=2, "D07"=2)) %>%
  save_kable(here("tables", "spatial_tables", "diet_comp_table.pdf"))


##### SALMON GRAPHS - TBA #####