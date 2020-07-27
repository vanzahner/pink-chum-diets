#updated spatial analysis code:

#last modified july 27, 2020

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
library(dendextend)
library(ggdendro)
#cluster dendrograms

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
  mutate(prey_group=if_else(class=="Sagittoidea" | phylum=="Echinodermata" | phylum=="Ochrophyta" | phylum=="Bryozoa", phylum,
                    if_else(genus=="Oikopleura" | class=="Actinopterygii" | class=="Polychaeta" | class=="Insecta" | class=="Bivalvia", class,
                    if_else(phylum=="Cnidaria" | phylum=="Ctenophora", phylum, 
                    if_else(family=="Caligidae", "Parasites",
                    if_else(class=="Gastropoda", "Pteropoda",
                    if_else(prey_info=="Unknown_egg", "Euphausiidae Eggs",
                    if_else(order=="Decapoda" | order=="Amphipoda" |
                            order=="Harpacticoida" | order=="Cyclopoida", order,
                    if_else(order=="Calanoida" & (size_class=="<1" | size_class=="1 to 2"), "Small (<2mm)",
                    if_else(order=="Calanoida" & (size_class=="2 to 5" | size_class=="5 to 10"), "Large (>2mm)",
                    if_else(prey_info=="Copepoda_nauplius", "Small (<2mm)",
                    if_else(infraorder=="Balanomorpha", infraorder,
                    if_else(family=="Euphausiidae", "Euphausiidae Larvae",
                    if_else(family=="Podonidae", "Cladocera",
                            prey_info))))))))))))))
#update zooplankton groups for summary and graphs

str(spat_zoop_intermediate$size_class)

prey_level_details <- c("Hyperiidea", "Senticaudata", "Amphipoda", "Small (<2mm)", "Large (>2mm)", "Euphausiidae", "Adults",
                        "Euphausiidae Larvae", "Larvae", "Decapoda", "Insecta", "Arachnida",
                        "Harpacticoida", "Cnidaria", "Ctenophora", "Appendicularia", "Chaetognatha",
                        "Cyclopoida", "Bivalvia", "Pteropoda", "Polychaeta", "Actinopterygii",
                        "Balanomorpha", "Cumacea", "Isopoda", "Cladocera", "Euphausiidae Eggs", "Echinodermata",
                        "Object", "Ochrophyta", "Parasites")

zoop_group_data <- spat_zoop_intermediate %>%
  mutate(prey_group_simple=if_else(prey_group=="Cnidaria" | prey_group=="Ctenophora", "Gelatinous",
                           if_else(prey_group=="Small (<2mm)" | prey_group=="Large (>2mm)", "Calanoida", 
                           if_else(prey_group=="Euphausiidae Larvae", "Euphausiidae",
                           if_else(prey_group!="Decapoda" & #prey_group!="Amphipoda" &
                                   #prey_group!="Harpacticoida" &
                                   prey_group!="Appendicularia" & #prey_group!="Chaetognatha" &
                                   #prey_group!="Balanomorpha" & prey_group!="Cladocera" & prey_group!="Mollusca" 
                                   prey_group!="Bivalvia" & prey_group!="Cyclopoida", 
                                   "Other", prey_group)))))
# keep prey groups that are substantial, rest = "Other" prey category

zoop_levels <- c("Calanoida", "Euphausiidae", "Decapoda", #"Harpacticoida",
                 "Gelatinous", "Appendicularia", "Cyclopoida", "Bivalvia", "Other")
#put the taxa groups in an order that somewhat matches the diet comp later on

zoop_colors <- c("#E41A1C", "#FDAE61", "#FF7F00", #"#33A02C",
                 "#80B1D3", "#1F78B4", "navy", "#666666", "#6A3D9A")
#pink, red, Lorange, orange, green, Lblue, blue, Lpurple, grey pink purple

zoop_group_data$prey_group_simple <- factor(zoop_group_data$prey_group_simple, levels = zoop_levels)
#reorder levels to what is a nice visualization and will match the colors

# calculate relative abundance for prey groups + other for graphs:

zoop_group_sum <- zoop_group_data %>%
  group_by(site_id, prey_group_simple) %>%
  summarise(total_abd=sum(abundance))
#summarise total abundance for each sample to plot relative abd of zoops

zoop_group_wide <- zoop_group_sum %>%
  spread(prey_group_simple, (total_abd), fill=0)

zoop_group_mat <- zoop_group_wide %>% 
  ungroup() %>% 
  select(-site_id) %>%
  decostand("total")

zoop_group_percent <- zoop_group_mat*100

zoop_group_rel_abd <- zoop_group_percent %>%
  mutate(site_id=zoop_group_wide$site_id) %>% 
  gather("prey_group_simple", "total_abd", Calanoida:Other)

zoop_group_rel_abd$prey_group_simple <- factor(zoop_group_rel_abd$prey_group_simple, levels = zoop_levels)

# calculate relative abundance for full prey groups for tables:

zoop_sum <- zoop_group_data %>%
  group_by(site_id, prey_group) %>%
  summarise(total_abd=sum(abundance))
#summarise total abundance for each sample to plot relative abd of zoops

zoop_wide <- zoop_sum %>%
  spread(prey_group, (total_abd), fill=0)

zoop_mat <- zoop_wide %>% 
  ungroup() %>% 
  select(-site_id) %>%
  decostand("total")

zoop_percent <- zoop_mat*100

zoop_rel_abd <- zoop_percent %>%
  mutate(site_id=zoop_group_wide$site_id) %>% 
  gather("prey_group_simple", "total_abd", Actinopterygii:`Small (<2mm)`)

zoop_rel_abd$prey_group_simple <- factor(zoop_rel_abd$prey_group_simple, levels = prey_level_details)

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

zoop_group_rel_abd %>%
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
  labs(x="Site", y="Relative Abundance (%)", fill="Zooplankton Group")
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

kable(zoop_envr_table, "latex", booktabs=TRUE, linesep="",
      escape = FALSE, align=c("l", "l", "l", "c", "c", "c", "c", "r", "r", "r", "r")) %>%
  add_header_above(c(" "=7, "Zooplankton Biomass (mg/m³)"=4)) %>% 
  save_kable(here("tables", "spatial_tables", "sampling_table.pdf"))

# Zooplankton abundance:

# FIX THIS UP HERE ALTER >>>>> ********** !!!!!!!!!!! **********

#spat_zoop_intermediate$site_id <- factor(spat_zoop_intermediate$site_id, levels = reverse_spat_sites)
#reorder levels in zoop dataframe for tables

zoop_rel_abd_sum <- zoop_rel_abd %>%
  filter(prey_group_simple!= "Ochrophyta" & prey_group_simple!= "Parasites") %>% 
  arrange(prey_group_simple) %>% 
  group_by(site_id, prey_group_simple) %>%
  summarise(abd_group=round(sum(total_abd), digits=2))

zoop_rel_abd_sum$abd_group[which(zoop_rel_abd_sum$abd_group==0)] <- "-"

zoop_comp_table <- zoop_rel_abd_sum %>%
  spread(prey_group_simple, abd_group) %>%
  arrange(site_id, spat_site_order) %>%
  ungroup() %>% 
  #rename(`Unknown Eggs`="Unknown_egg") %>% 
  select(-c("site_id")) %>% 
  t()
# if < 1 individual / m3 for each site = group gets filtered out

colnames(zoop_comp_table) <- spat_site_order

kable(zoop_comp_table, "latex", booktabs=TRUE, escape = FALSE, linesep="") %>% 
  pack_rows("Calanoida", 2, 3, latex_gap_space = "0em") %>% 
  pack_rows("Gelatinous", 6, 7, latex_gap_space = "0em") %>% 
  pack_rows("Other", 12, nrow(zoop_comp_table), latex_gap_space = "0em") %>% 
  add_indent(c(2, 3, 6, 7, 12:nrow(zoop_comp_table))) %>% 
  save_kable(here("tables", "spatial_tables", "zoop_relA_table.pdf"))

colSums(zoop_comp_table)

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
                  #if_else(subphylum=="Chelicerata" | subphylum=="Hexapoda", phylum, #try fix outlier
                  taxa_info))))))#)
         ,
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
  mutate(prey_group=if_else(class=="Sagittoidea" | phylum=="Echinodermata" | phylum=="Ochrophyta", phylum,
                    if_else(genus=="Oikopleura" | class=="Actinopterygii" | class=="Polychaeta" | class=="Bivalvia", class,
                    if_else(order=="Decapoda" | order=="Cumacea" | order=="Isopoda"  |
                            order=="Pteropoda" | order=="Harpacticoida" | order=="Cyclopoida", order,
                    if_else(order=="Calanoida" & (size_class=="<1" | size_class=="1 to 2"), "Small (<2mm)",
                    if_else(order=="Calanoida" & (size_class=="2 to 5" | size_class=="5 to 10"), "Large (>2mm)",
                    if_else(suborder=="Balanomorpha" | suborder=="Hyperiidea" | suborder=="Senticaudata", suborder,
                    if_else(family=="Euphausiidae" & life_stage=="" | family=="Podonidae", family,
                    if_else(family=="Euphausiidae" & life_stage!="", "Euphausiidae Larvae",
                    if_else(class=="Insecta" | class=="Arachnida", class, #"Insecta/Arachnida",
                    if_else(phylum=="Cnidaria" | phylum=="Ctenophora", phylum, #"Gelatinous", #"Cnidaria_Ctenophora",
                    if_else(prey_info=="Copepoda", "Crustacea",
                    if_else(life_stage=="Object", life_stage,
                    prey_info)))))))))))))
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
  select(Actinopterygii:`Small (<2mm)`) %>% 
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
  #filter(max>8) %>%
  arrange(desc(max), prey)
#this calculation tells me which prey groups are on average >8% relative ww

spat_diet_all <- spat_diet_intermediate %>%
  mutate(prey_group_simple=if_else(prey_group=="Insecta" | prey_group=="Arachnida", "Insecta/Arachnida",
                           if_else(prey_group=="Cnidaria" | prey_group=="Ctenophora", "Gelatinous",
                           if_else(prey_group=="Senticaudata" | prey_group=="Hyperiidea", "Amphipoda",
                           if_else(prey_group=="Euphausiidae" | prey_group=="Euphausiidae Larvae", "Euphausiidae",
                           if_else(prey_group=="Small (<2mm)" | prey_group=="Large (>2mm)", "Calanoida",
                           if_else(prey_group!="Decapoda" & prey_group!="Harpacticoida" & 
                                   prey_group!="Appendicularia" & prey_group!="Chaetognatha",
                                   "Other", prey_group)))))))
# keep prey groups that are substantial, rest = "Other" prey category

prey_levels <- c("Amphipoda", "Calanoida", "Euphausiidae", "Decapoda", "Insecta/Arachnida",  
                 "Harpacticoida", "Gelatinous", "Appendicularia", "Chaetognatha", "Other")
#vector to reorder prey groups into what makes sense for diet comp bargraph

color_levels <- c("#FB9A99", "#E41A1C", "#FDAE61", "#FF7F00", "#B2DF8A",
                  "#33A02C", "#80B1D3", "#1F78B4", "#BC80BD", "#6A3D9A")
#pink, red, Lorange, orange, Lgreen, green, Lblue, blue, Lpurple, purple

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
  summarise(mean_ww=round(mean(weight), digits=1), se_ww=round(sd(weight)/10, digits=1),
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
         fishw= paste(mean_ww, se_ww, sep=" ± "),
         food= paste(mean_food, se_food, sep=" ± "),
         GFI= paste(mean_gfi, se_gfi, sep=" ± ")) %>%
  select(Species=fish_species, Site=site_id, `Fish FL (mm)`=fl, `Fish WW (g)`=fishw,
         #`Food WW (mg)`=food,
         `GFI (%BW)`=GFI, `# Empty`=n, #`% Empty Stom.`=per_empty,
         `Overlap`=overlap) %>%
  unique() %>%
  arrange(Site, c("D07", "D07", "D09", "D09", "D11", "D11", "J06", "J06", "J08", "J08", "J02", "J02"))

gfi_all_data_table$Site <- c("D07", " ", "D09", " ", "D11", " ", "J06", " ", "J08", " ", "J02", " ")

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
  select(-Crustacea) %>% 
  rename(Larvae=`Euphausiidae Larvae`, Adults=Euphausiidae) %>% 
  mutate(site=group_bio_wide$site_id, fish=group_bio_wide$fish_species) %>%
  gather(key="taxa", value="rel_bio", Actinopterygii:`Small (<2mm)`) %>% 
  group_by(site, fish, taxa) %>%
  summarise(rel_bio=round(mean(rel_bio), digits=1))
#  ungroup() %>% 
#  select(-site) %>% 

group_bio_data$taxa <- factor(group_bio_data$taxa, levels=prey_level_details)

group_bio_data$rel_bio[which(group_bio_data$rel_bio==0)] <- "-"
  
group_bio_wide <- group_bio_data %>%
  arrange(taxa) %>% 
  spread(taxa, rel_bio) %>%
  t()

group_bio_dataframe <- data.frame(group_bio_wide)

diet_table <- group_bio_dataframe[3:nrow(group_bio_dataframe), ]

#colnames(group_bio_data) <- c("D07", "D07", "D09", "D09", "D11", "D11", "J06", "J06", "J08", "J08", "J02", "J02")
#colnames(diet_table) <- c("J02", "J02", "J08", "J08", "J06", "J06", "D11", "D11", "D09", "D09", "D07", "D07")

colnames(diet_table) <- rep(c("PI", "CU"), 6)

kable(diet_table, "latex", booktabs=TRUE, linesep="") %>%
  add_header_above(c(" "=1, "J02"=2, "J08"=2, "J06"=2, "D11"=2, "D09"=2, "D07"=2)) %>%
  pack_rows("Amphipoda", 1, 2) %>% 
  pack_rows("Calanioda", 3, 4, latex_gap_space = "0em") %>% 
  pack_rows("Euphausiidae", 5, 6, latex_gap_space = "0em") %>% 
  pack_rows("Insecta/Arachnida", 8, 9, latex_gap_space = "0em") %>% 
  pack_rows("Gelatinous", 11, 12, latex_gap_space = "0em") %>% 
  pack_rows("Other", 15, nrow(diet_table), latex_gap_space = "0em") %>% 
  add_indent(c(1:6, 8, 9, 11, 12, 15:nrow(diet_table))) %>% 
  save_kable(here("tables", "spatial_tables", "diet_comp_table.pdf"))

##### SALMON GRAPHS - DIET COMP #####

diet_biomass_detail <- spatial_diets %>%
  filter(food_weight_corr!=0) %>% 
  group_by(ufn, fish_species, site_id, prey_group_simple) %>%
  summarise(prey_weight_sum=sum(prey_weight_corr))
#summarize biomass for each fish

diet_bio_wide_detail <- diet_biomass_detail %>%
  ungroup() %>%
  select(ufn, fish_species, site_id, prey_group_simple, prey_weight_sum) %>% 
  group_by(ufn, fish_species, site_id) %>% 
  spread(key=prey_group_simple, value = prey_weight_sum, fill=0)
#wide data set (might not need it, but it's a good double check that n=120!)  

diet_bio_mat_detail <- diet_bio_wide_detail %>%
  ungroup() %>% 
  select(-c(ufn, fish_species, site_id)) %>%
  decostand(method="total")

diet_bio_per_detail <- diet_bio_mat_detail*100

diet_bio_data_detail <- diet_bio_per_detail %>%
  mutate(site=diet_bio_wide_detail$site_id, fish=diet_bio_wide_detail$fish_species) %>%
  gather(key="taxa", value="rel_bio", Amphipoda:Other) %>% 
  group_by(site, fish, taxa) %>%
  summarise(rel_bio=round(mean(rel_bio), digits=1))

ggplot(diet_bio_data_detail) + 
  geom_bar(aes(x = site, y = rel_bio, fill = factor(taxa, levels = prey_levels)), 
           stat="identity", position='stack') + 
  scale_fill_manual(values=color_levels)+
  scale_y_continuous(expand = c(0,0), limits=c(0, 110))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"),
        strip.text = element_text(size=16),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size=14), axis.text = element_text(size=12),
        legend.text = element_text(size=12), legend.title = element_text(size=14))+
  facet_wrap(~fish, dir = "v")+
  labs(fill  = "Prey Group", y="Relative Biomass (%)")

ggsave(here("figs", "spatial_figs", "spatial_diet_comp.png"))

##### SALMON GRAPHS - CLUSTER #####

spat_diet_wide_detail <- spatial_diets %>%
  mutate(region=if_else(site_id=="D07" | site_id=="D09" | site_id=="D11", "DI", "JS")) %>% 
  filter(food_weight_corr!=0 & prey_info!="Crustacea" & prey_info!="Copepoda" &
           prey_info!="Parasites" & prey_info !="Detritus" & prey_info != "Coscinodiscophycidae" &
           prey_info!="Object" & prey_info!="Microplastic_chunk_Object"
  ) %>% 
  group_by(ufn, fish_species, site_id, region, prey_info) %>%
  summarise(biomass=sum(prey_weight_corr)) %>%
  spread(key=prey_info, value = biomass, fill=0) %>%
  ungroup()
#calculate wide data set with new prey groups (detailed and general!)

spat_diet_wide_detail$region <- factor(spat_diet_wide_detail$region, levels=c("JS", "DI"))

site_names <- spat_diet_wide_detail$site_id
species_names <- spat_diet_wide_detail$fish_species
region_names <- spat_diet_wide_detail$region
ufn_names <- spat_diet_wide_detail$ufn
#create dataframe with UFNs, site and species for reattaching to matrices

site_sp_names <- transmute(spat_diet_wide_detail, site_sp=paste(site_id, fish_species, sep = "_"))

spat_diet_matrix_detail <- spat_diet_wide_detail %>%
  select(Acartia:Tortanus_discaudatus) %>% 
  decostand(method="total")
#matrix to calculation relative biomass of 25 different prey groups

spat_diet_rel_bio_detail <- cbind(ufn_names, spat_diet_matrix_detail)
#combine ufn/site/species back onto relative biomass of prey groups data

#create a matrix with ufns as row names
matrix1<-as.matrix(spat_diet_rel_bio_detail)
row.names(matrix1) <- matrix1[,1]
spat_diet_matrix <- matrix1[,-1]
class(spat_diet_matrix)<-"numeric"
spat_trans_matrix <- asin(sqrt(spat_diet_matrix))
#need to rename in between matrices and dataframes better...

Bray_Curtis_Dissimilarity <- vegdist(spat_trans_matrix, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity, method = "average")
#make dendrogram data (heirarchical clustering by average linkages method)

dendr <- dendro_data(bcclust, type = "rectangle")
#put it in ggdendro form

#bcd <- as.dendrogram(cut_clust)
#put it in dendextend form...



#cut_clust <- cutree(bcclust, k=17)
#cut_clust <- as.data.frame(cut_clust)
#cut_clust <- rownames_to_column(cut_clust, var = "ufn") %>% as_tibble()
#lab_clust <- left_join(lab, cut_clust, by="ufn")
#dendr$labels <- cbind(dendr$labels, sites=as.factor(lab_clust$Site))

cut <- 17# Number of clusters
#hc <- hclust(dist(df), "ave")       # bcclust        # hierarchical clustering
#dendr <- dendro_data(hc, type = "rectangle") 
clust <- cutree(bcclust, k = cut)               # find 'cut' clusters
clust.df <- data.frame(label = names(clust), cluster = clust)
# Split dendrogram into upper grey section and lower coloured section
height <- unique(dendr$segments$y)[order(unique(dendr$segments$y), decreasing = TRUE)]
cut.height <- mean(c(height[cut], height[cut-1]))
dendr$segments$line <- ifelse(dendr$segments$y == dendr$segments$yend &
                                dendr$segments$y > cut.height, 1, 2)
dendr$segments$line <- ifelse(dendr$segments$yend  > cut.height, 1, dendr$segments$line)
# Number the clusters
dendr$segments$cluster <- c(-1, diff(dendr$segments$line))
change <- which(dendr$segments$cluster == 1)
for (i in 1:cut) dendr$segments$cluster[change[i]] = i + 1
dendr$segments$cluster <-  ifelse(dendr$segments$line == 1, 1, 
                                  ifelse(dendr$segments$cluster == 0, NA, dendr$segments$cluster))
dendr$segments$cluster <- na.locf(dendr$segments$cluster)
# Consistent numbering between segment$cluster and label$cluster
clust.df$label <- factor(clust.df$label, levels = levels(dendr$labels$label))
clust.df <- arrange(clust.df, label)
clust.df$cluster <- factor((clust.df$cluster), levels = unique(clust.df$cluster), labels = (1:cut) + 1)
dendr[["labels"]] <- merge(dendr[["labels"]], clust.df, by = "label")
# Positions for cluster labels
n.rle <- rle(dendr$segments$cluster)
N <- cumsum(n.rle$lengths)
N <- N[seq(1, length(N), 2)] + 1
N.df <- dendr$segments[N, ]
N.df$cluster <- N.df$cluster - 1

data_w_site_sp_combo <- cbind(spat_diet_wide_detail, site_sp_names)

fishsp <- data_w_site_sp_combo %>%
  ungroup() %>%
  select(ufn, Sp=fish_species, Site=site_id)

labs <- label(dendr)

colnames(labs) <- c("ufn", "x", "y", "cluster")

lab <- left_join(labs, fishsp, by = "ufn")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend,
               colour=factor(cluster)), show.legend = FALSE, size=0.5) + 
  geom_point(data=label(dendr), aes(x=x, y=y, shape=lab$Sp, fill=lab$Site), size=2)+
  geom_hline(yintercept=0.648, linetype="dashed")+
  scale_shape_manual(values=c(21,25), name="Species")+
  scale_fill_manual(values = c("#053061", "#1F78B4", "lightseagreen", "#F781BF", "#E41A1C", "darkred"), name="Site")+
  scale_color_manual(values = c(
    "#666666", "lightseagreen", "lightseagreen", "darkred", "lightseagreen",
    "#F781BF", "#1F78B4", "#053061", "#1F78B4", "#F781BF", "lightseagreen",
    "#F781BF", "#E41A1C", "#E41A1C", "#F781BF", "darkred", "#E41A1C", "darkred"
    ), guide=F)+
  guides(fill= guide_legend(override.aes = list(shape=21)),
         shape=guide_legend(override.aes=list(shape=c(19, 17)), order = 1))+
  theme_bw()+
  theme(axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.text = element_text(size=8),
        axis.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.title = element_text(size=10),
        legend.background = element_rect(color = "dark grey", fill = NA),
        panel.grid=element_blank(), legend.position = c(0.89, .83),
        legend.box = "horizontal")+
  labs(y="Dissimilarity")
#plot the dendrogram data for the different fish ID's

# legend inside !!!!!

ggsave(here("figs", "spatial_figs", "spatial_cluster.png"), width=22.75, height=15, units = "cm", dpi=600)
#cluster groups (top to bottom) DI CU; DI PI; J02 CU, J02 PI, J08 PI, J08 CU, J06 CU...
#outliers scattered amongst other clusters: D11 and J06 (lowest fullness, most empty!)

#plot(bcd, xlab="Dissimilarity", horiz = TRUE)

#simproftest <- simprof(spat_trans_matrix, method.cluster = "average", method.distance = "braycurtis", num.expected = 100, num.simulated = 99)
#simprof.plot(simproftest)
#takes a long time to run this code...

##### SALMON GRAPHS - NMDS #####

spat_diet_wide_nmds <- spatial_diets %>%
  mutate(region=if_else(site_id=="D07" | site_id=="D09" | site_id=="D11", "DI", "JS")) %>% 
  filter(food_weight_corr!=0 #& prey_info!="Crustacea" & prey_info!="Copepoda" &
         #prey_info!="Parasites" & prey_info !="Detritus" & prey_info != "Coscinodiscophycidae" &
         #prey_info!="Object" & prey_info!="Microplastic_chunk_Object"
          & ufn!="U5285" # spider
         ) %>% 
  group_by(ufn, fish_species, site_id, region, prey_info) %>%
  summarise(biomass=sum(prey_weight_corr)) %>%
  spread(key=prey_info, value = biomass, fill=0) %>%
  ungroup()
#calculate wide data set with new prey groups (detailed and general!)

spat_diet_wide_nmds$region <- factor(spat_diet_wide_nmds$region, levels=c("JS", "DI"))

site_names_nmds <- spat_diet_wide_nmds$site_id
species_names_nmds <- spat_diet_wide_nmds$fish_species
region_names_nmds <- spat_diet_wide_nmds$region
ufn_names_nmds <- spat_diet_wide_nmds$ufn
#create dataframe with UFNs, site and species for reattaching to matrices

spat_diet_matrix_nmds <- spat_diet_wide_nmds %>%
  select(Acartia:Tortanus_discaudatus) %>% 
  decostand(method="total")
#matrix to calculation relative biomass of 25 different prey groups

spat_diet_rel_bio_nmds <- cbind(ufn_names_nmds, spat_diet_matrix_nmds)
#combine ufn/site/species back onto relative biomass of prey groups data

#create a matrix with ufns as row names
matrixA<-as.matrix(spat_diet_rel_bio_nmds)
row.names(matrixA) <- matrixA[,1]
spat_nmds_matrix <- matrixA[,-1]
class(spat_nmds_matrix)<-"numeric"
spat_trans_nmds <- asin(sqrt(spat_nmds_matrix))
#need to rename in between matrices and dataframes better...

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc<- metaMDS(spat_trans_nmds,distance="bray",labels=region_names_nmds, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)

NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=region_names_nmds)
#plot NMDS, only once (picking Bray because all similar), no presence absence

ord.bc<-ordiellipse(eco.nmds.bc,region_names_nmds,display="sites",kind="sd", conf = 0.95, label=T)
#Ellipses are standard deviation, no scaling of data (can use standard error, scaling, and confidence limit options)

veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

df_ell.bc <- data.frame()
for(g in levels(NMDS.bc$group)){
  df_ell.bc <- rbind(df_ell.bc, cbind(as.data.frame(with(NMDS.bc[NMDS.bc$group==g,],
                                                         veganCovEllipse(ord.bc[[g]]$cov,ord.bc[[g]]$center))),group=g))
}

a <- ggplot(NMDS.bc, aes(NMDS1.bc, NMDS2.bc))+
  geom_point(stat = "identity", aes(shape=species_names_nmds, fill=site_names_nmds), size=3)+#, color = "black")+
  geom_path(data=df_ell.bc, aes(x=NMDS1, y=NMDS2,colour=group), size=1, linetype=2) +
  scale_shape_manual(values=c(21, 24), name="Species")+
  scale_fill_manual(values=c("#053061", "#1F78B4", "lightseagreen", 
                             "#F781BF", "#e41a1c", "darkred"
  ),
  
  name="Site", guide="legend") +
  guides(fill= guide_legend(override.aes = list(shape=21)),
  shape=guide_legend(override.aes=list(shape=c(19, 17))))+
  #shape=guide_legend(order = 1))+
  labs(x="NMDS 1", y="NMDS 2"#, title = "Diet Dissimilarity NMDS"
  )+
  scale_colour_manual(values=c("#053061", "#B2182B"), name="Region") +
  theme_bw()+
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(angle=90,size=12),
        axis.text.y=element_text(size=12),
        panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
        axis.ticks = element_blank()) + coord_fixed() +
  annotate("text",x=1.5,y=-1.7,label="(stress = 0.17)",size=4, hjust = 0)
#NMDS graph for the different sites!

a

ggsave(here("figs","spatial_figs","spatial_NMDS.png"))
