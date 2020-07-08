#updated temporal analysis code:

#last modified july 6, 2020

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
                    if_else(life_stage=="trochophore", "Polychaeta",
                    if_else(phylum=="Cnidaria" | phylum=="Ctenophora", phylum, #"Cnidaria_Ctenophora",
                    if_else(family=="Caligidae", "Parasites",
                    if_else(prey_info=="Copepoda_nauplius", "Calanoida",
                    if_else(order=="Calanoida" | order=="Decapoda" | order=="Amphipoda" |
                            order=="Harpacticoida" | order=="Cyclopoida", order,
                    if_else(infraorder=="Balanomorpha", infraorder,
                    if_else(family=="Euphausiidae" & life_stage!="egg", family,
                    if_else((family=="Euphausiidae" | genus=="Unknown") & life_stage=="egg", "Euphausiidae Eggs",
                    if_else(family=="Podonidae", "Cladocera",
                            prey_info))))))))))))
#update zooplankton groups for summary and graphs

zoop_group_data <- temp_zoop_intermediate %>%
  mutate(prey_group_simple=if_else(prey_group=="Cnidaria" | prey_group=="Ctenophora", "Gelatinous",
                           if_else(prey_group!="Calanoida" & #prey_group!="Decapoda" & 
                                   prey_group!="Euphausiidae" &# prey_group!="Amphipoda" & prey_group!="Harpacticoida" & 
                                   prey_group!="Appendicularia" & prey_group!="Euphausiidae Eggs" & #prey_group!="Chaetognatha" &
                                   prey_group!="Balanomorpha" & prey_group!="Cladocera", # & prey_group!="Mollusca" & prey_group!="Cyclopoida", 
                                   "Other",
                                   prey_group)))
# keep prey groups that are substantial, rest = "Other" prey category

zoop_levels <- c("Calanoida", #"Decapoda", 
                 "Euphausiidae", "Euphausiidae Eggs", #"Amphipoda", "Harpacticoida",
                 "Gelatinous", "Appendicularia", #"Chaetognatha",
                 #"Mollusca", "Cyclopoida",
                 "Balanomorpha", "Cladocera", "Other")
#put the taxa groups in an order that somewhat matches the diet comp later on

zoop_colors <- c("#E31A1C", "#FDBF6F", "#FF7F00", "#1B9E77",
                 #"#B2DF8A", "#33A02C", #green
                 "#A6CEE3", "#1F78B4", #"#CAB2D6", 
                 #"#E7298A", "#FB9A99", #pink
                 "#A6761D", "#E6AB02", "#6A3D9A")
#red, Lorange, orange, green, Lblue, blue, Lpurple, pink, hotpink, Y, Br, purple

zoop_group_data$prey_group_simple <- factor(zoop_group_data$prey_group_simple, levels = zoop_levels)
#reorder levels to what is a nice visualization and will match the colors

zoop_group_sum <- zoop_group_data %>%
  group_by(site_id, survey_date, year, prey_group_simple) %>%
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

# Zoop taxa composition graph:

zoop_group_sum %>%
  ggplot(aes(survey_date, total_abd))+
  geom_bar(aes(fill=prey_group_simple), width=2, stat="identity", position="fill")+
  scale_fill_manual(values=zoop_colors)+
  theme_bw()+
  facet_grid(site_id~year, scales = "free_x")+
  scale_y_continuous(labels=scales::comma)+
  theme(panel.grid=element_blank(), strip.text = element_text(size=16),
        axis.ticks.x = element_blank(), axis.text.x = element_text(color="black"),
        axis.text.y=element_text(color="black"),
        axis.title = element_text(size=14), axis.text = element_text(size=12),
        legend.text = element_text(size=12), legend.title = element_text(size=14))+
  labs(x="Site", y="Abundance (#/m³)", fill="Zooplankton Group")
#zoop comp graph 

ggsave(here("figs", "temporal_figs", "zoop_comp_temporal.png"))
#save the zoop taxa comp. graph

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

temp_data_combo <- left_join(temp_diet_raw, temp_zoop_envr, by=c("site_id", "survey_id", "survey_date", "yday", "year"))
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

temp_diet_groups <- temp_diet_sum %>%
  mutate(taxa_new=if_else(n<3 & genus!="Neotrypaea" & taxa_info!="Cancer_oregonensis",
                  if_else(life_stage=="Object" | life_stage=="Detritus", "",
                  if_else(class=="Arachnida" | class=="Insecta" | class=="Actinopterygii", class,
                  if_else(genus=="Monstrilla" | order=="Mysida", subclass,
                  if_else(genus=="Candacia" | genus=="Paraeuchaeta" | genus=="Eurytemora" | genus=="Microcalanus" |
                          genus=="Epilabidocera" | genus=="Oncaea" | order=="Harpacticoida" | genus=="Primno", order,
                  if_else(suborder=="Senticaudata" & infraorder!= "Corophiida", suborder,
                  if_else(family=="Paguridae" | infraorder=="Corophiida", infraorder,
                  if_else(family=="Pinnotheridae", family,
                  if_else(phylum=="Nematoda", "Parasite",
                  if_else(species!="", genus,
                          taxa_info))))))))),
                  if_else(phylum=="Echinodermata", phylum,
                  if_else(class=="Trematoda", "",
                  if_else(order=="Pteropoda", order, taxa_info)))),
         life_stage_new=if_else(str_detect(life_stage, "Zoea") | life_stage=="Megalopa" |
                                order=="Decapoda" & life_stage=="Juvenile", "Larvae", 
                        if_else(str_detect(life_stage, "Copepodite"), "Copepodite",
                        if_else(phylum=="Echinodermata", "Larvae",
                        if_else(prey_info=="Senticaudata_Juvenile" | prey_info=="Calanoida_Egg" |
                                class=="Actinopterygii" & life_stage!="Egg" | order=="Isopoda" | taxa_info=="Eumalacostraca", "",
                                life_stage)))),
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
#reduced number of taxa from 176 to 112, a lot more manageable now!

temp_diet_intermediate <- temp_diet_copy %>%
  filter(order!="Cumacea" & class!="Ostracoda") %>% #filter out <0.1% rel. biomass prey groups
  mutate(prey_group=if_else(class=="Sagittoidea" | phylum=="Echinodermata" | #phylum=="Mollusca" |
                            phylum=="Bryozoa" | phylum=="Ochrophyta", phylum,
                    if_else(genus=="Oikopleura" | class=="Actinopterygii" | class=="Polychaeta" | class=="Bivalvia", class,
                    if_else(order=="Calanoida" | order=="Decapoda" | order=="Mysida"
                            | order=="Amphipoda" | order=="Isopoda"# | order=="Cumacea"
                            | order=="Harpacticoida" | order=="Cyclopoida" | order=="Pteropoda", order,
                    if_else(suborder=="Balanomorpha", suborder,
                    if_else(family=="Euphausiidae" & life_stage=="", family,
                    if_else(family=="Euphausiidae" & life_stage=="Egg", "Euphausiidae Eggs",
                    if_else(family=="Euphausiidae" & (life_stage=="Furcilia" | life_stage=="Calyptopis" | life_stage=="Nauplii"), "Euphausiidae_Larvae",
                    if_else(family=="Podonidae", "Cladocera",
                    if_else(class=="Insecta" | class=="Arachnida", class, #"Insecta_Arachnida",
                    if_else(phylum=="Cnidaria" | phylum=="Ctenophora", phylum, #"Cnidaria_Ctenophora",
                    if_else(prey_info=="Copepoda", "Crustacea",
                    if_else(life_stage=="Object", life_stage,
                            prey_info)))))))))))))
#create more general prey groups for any needed tables, graphs, calc, etc.

##### SALMON DATA - FIG/TABLE PREP ##### 

temp_diet_wide <- temp_diet_intermediate %>%
  filter(food_weight_corr!=0) %>% 
  group_by(ufn, survey_date, year, fish_species, site_id, prey_group) %>%
  summarise(biomass=sum(prey_weight_corr)) %>%
  spread(key=prey_group, value = biomass, fill=0) %>%
  ungroup()
#calculate wide data set with new prey groups (detailed and general!)

temp_diet_info <-temp_diet_wide %>%
  select(ufn, fish_species, site_id, year, survey_date)
#create dataframe with UFNs, site and species for reattaching to matrices

temp_diet_matrix <- temp_diet_wide %>%
  select(Actinopterygii:Pteropoda, -c(Detritus, Digested_food_worms, Crustacea, Eumalacostraca, Parasite, Ochrophyta)) %>% 
  decostand(method="total")
#matrix to calculation relative biomass of 25 different prey groups

temp_matrix_percent <- temp_diet_matrix*100

temp_diet_rel_bio <- cbind(temp_diet_info, temp_matrix_percent)
#combine ufn/site/species back onto relative biomass of prey groups data

# Calculate average relative biomass of prey for each site/date and species:

temp_diet_rel_bio %>%
  gather(key="prey", value="rel_bio", Actinopterygii:Pteropoda) %>% 
  group_by(fish_species, prey, year, site_id) %>%
  summarise(average=mean(rel_bio)) %>%
  summarise(max=max(average)) %>%
  arrange(desc(max)) %>% 
  filter(max>8) %>%
  arrange(prey)
#this calculation tells me which prey groups are on average >4% relative ww

temp_diet_all <- temp_diet_intermediate %>%
  mutate(prey_group_simple=if_else(prey_group=="Cnidaria" | prey_group=="Ctenophora", "Gelatinous",
                           if_else(prey_group!="Calanoida" & prey_group!="Decapoda"& prey_group!="Echinodermata" & prey_group!="Cladocera" & #prey_group!="Euphausiidae" & 
                                   prey_group!="Balanomorpha" & prey_group!="Euphausiidae Eggs" & prey_group!="Appendicularia" & prey_group!="Chaetognatha",
                                   "Other",
                                   prey_group)))
# keep prey groups that are substantial, rest = "Other" prey category

prey_levels <- c("Calanoida", "Decapoda", "Cladocera", "Balanomorpha", "Echinodermata", 
                 "Euphausiidae Eggs", "Gelatinous", "Appendicularia", "Chaetognatha", "Other")
#vector to reorder prey groups into what makes sense for diet comp bargraph

color_temp <- c("#E41A1C", #"#FDB462", 
                "#FF7F00",
                "goldenrod1", #"#E6AB02", 
                "#A65628", "#999999",
                #clad yell, #barn brown, #echin grey
                "#1B9E77", #Eggs teal 
                "#80B1D3", "#1F78B4", "#BEAED4", "#984EA3")

temp_diet_all$prey_group_simple <- factor(temp_diet_all$prey_group_simple, levels = prey_levels)
#reorder taxa groups into correct order for printing graphs (and tables)

temporal_diets <- select(temp_diet_all, ufn, fish_species, site_id, survey_date, yday, year, food_weight_corr, prey_info, prey_group, prey_group_simple,
                        count, digestion_state, prey_weight_corr, length_avg, size_class, adipose, weight, fork_length, seine_id, survey_id,
                        temperature, salinity, collected, zoop_ww, set_time, time_searching, so_taken:he_total, precip:wind_direction, secchi)
#delete useless columns (can further simplify later), this=working dataset

temp_stomachs <- temporal_diets %>%
  select(ufn, fish_species, site_id, survey_date, yday, year, food_weight_corr, weight, fork_length, fork_length, adipose, seine_id, survey_id,
         temperature, salinity, collected, zoop_ww, set_time, time_searching, so_taken:he_total, precip:wind_direction, secchi) %>%
  unique()
#metadata of salmon stomachs with no prey info (envr, zoops, fish ww, etc)

# calculating relative biomass before graphing it:

temp_diet_biomass <- temp_diet_all %>%
  filter(food_weight_corr!=0) %>% 
  group_by(ufn, fish_species, site_id, survey_date, year, prey_group_simple) %>%
  summarise(bio=sum(prey_weight_corr)) %>%
  spread(key=prey_group_simple, value = bio, fill=0)
  
diet_group_biomass <- temp_diet_biomass %>%
  ungroup() %>% 
  select(Calanoida:Other) %>%
  decostand("total")

diet_group_biomass_percent <- diet_group_biomass*100

diet_group_biomass_ave <- diet_group_biomass_percent %>%
  mutate(Species=temp_diet_biomass$fish_species, Site=temp_diet_biomass$site_id,
         Date=temp_diet_biomass$survey_date, Year=temp_diet_biomass$year) %>%
  group_by(Species, Site, Date, Year, .drop = FALSE) %>% 
  gather("Prey_Group", "Biomass", Calanoida:Other) %>%
  group_by(Species, Site, Date, Year, Prey_Group) %>% 
  summarise(Ave_Rel_Bio=mean(Biomass))

diet_group_wide <- diet_group_biomass_ave %>%
  spread(key=Prey_Group, value=Ave_Rel_Bio)

diet_group_bio_graph <- diet_group_wide %>%
  transmute(Other=Other,
            Chaetognatha=Other+Chaetognatha,
            Appendicularia=Chaetognatha+Appendicularia,
            Gelatinous=Appendicularia+Gelatinous,
            `Euphausiidae Eggs`=Gelatinous+`Euphausiidae Eggs`,
            Echinodermata=`Euphausiidae Eggs`+Echinodermata,
            Balanomorpha=Echinodermata+Balanomorpha,
            Cladocera=Balanomorpha+Cladocera,
#            Euphausiidae=Balanomorpha+Euphausiidae,
            Decapoda=Cladocera+Decapoda,
            Calanoida=Decapoda+Calanoida) %>%
  gather("Prey", "Biomass", Other:Calanoida)

diet_group_biomass_ave$Prey_Group <- factor(diet_group_biomass_ave$Prey_Group, levels = prey_levels)
#reorder taxa groups into correct order for graph

barwidth=2

diet_group_biomass_ave$Date[which(diet_group_biomass_ave$Date=="2015-06-05")] <- "2015-06-04"
diet_group_biomass_ave$Date[which(diet_group_biomass_ave$Date=="2015-06-07")] <- "2015-06-04"
# merge these two groups for the sake of graphing and move back a bit to limit crowding

diet_pink_graph <- diet_group_biomass_ave %>%
  filter(Species=="Pink") %>%
  group_by(Date, Site) %>%
  arrange(Prey_Group) %>%
  mutate(pos = cumsum(Ave_Rel_Bio) - Ave_Rel_Bio / 2)

diet_chum_graph <- diet_group_biomass_ave %>%
  filter(Species=="Chum") %>%
  group_by(Date, Site) %>%
  arrange(Prey_Group) %>%
  mutate(pos = cumsum(Ave_Rel_Bio) - Ave_Rel_Bio / 2)

##### ENVR + ZOOP TABLES #####

# Sampling table:

zoop_table <- temp_zoop_ww %>%
  select(site_id, survey_date, sieve, biomass) %>%
  group_by(site_id, survey_date, sieve) %>% 
  summarise(biomass=round(sum(biomass), digits = 2)) %>% 
  spread(sieve, biomass, fill=0) %>%
  mutate(Total=sum(`250`, `1000`, `2000`))

sample_sizes <- temp_stomachs %>% 
  group_by(site_id, survey_date) %>%
  count(fish_species) %>%
  spread(key=fish_species, value=n, fill=0)

zoop_table_intermediate <- left_join(temp_envr_surface, zoop_table, by=c("site_id", "survey_date"))

zoop_envr_table <- left_join(zoop_table_intermediate, sample_sizes, by=c("site_id", "survey_date")) %>%
  mutate(`Region (Site)`=c("Discovery Islands", "(D07)", rep("", 5), "Johnstone Strait", "(J07)", rep("", 4)),
         Year=c("2015", "", "", "", "2016", "", "", "2015", "", "", "2016", "", "")) %>% 
  select(`Region (Site)`, site_id, Date=survey_date, Year, `$\\#$ Pink`=Pink, `$\\#$ Chum`=Chum,
    `Temp. (°C)`=temperature, `Salinity (‰)`=salinity,
    `250 $\\mu$m`=`250`, `1000 $\\mu$m`=`1000`, `2000 $\\mu$m`=`2000`, Total)
  
for (i in 4:ncol(zoop_envr_table)){
  zoop_envr_table[, i][which(is.na(zoop_envr_table[, i]))] <- "No Data"
}

zoop_envr_table$Date <- format(zoop_envr_table$Date, format="%B %d")

select(zoop_envr_table, -site_id) %>%  
kable("latex", booktabs=TRUE, linesep=c(rep("", 3), '\\addlinespace', "", "", "\\addlinespace", "", "", "\\addlinespace", "", ""),
      escape = FALSE, align=c("l", "l", "l", "c", "c", "c", "c", "r", "r", "r", "r")) %>%
  add_header_above(c(" "=7, "Zooplankton Biomass (mg/m³)"=4)) %>% 
  save_kable(here("tables", "temporal_tables", "sampling_table.pdf"))
# 95 pink + 117 chum = 212 salmon

# Zooplankton abundance:

zoop_comp <- temp_zoop_intermediate %>%
  group_by(site_id, survey_date, prey_group) %>%
  summarise(abd_group=round(sum(abundance), digits=1)) %>%
  spread(prey_group, abd_group, fill=0) %>%
  ungroup() %>% 
  select(site_id, Date=survey_date, everything(), -c("Actinopterygii", "Ochrophyta", "Insecta")) #%>% 
  #t()
# if < 1 individual / m3 for each site = group gets filtered out

zoop_comp$Date <- format(zoop_comp$Date, format="%B %d")

zoop_comp_intermediate <- left_join(zoop_envr_table, zoop_comp, by=c("site_id", "Date"))

for (i in 1:ncol(zoop_comp_intermediate)){
  zoop_comp_intermediate[, i][which(is.na(zoop_comp_intermediate[, i]))] <- ""
}

zoop_comp_intermediate$Amphipoda[which(zoop_comp_intermediate$Amphipoda=="")] <- "No Data"

zoop_comp_table <- select(zoop_comp_intermediate, Amphipoda:Polychaeta) %>%
  t()

colnames(zoop_comp_table) <- zoop_comp_intermediate$Date

kable(zoop_comp_table, "latex", booktabs=TRUE, escape = FALSE, align = c("r"), linesep=rep(c("", "\\addlinespace"))) %>% 
  add_header_above(c(" "=1, "2015"=4, "2016"=3, "2015"=3, "2016"=3)) %>% 
  add_header_above(c(" "=1, "Discovery Islands (D07)"=7, "Johnstone Strait (J07)"=6)) %>%
  pack_rows("Gelatinous", 8, 9) %>% 
  pack_rows("Other", 10, nrow(zoop_comp_table)) %>% 
  save_kable(here("tables", "temporal_tables", "zoop_rawA_table.pdf"))

# Need to update this to fix zoop groups and reorganize them (break up DI and JS?)


##### SALMON TABLE - PREY COMP  #####

# summary table (ave % ww by year/site/sp) for main chapter:

rel_bio_sum <- temp_diet_rel_bio %>%
  filter() %>% 
  gather(key="taxa", value="rel_bio", Actinopterygii:Pteropoda) %>% 
  group_by(site_id, year, fish_species, taxa) %>%
  summarise(ave_rel_bio=round(mean(rel_bio), digits=1)) 

rel_bio_sum$ave_rel_bio[which(rel_bio_sum$ave_rel_bio==0)] <- "-"

prey_level_details <- c("Calanoida", "Cyclopoida", "Harpacticoida", "Decapoda", "Cladocera",
                        "Balanomorpha", "Echinodermata", "Euphausiidae Eggs", "Euphausiidae_Larvae",
                        "Euphausiidae","Cnidaria", "Ctenophora", "Appendicularia", "Chaetognatha", 
                        "Actinopterygii", "Amphipoda", "Mysida", "Isopoda", "Insecta", "Arachnida",
                        "Pteropoda", "Bivalvia", "Polychaeta", "Bryozoa", "Object")

rel_bio_sum$taxa <- factor(rel_bio_sum$taxa, levels=prey_level_details)

rel_bio_chr <- rel_bio_sum %>%
  arrange(taxa) %>% 
  spread(taxa, ave_rel_bio) %>%
  rename(` `=fish_species, Larvae=Euphausiidae_Larvae, Eggs=`Euphausiidae Eggs`, Adults=Euphausiidae)

group_bio_data <- t(rel_bio_chr)

group_bio_dataframe <- data.frame(group_bio_data)

diet_table <- group_bio_dataframe[4:nrow(group_bio_dataframe), ]

colnames(diet_table) <- rep(c("Pink", "Chum"), 4)

kable(diet_table, "latex", booktabs=TRUE) %>%
  add_header_above(c(" "=1, "2015"=2, "2016"=2, "2015"=2, "2016"=2)) %>% 
  add_header_above(c(" "=1, "D07"=4, "J07"=4)) %>%
  pack_rows("Gelatinous", 11, 12) %>% 
  pack_rows("Copepoda", 1, 3) %>% 
  pack_rows("Euphausiidae", 8, 10) %>% 
  pack_rows("Other", 15, nrow(diet_table)) %>% 
  add_indent(c(1:3, 8:12, 15:nrow(diet_table))) %>% 
  save_kable(here("tables", "temporal_tables", "diet_comp_table.pdf"))

# more detailed table (ave % ww by date) for appendix:

rel_bio_sum_detail <- temp_diet_rel_bio %>%
  gather(key="taxa", value="rel_bio", Actinopterygii:Polychaeta) %>% 
  group_by(site_id, survey_date, year, fish_species, taxa) %>% 
  summarise(ave_rel_bio=round(mean(rel_bio), digits=1)) 

rel_bio_sum_detail$ave_rel_bio[which(rel_bio_sum_detail$ave_rel_bio==0)] <- "-"

rel_bio_sum_detail$taxa <- factor(rel_bio_sum_detail$taxa, levels=prey_level_details)

rel_bio_chr_detail <- rel_bio_sum_detail %>%
  arrange(taxa) %>% 
  spread(taxa, ave_rel_bio) %>%
  arrange(year) %>% 
  rename(` `=fish_species, Larvae=Euphausiidae_Larvae, Eggs=`Euphausiidae Eggs`, Adults=Euphausiidae)

group_bio_data_detail <- t(rel_bio_chr_detail)

group_bio_detail_df <- data.frame(group_bio_data_detail)

diet_table_detail <- group_bio_detail_df[5:nrow(group_bio_detail_df), ]

colnames(diet_table_detail) <- rep(c("Pink", "Chum"), 12)

kable(diet_table_detail, "latex", booktabs=TRUE, linesep="") %>%
  add_header_above(c(" "=1, "May 21"=2, "June 05"=1, "June 07"=1, "June 13"=2, "June 02"=2, "June 14"=2, "June 29"=2, 
                     "May 19"=2, "June 03"=2, "June 16"=2, "June 03"=2, "June 20"=2, "July 05"=2)) %>% 
  add_header_above(c(" "=1, "D07"=6, "J07"=6, "D07"=6, "J07"=6), bold=T) %>%
  add_header_above(c(" "=1, "2015"=12, "2016"=12), bold=T) %>% 
  pack_rows("Gelatinous", 11, 12, latex_gap_space = "0em") %>% 
  pack_rows("Copepoda", 1, 3) %>% 
  pack_rows("Euphausiidae", 8, 10, latex_gap_space = "0em") %>% 
  pack_rows("Other", 15, nrow(diet_table_detail), latex_gap_space = "0em") %>% 
  add_indent(c(1:3, 8:12, 15:nrow(diet_table_detail))) %>% 
  save_kable(here("tables", "temporal_tables", "diet_comp_table_detailed.pdf"))

##### SALMON TABLE - INDICES #####

temp_gfi_table <- temp_stomachs %>%
  filter(is.na(weight)!=TRUE) %>% 
  select(fish_species, site_id, year, weight, food_weight_corr, fork_length) %>%
  mutate(weight_corr= weight*1000, # grams to milligrams? * FIX IN RAW DATA LATER ! *
         gfi=food_weight_corr/weight_corr*100) %>% 
  group_by(fish_species, year, site_id) %>%
  summarise(mean_ww=round(mean(weight_corr), digits=1), se_ww=round(sd(weight_corr)/10, digits=1),
            mean_food=round(mean(food_weight_corr), digits=1), se_food=round(sd(food_weight_corr)/10, digits=1),
            mean_gfi=round(mean(gfi), digits=2), se_gfi=round(sd(gfi)/10, digits=2))

temp_length_table <- temp_stomachs %>%
  filter(is.na(fork_length)!=TRUE) %>%
  select(fish_species, site_id, year, fork_length) %>%
  group_by(fish_species, year, site_id) %>%
  summarise(mean_fl=round(mean(fork_length), digits=1), se_fl=round(sd(fork_length)/10, digits=1))

temp_empty_table <- temp_stomachs %>%
  filter(food_weight_corr==0) %>%
  group_by(fish_species, site_id, year) %>%
  count() %>%
  mutate(per_empty=n*10)

summed_data <- temporal_diets %>%
  filter(!prey_info %in% c("Coscinodiscophycidae", "Microplastic_chunk_Object",
                           "Object", "Parasites", "Detritus")) %>% 
  select(fish_species, site_id, year, prey_info, prey_weight_corr) %>%
  group_by(fish_species, site_id, year, prey_info) %>%
  summarise(totalw=sum(prey_weight_corr)) %>%
  spread(key=prey_info, value=totalw, fill=0) 

sites <- summed_data$site_id
salmon <- summed_data$fish_species

summed_matrix <- summed_data %>%
  ungroup() %>% 
  select(Acartia:Tortanus_discaudatus) %>% 
  decostand(method="total")

proportional_sums <- cbind(sites, salmon, summed_matrix)

# fix this overlap part later: issue of unequal sample size. ignore calculation?

#calculate_overlap <- function(dataset, site) {
#  dataset %>%
#    filter(sites==site) %>%
#    select(-c(sites, salmon)) %>%
#    summarise_all(min) %>%
#    rowSums()
#}

#D07sim <- calculate_overlap(proportional_sums, "D07")
#D09sim <- calculate_overlap(proportional_sums, "D09")
#D11sim <- calculate_overlap(proportional_sums, "D11")
#J06sim <- calculate_overlap(proportional_sums, "J06")
#J08sim <- calculate_overlap(proportional_sums, "J08")
#J02sim <- calculate_overlap(proportional_sums, "J02")

#per_overlap <- data.frame(site_id=c("J02", "J08", "J06", "D11", "D09", "D07"),
#                          overlap=c(J02sim, J08sim, J06sim, D11sim, D09sim, D07sim))

#per_overlap$site_id <- factor(per_overlap$site_id, levels = reverse_spat_sites)

#duplicateddata <- data.frame(site_id=rep(c("J02", "J08", "J06", "D11", "D09", "D07"), 2),
#                             overlap=c(round(J02sim*100, digits = 1), round(J08sim*100, digits = 1), round(J06sim*100, digits = 1), round(D11sim*100, digits = 1), "33.0", round(D07sim*100, digits = 1), "", "", "", "", "", ""))
#D09 sim = 33.00698 and rounded = 33.0 but round doesn't include zeros. so code is 33.0

# merge peroverlap, spat_empty_table (replace NAs), spat_length_table, spat_gfi_table:

gfi_fl_table <- left_join(temp_gfi_table, temp_length_table, by=c("fish_species", "site_id", "year"))

gfi_empty_table <- left_join(gfi_fl_table, temp_empty_table, by=c("fish_species", "site_id", "year"))

#gfi_overlap_table <- bind_cols(gfi_empty_table, duplicateddata)

gfi_empty_table$n[which(is.na(gfi_empty_table$n)==TRUE)] <- 0
gfi_empty_table$per_empty[which(is.na(gfi_empty_table$per_empty)==TRUE)] <- 0

#gfi_overlap_table$site_id <- factor(gfi_overlap_table$site_id, levels = reverse_spat_sites)

gfi_all_data_table <- gfi_empty_table %>%
  mutate(fl= paste(mean_fl, se_fl, sep=" ± "),
         fishw= paste(comma(mean_ww, digits=1), se_ww, sep=" ± "),
         food= paste(mean_food, se_food, sep=" ± "),
         GFI= paste(mean_gfi, se_gfi, sep=" ± ")) %>%
  select(Species=fish_species, Site=site_id, `Fish FL (mm)`=fl, `Fish WW (mg)`=fishw,
         #`Food WW (mg)`=food,
         GFI=GFI, `# Empty`=n#, #`% Empty Stom.`=per_empty,
         #`Overlap`=overlap
         ) %>%
  unique() #%>%
  #arrange(Site, c("D07", "D07", "D09", "D09", "D11", "D11", "J06", "J06", "J08", "J08", "J02", "J02"))

kable(gfi_all_data_table, "latex", booktabs=TRUE, align=c(rep("l", 4), rep("c", 3)),
      linesep= c('', '\\addlinespace')) %>% 
  save_kable(here("tables", "temporal_tables", "index_table.pdf"))


##### SALMON GRAPH - SIZE BINS #####

ggplot(temp_stomachs)+
  geom_histogram(aes(fork_length, fill=fish_species), position="dodge", binwidth = 5)+
  facet_grid(site_id~year)+
  geom_vline(aes(xintercept=87.5))+
  geom_vline(aes(xintercept=117.5))+
  scale_fill_manual(values=c("#d294af", "#516959"))+
  theme_bw()+
  theme(axis.title.y.right = element_text(color = "red"),
        panel.grid=element_blank(),
        axis.text.y.right = element_text(color="red"),
        axis.text.y.left = element_text(color="black"),
        axis.text.x = element_text(color="black"),
        strip.text = element_text(size=16),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size=14), axis.text = element_text(size=12),
        legend.text = element_text(size=12), legend.title = element_text(size=14))+
  labs(x="Fork Length (mm)", y="Count", fill="Species")

ggsave(here("figs", "temporal_figs", "salmon_size_temporal.png"))

size_summary <- temp_stomachs %>%
  mutate(fish_size=if_else(fork_length<87, "Small",
                           if_else(fork_length>117, "Large", "Medium"))) %>%
  group_by(fish_size) %>% 
  tally()


##### SALMON GRAPH - PREY COMP #####

ggplot() + 
  geom_bar(data = diet_pink_graph, 
           mapping = aes(x = Date, y = Ave_Rel_Bio, fill = factor(Prey_Group, levels = prey_levels)), 
           stat="identity", position='stack', width = barwidth) + 
  scale_x_date(date_breaks = "17 days", date_labels = "%b %d", limits = c(NULL, NULL))+ 
  geom_bar(data = filter(diet_group_biomass_ave, Species=="Chum"), 
           mapping = aes(x = Date + barwidth + 0.5, y = Ave_Rel_Bio, fill = factor(Prey_Group, levels = prey_levels)), 
           stat="identity", position='stack', width = barwidth) + 
  scale_fill_manual(values=color_temp)+
  geom_rect(data = diet_pink_graph, aes(xmin = Date - 1, xmax=Date+1, ymin = 0, ymax = 100), color="#d294af", fill=NA)+
  geom_rect(data = diet_chum_graph, aes(xmin = Date+ barwidth - 0.5, xmax=Date+ barwidth+1.5, ymin = 0, ymax = 100), color="#516959", fill=NA)+
  geom_text(data = diet_pink_graph, aes(x = Date - 0.75, y = 105), label = "PI", color="#d294af")+
  geom_text(data = diet_chum_graph, aes(x = Date + barwidth + 1, y = 105), label = "CU", color="#516959")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"),
        strip.text = element_text(size=16),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size=14), axis.text = element_text(size=12),
        legend.text = element_text(size=12), legend.title = element_text(size=14))+
  facet_grid(Site~Year, scales="free")+
  labs(fill  = "Prey Group", y="Relative Biomass (%)")

ggsave(here("figs", "temporal_figs", "temporal_diet_comp.png"))

