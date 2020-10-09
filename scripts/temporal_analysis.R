#updated temporal analysis code:

#last modified september 26, 2020

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
library(ggnewscale)
#multiple color schemes on graphs

##### ENVR + ZOOP + SALMON DATA - READ IN #####

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

# Read in main data file for salmon temporal diets:

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

##### ENVR + ZOOP DATA - TAXA REGROUP #####

# Update zoop groups for relative abundance / taxa composition graph:

temp_zoop_intermediate <- temp_zoop_data %>%
  mutate(prey_group=if_else(class=="Sagittoidea" | phylum=="Echinodermata" | phylum=="Ochrophyta" | phylum=="Bryozoa", phylum,
                    if_else(genus=="Oikopleura" | class=="Actinopterygii" | class=="Polychaeta" |
                            class=="Insecta"| class=="Bivalvia", class,
                    if_else(life_stage=="trochophore", "Polychaeta",
                    if_else(phylum=="Cnidaria" | phylum=="Ctenophora", phylum, #"Cnidaria_Ctenophora",
                    if_else(family=="Caligidae", "Parasites",
                    if_else(class=="Gastropoda", "Pteropoda",
                    if_else(prey_info=="Copepoda_nauplius", "Calanoida",
                    if_else(order=="Calanoida" | order=="Decapoda" | order=="Amphipoda" |
                            order=="Harpacticoida" | order=="Cyclopoida", order,
                    if_else(infraorder=="Balanomorpha", infraorder,
                    if_else(family=="Euphausiidae" & life_stage!="egg", "Euphausiidae Larvae",
                    if_else((family=="Euphausiidae" | genus=="Unknown") & life_stage=="egg", "Euphausiidae Eggs",
                    if_else(family=="Podonidae", "Cladocera",
                            prey_info)))))))))))))
#update zooplankton groups for summary and graphs

zoop_group_data <- temp_zoop_intermediate %>%
  mutate(prey_group_simple=if_else(prey_group=="Cnidaria" | prey_group=="Ctenophora", "Gelatinous",
                                   if_else(prey_group!="Calanoida" & prey_group!="Decapoda" & prey_group!="Echinodermata" &
                                           prey_group!="Euphausiidae" & # prey_group!="Amphipoda" & prey_group!="Harpacticoida" & 
                                           prey_group!="Appendicularia" & prey_group!="Euphausiidae Eggs" & #prey_group!="Chaetognatha" &
                                           prey_group!="Balanomorpha" & prey_group!="Cladocera", # & prey_group!="Mollusca" & prey_group!="Cyclopoida", 
                                           "Other",
                                           prey_group)))
# keep prey groups that are substantial, rest = "Other" prey category

zoop_levels <- c("Calanoida", "Decapoda", "Cladocera", "Balanomorpha", "Echinodermata", 
                 "Euphausiidae Eggs", "Gelatinous", "Appendicularia", "Other")
#put the taxa groups in an order that somewhat matches the diet comp later on

zoop_colors <- c("#E41A1C", "#FF7F00", "goldenrod1", "#A65628", "#999999",
                 "#1B9E77", "#80B1D3", "#1F78B4", "#6A3D9A")
#red, Lorange, orange, green, Lblue, blue, Lpurple, pink, hotpink, Y, Br, purple

zoop_group_data$prey_group_simple <- factor(zoop_group_data$prey_group_simple, levels = zoop_levels)
#reorder levels to what is a nice visualization and will match the colors

zoop_group_sum <- zoop_group_data %>%
  group_by(site_id, survey_date, year, prey_group_simple) %>%
  summarise(total_abd=sum(abundance))
#summarise total abundance for each sample to plot relative abd of zoops (groups with other)

zoop_group_wide <- zoop_group_sum %>%
  spread(prey_group_simple, (total_abd), fill=0)

zoop_group_mat <- zoop_group_wide %>% 
  ungroup() %>% 
  select(-site_id, -survey_date, -year) %>%
  decostand("total")

zoop_group_percent <- zoop_group_mat*100

zoop_group_rel_abd <- zoop_group_percent %>%
  mutate(site_id=zoop_group_wide$site_id, survey_date=zoop_group_wide$survey_date, year=zoop_group_wide$year) %>%
  gather("prey_group_simple", "total_abd", Calanoida:Other)

##### ENVR + ZOOP GRAPHS #####

# Graph for environmental data:

temp_envr_data %>%
  ggplot(aes(survey_date, temperature))+
  geom_line(aes(group=NA))+
  theme_bw(base_size = 12)+
  geom_line(aes(y=salinity/2, x=survey_date, group=NA), color="red")+
  scale_y_continuous(sec.axis = sec_axis(~.*2, name= "Salinity (‰)"))+
  scale_x_date(date_breaks = "12 days", date_labels = "%b %d")+ 
  facet_grid(site_id~year, scales = "free_x")+
  theme(axis.title.y.right = element_text(color = "red"),
        panel.grid=element_blank(),
        axis.text.y.right = element_text(color="red"),
        axis.text.y.left = element_text(color="black"),
        axis.text.x = element_text(color="black"),
        strip.text = element_text(size=14),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size=12), axis.text = element_text(size=10),
        legend.text = element_text(size=10), legend.title = element_text(size=14))+
  labs(y="Temperature (°C)", x=NULL)
#temperature and salinity graph combo

ggsave(here("figs", "temporal_figs", "temp_salinity_temporal.png"), width = 15, height = 15, units = "cm", dpi=800)

# Note: graphed secchi has same trend as temperature
#warm fresh DI= high secchi; cooler/saltier DI = low secchi (mixing); cold/sal JS=high

# Biomass Graph:

temp_zoop_ww %>% 
  ggplot(aes(survey_date, biomass))+
  geom_bar(aes(fill=size_frac), width=2, stat="identity")+
  theme_bw()+
  scale_fill_brewer(palette = "Dark2")+
  scale_x_date(date_breaks = "12 days", date_labels = "%b %d")+ 
  facet_grid(site_id~year, scales = "free_x")+
  scale_y_continuous(expand = c(0,0), limits=c(0, 1300))+
  geom_text(data = filter(temp_zoop_ww, site_id=="J07" & year==2016), aes(x=as.Date("2016-07-05"), y=50), label="X", size=4, color="#000000")+
  theme(panel.grid=element_blank(), strip.text = element_text(size=14),
        axis.ticks.x = element_blank(), axis.text.x = element_text(color="black"),
        axis.text.y=element_text(color="black"),
        axis.title = element_text(size=10), axis.text = element_text(size=8),
        legend.text = element_text(size=8), legend.title = element_text(size=10))+
  labs(x=NULL, y="Biomass (mg/m³)", fill="Size Fraction (μm)")
#graph of zooplankton biomass (total and by size fraction)

ggsave(here("figs", "temporal_figs", "zoop_biomass_temporal.png"), width = 16, height = 15, units = "cm", dpi=800)
#save zoop biomass graph to folder

# Zoop taxa composition graph:

zoop_group_rel_abd$prey_group_simple <- factor(zoop_group_rel_abd$prey_group_simple, levels = zoop_levels)

zoop_group_rel_abd %>%
  ggplot(aes(survey_date, total_abd))+
  geom_bar(aes(fill=prey_group_simple), width=2, stat="identity"#, position="fill"
           )+
  scale_fill_manual(values=zoop_colors)+
  geom_segment(data=filter(zoop_group_rel_abd, site_id=="D07" & year==2015), aes(x=as.Date("2015-06-06"), xend=as.Date("2015-06-06"), y=0.1, yend=99.9), size=0.5, color="#000000", inherit.aes = F)+
  theme_bw()+
  geom_text(data = filter(zoop_group_rel_abd, site_id=="J07" & year==2016), aes(x=as.Date("2016-07-05"), y=5), label="X", size=4, color="#000000")+
  facet_grid(site_id~year, scales = "free_x")+
  scale_y_continuous(expand = c(0,0), limits = c(0, 105))+
  scale_x_date(date_breaks = "12 days", date_labels = "%b %d", limits = c(NULL, NULL))+
  theme(panel.grid=element_blank(), strip.text = element_text(size=14),
        axis.ticks.x = element_blank(), axis.text.x = element_text(color="black"),
        axis.text.y=element_text(color="black"),
        axis.title = element_text(size=10), axis.text = element_text(size=8),
        legend.text = element_text(size=8), legend.title = element_text(size=10))+
  labs(x=NULL, y="Relative Abundance (%)", fill="Zooplankton Group")
#zoop comp graph 

ggsave(here("figs", "temporal_figs", "zoop_comp_temporal.png"), width = 15, height = 15, units = "cm", dpi=800)
#save the zoop taxa comp. graph

##### ENVR + ZOOP TABLES #####

# Sampling table:

zoop_table <- temp_zoop_ww %>%
  select(site_id, survey_date, sieve, biomass) %>%
  group_by(site_id, survey_date, sieve) %>% 
  summarise(biomass=round(sum(biomass), digits = 2)) %>% 
  spread(sieve, biomass, fill=0) %>%
  mutate(Total=sum(`250`, `1000`, `2000`))

diet_raw_info <- select(temp_diet_raw, ufn, fish_species, survey_date, site_id)

sample_sizes <- diet_raw_info %>% 
  group_by(ufn, site_id, survey_date) %>%
  unique() %>% 
  ungroup() %>%
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
  spread(prey_group, abd_group, fill=0)

zoop_comp_rel_abd <- zoop_comp %>%
  ungroup() %>% 
  select(-c(site_id, survey_date, Actinopterygii, Ochrophyta, Platyhelminthes, Ctenophora, Insecta)) %>%
  decostand("total") 

zoop_comp_percent <- zoop_comp_rel_abd*100

prey_level_details <- c("Calanoida", "Decapoda", "Cladocera", "Balanomorpha", "Echinodermata",
                        "Euphausiidae Eggs", "Cnidaria", "Ctenophora", "Appendicularia",
                        "Chaetognatha", "Actinopterygii", "Cyclopoida", "Harpacticoida",
                        "Euphausiidae Larvae", "Euphausiidae", "Amphipoda", "Mysida", "Isopoda",
                        "Insecta", "Arachnida", "Pteropoda", "Bivalvia", "Polychaeta", "Bryozoa", "Object")

zoop_comp_long_data <- zoop_comp_percent %>% 
  mutate(site_id=zoop_comp$site_id, Date=zoop_comp$survey_date) %>% 
  select(site_id, Date, everything()) %>%
  gather("prey", "abd", Amphipoda:Pteropoda)

zoop_comp_long_data$prey <- factor(zoop_comp_long_data$prey, levels=prey_level_details)

zoop_comp_groups <- zoop_comp_long_data %>%
  droplevels() %>% 
  arrange(prey) %>% 
  group_by(site_id, Date, prey) %>% 
  transmute(abd=round(abd, digits=1)) %>% 
  spread(prey, abd, fill="-")

zoop_comp_groups$Date <- format(zoop_comp_groups$Date, format="%B %d")

zoop_comp_intermediate <- left_join(zoop_envr_table, zoop_comp_groups, by=c("site_id", "Date"))

for (i in 1:ncol(zoop_comp_intermediate)){
  zoop_comp_intermediate[, i][which(is.na(zoop_comp_intermediate[, i]))] <- " "
  zoop_comp_intermediate[, i][which(zoop_comp_intermediate[, i]==0)] <- "-"
}

zoop_comp_intermediate$Calanoida[which(zoop_comp_intermediate$Calanoida==" ")] <- "No Data"

zoop_comp_table <- select(zoop_comp_intermediate, Calanoida:Bryozoa) %>%
  t()

colnames(zoop_comp_table) <- zoop_comp_intermediate$Date

kable(zoop_comp_table, "latex", booktabs=TRUE, escape = FALSE, align = c("r"), linesep=rep(c("", "\\addlinespace"))) %>% 
  add_header_above(c(" "=1, "2015"=4, "2016"=3, "2015"=3, "2016"=3)) %>% 
  add_header_above(c(" "=1, "Discovery Islands (D07)"=7, "Johnstone Strait (J07)"=6)) %>%
  pack_rows("Gelatinous", 7, 7) %>% 
  pack_rows("Other", 10, nrow(zoop_comp_table)) %>% 
  add_indent(c(7, 10:nrow(zoop_comp_table))) %>% 
  save_kable(here("tables", "temporal_tables", "zoop_relA_table.pdf"))

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
                    if_else(family=="Euphausiidae" & (life_stage=="Furcilia" | life_stage=="Calyptopis" | life_stage=="Nauplii"), "Euphausiidae Larvae",
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

color_temp <- c("#E41A1C", "#FF7F00", "goldenrod1", "#A65628", "#999999",
                "#1B9E77", "#80B1D3", "#1F78B4", "#BC80BD", "#6A3D9A")

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

temporalk <- temp_stomachs %>%
  mutate(k=((100000*weight)/(fork_length^3)))

##### SALMON TABLE - INDICES #####

temp_data_wide <- temporal_diets %>%
  group_by(ufn, prey_info, site_id, fish_species, survey_date, year) %>% 
  summarise(ww=sum(prey_weight_corr)) %>% 
  spread(prey_info, ww, fill=0) %>%
  ungroup()

temp_data_wide_info <- select(temp_data_wide, ufn, site_id, fish_species, survey_date, year)

temp_data_pa <- temp_data_wide %>% 
  select(Acartia:Tortanus_discaudatus) %>% 
  decostand(method = "pa")

totals <- vector(length = nrow(temp_data_pa))
#create an empty vector
totals <- rowSums(temp_data_pa)
#fill that vector with calculated row totals (total per stom.)
totals <- as.data.frame(totals)

temp_data_taxa_sum <- cbind(temp_data_wide_info, totals)

count(temp_data_taxa_sum)

temp_data_taxa_sum %>%
  group_by(site_id, fish_species, survey_date, year) %>%
  summarise(mean(totals))

temp_indices <- left_join(temporalk, temp_data_taxa_sum, by=c("ufn", "fish_species", "site_id", "year", "survey_date"))

temp_indices$year <- as.character(temp_indices$year)

temp_gfi_all_data <- temp_indices %>%
  filter(is.na(weight)!=TRUE) %>% 
  select(fish_species, site_id, survey_date, year, weight, food_weight_corr, fork_length, k, totals) %>%
  mutate(weight_corr= weight*1000, # grams to milligrams? * FIX IN RAW DATA LATER ! *
         gfi=food_weight_corr/weight_corr*100)

temp_gfi_table <- temp_gfi_all_data %>% 
  group_by(fish_species, year, site_id) %>%
  summarise(mean_ww=round(mean(weight), digits=1), se_ww=round(sd(weight), digits=1),
            mean_food=round(mean(food_weight_corr), digits=1), se_food=round(sd(food_weight_corr), digits=1),
            mean_gfi=round(mean(gfi), digits=2), se_gfi=round(sd(gfi), digits=2),
            mean_rich=round(mean(totals), digits=1), se_rich=round(sd(totals), digits=1))
#divide by 10 is supposed to be sample size ... would have to change it since n!=10 here *****

# temporary solution: use SD instead of SE ! 

temp_length_table <- temp_indices %>%
  filter(is.na(fork_length)!=TRUE) %>%
  select(fish_species, site_id, year, fork_length, k) %>%
  group_by(fish_species, year, site_id) %>%
  summarise(mean_fl=round(mean(fork_length), digits=1), se_fl=round(sd(fork_length), digits=1),
            mean_k=round(mean(na.omit(k)), digits=2), se_k=round(sd(na.omit(k)), digits=2))

temp_empty_table <- temp_indices %>%
  filter(food_weight_corr==0) %>%
  group_by(fish_species, site_id, year) %>%
  count() %>%
  mutate(per_empty=n*10)

summed_data <- temporal_diets %>%
  filter(!prey_info %in% c("Coscinodiscophycidae", "Microplastic_chunk_Object",
                           "Object", "Parasites", "Detritus")) %>% 
  select(fish_species, site_id, survey_date, year, prey_info, prey_weight_corr) %>%
  group_by(fish_species, site_id, survey_date, year, prey_info) %>%
  summarise(totalw=sum(prey_weight_corr)) %>%
  spread(key=prey_info, value=totalw, fill=0) 

summed_matrix <- summed_data %>%
  ungroup() %>% 
  select(Acartia:Tortanus_discaudatus) %>% 
  decostand(method="total")

sites <- summed_data$site_id
dates <- summed_data$survey_date
years <- summed_data$year
salmon <- summed_data$fish_species

proportional_sums <- cbind(sites, dates, years, salmon, summed_matrix)

calculate_overlap <- function(dataset, site, date) {
  dataset %>%
    filter(sites==site & dates==date) %>%
    select(-c(sites, dates, years, salmon)) %>%
    summarise_all(min) %>%
    rowSums()
}

proportional_sums$dates[which(proportional_sums$dates=="2015-06-05")] <- "2015-06-06"
proportional_sums$dates[which(proportional_sums$dates=="2015-06-05")] <- "2015-06-06"

D07simA <- calculate_overlap(proportional_sums, "D07", "2015-05-21")
D07simB <- calculate_overlap(proportional_sums, "D07", "2015-06-06")
D07simC <- calculate_overlap(proportional_sums, "D07", "2015-06-13")
D07simD <- calculate_overlap(proportional_sums, "D07", "2016-05-19")
D07simE <- calculate_overlap(proportional_sums, "D07", "2016-06-03")
D07simF <- calculate_overlap(proportional_sums, "D07", "2016-06-16")

J07simA <- calculate_overlap(proportional_sums, "J07", "2015-06-02")
J07simB <- calculate_overlap(proportional_sums, "J07", "2015-06-14")
J07simC <- calculate_overlap(proportional_sums, "J07", "2015-06-29")
J07simD <- calculate_overlap(proportional_sums, "J07", "2016-06-03")
J07simE <- calculate_overlap(proportional_sums, "J07", "2016-06-20")
J07simF <- calculate_overlap(proportional_sums, "J07", "2016-07-05")

per_overlap <- data.frame(site_id=c(rep("D07", 6), rep("J07", 6)),
                          survey_date=as.Date(c("2015-05-21", "2015-06-06", "2016-06-13",
                                        "2016-05-19", "2016-06-03", "2016-06-16",
                                        "2016-06-02", "2016-06-14", "2016-06-29",
                                        "2016-06-03", "2016-06-20", "2016-07-05")),
                          date_id=c("May 21", "June 6", "June 13", "May 19", "June 3", "June 16",
                                    "June 2", "June 14", "June 29", "June 3", "June 20", "July 5"),
                          year=c(rep("2015", 3), rep("2016", 3), rep("2015", 3), rep("2016", 3)),
                          overlap=c(D07simA, D07simB, D07simC, D07simD, D07simE, D07simF,
                                    J07simA, J07simB, J07simC, J07simD, J07simE, J07simF))

overlap_summary <- per_overlap %>%
  group_by(site_id, year) %>% 
  summarise(ave_overlap=round(mean(overlap)*100, digits=1))
# average = 61, 48, 17, 10 % overlap from DI15 -> Di16 -> js15 -> js16

#58, 66, 9, 7% when calculated all together (mean calculation is better, more representative)

dates_b_d=c("May 21", "June 6", "June 13", "May 19", "June 3", "June 16",
            "June 2", "June 14", "June 29", "June 3", "June 20", "July 5")

# this one needs updating before moving forward: (average by site/year. OR calculate for site/year.)

#duplicateddata <- data.frame(site_id=c(rep("D07", 12), rep("J07", 12)),
#                             date_id=c(rep(dates_b_d, each=2)),
#                             overlap=c(round(J02sim*100, digits = 1), "", round(J08sim*100, digits = 1), "", round(J06sim*100, digits = 1), "", round(D11sim*100, digits = 1), "", round(D07sim*100, digits = 1), "", round(D07sim*100, digits = 1), "",
#                                       round(J02sim*100, digits = 1), "", round(J08sim*100, digits = 1), "", round(J06sim*100, digits = 1), "", round(D11sim*100, digits = 1), "", round(D07sim*100, digits = 1), "", round(D07sim*100, digits = 1), ""))
#D09 sim = 33.00698 and rounded = 33.0 but round doesn't include zeros. so code is 33.0

# merge peroverlap, spat_empty_table (replace NAs), spat_length_table, spat_gfi_table:

gfi_fl_table <- left_join(temp_gfi_table, temp_length_table, by=c("fish_species", "site_id", "year"))

gfi_empty_table <- left_join(gfi_fl_table, temp_empty_table, by=c("fish_species", "site_id", "year"))

overlap_vector <- data.frame(overlap=c(overlap_summary$ave_overlap, rep("", 4)))

gfi_overlap_table <- left_join(gfi_empty_table, overlap_summary, by=c("site_id", "year"))

gfi_overlap_table$n[which(is.na(gfi_overlap_table$n)==TRUE)] <- 0
gfi_overlap_table$per_empty[which(is.na(gfi_overlap_table$per_empty)==TRUE)] <- 0
gfi_overlap_table$ave_overlap[which(gfi_overlap_table$ave_overlap==61)] <- "61.0"

gfi_overlap_table$ave_overlap <- c(unique(gfi_overlap_table$ave_overlap), rep(" ", 4))

gfi_all_data_table <- gfi_overlap_table %>%
  ungroup() %>% 
  arrange(site_id, year) %>% 
  mutate(fl= paste(mean_fl, se_fl, sep=" ± "),
         fishw= paste(mean_ww, se_ww, sep=" ± "),
         food= paste(mean_food, se_food, sep=" ± "),
         `GFI (%BW)`= paste(mean_gfi, se_gfi, sep=" ± "),
         K=paste(mean_k, se_k, sep = " ± "),
         Richness=paste(mean_rich, se_rich, sep=" ± "),
         Year=c("2015", " ", "2016", " ", "2015", " ", "2016", " "),
         #Overlap=c(unique(ave_overlap), rep(" ", 4)),
         Site=c("D07", rep(" ", 3), "J07", rep(" ", 3))) %>%
  select(Species=fish_species, Site, Year, `Fish FL (mm)`=fl, `Fish WW (g)`=fishw, `Condition (K)`=K,
         #`Food WW (mg)`=food,
         `GFI (%BW)`, `# Empty`=n, #`% Empty Stom.`=per_empty,
         Overlap=ave_overlap, Richness) %>%
  unique()

kable(gfi_all_data_table, "latex", booktabs=TRUE, align=c(rep("l", 7), rep("c", 2), "l"),
      linesep= c('', '\\addlinespace')) %>% 
  save_kable(here("tables", "temporal_tables", "index_table.pdf"))

##### SALMON TABLE - PREY COMP  #####

# summary table (ave % ww by year/site/sp) for main chapter:

rel_bio_sum <- temp_diet_rel_bio %>%
  gather(key="taxa", value="rel_bio", Actinopterygii:Pteropoda) %>% 
  group_by(site_id, year, fish_species, taxa) %>%
  summarise(ave_rel_bio=round(mean(rel_bio), digits=1)) 

rel_bio_sum$ave_rel_bio[which(rel_bio_sum$ave_rel_bio==0)] <- "-"

rel_bio_sum$taxa <- factor(rel_bio_sum$taxa, levels=prey_level_details)

rel_bio_chr <- rel_bio_sum %>%
  arrange(taxa) %>% 
  spread(taxa, ave_rel_bio) %>%
  rename(` `=fish_species)

group_bio_data <- t(rel_bio_chr)

group_bio_dataframe <- data.frame(group_bio_data)

diet_table <- group_bio_dataframe[4:nrow(group_bio_dataframe), ]

colnames(diet_table) <- rep(c("Pink", "Chum"), 4)

kable(diet_table, "latex", booktabs=TRUE) %>%
  add_header_above(c(" "=1, "2015"=2, "2016"=2, "2015"=2, "2016"=2)) %>% 
  add_header_above(c(" "=1, "D07"=4, "J07"=4)) %>%
  pack_rows("Gelatinous", 7, 8, latex_gap_space = "0em") %>% 
  pack_rows("Other", 11, nrow(diet_table), latex_gap_space = "0em") %>% 
  add_indent(c(7:8, 11:nrow(diet_table))) %>% 
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
  rename(` `=fish_species)

group_bio_data_detail <- t(rel_bio_chr_detail)

group_bio_detail_df <- data.frame(group_bio_data_detail)

diet_table_detail <- group_bio_detail_df[5:nrow(group_bio_detail_df), ]

colnames(diet_table_detail) <- rep(c("Pink", "Chum"), 12)

kable(diet_table_detail, "latex", booktabs=TRUE, linesep="") %>%
  add_header_above(c(" "=1, "May 21"=2, "June 05"=1, "June 07"=1, "June 13"=2, "June 02"=2, "June 14"=2, "June 29"=2, 
                     "May 19"=2, "June 03"=2, "June 16"=2, "June 03"=2, "June 20"=2, "July 05"=2)) %>% 
  add_header_above(c(" "=1, "Discovery Islands (D07)"=6, "Johnstone Strait (J07)"=6,
                     "Discovery Islands (D07)"=6, "Johnstone Strait (J07)"=6), bold=T) %>%
  add_header_above(c(" "=1, "2015"=12, "2016"=12), bold=T, font_size = 12) %>% 
  pack_rows("Gelatinous", 7, 8, latex_gap_space = "0em") %>% 
  pack_rows("Other", 11, nrow(diet_table_detail), latex_gap_space = "0em") %>% 
  add_indent(c(7:8, 11:nrow(diet_table_detail))) %>% 
  save_kable(here("tables", "temporal_tables", "diet_comp_table_detailed.pdf"))

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
  scale_y_continuous(expand = c(0,0), limits=c(0, 110))+
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

##### SALMON GRAPH - SIZE BINS #####

mean_fl_no_sp <- temp_stomachs %>%
  group_by(year, site_id) %>%
  filter(is.na(fork_length)!=TRUE) %>% 
  summarize(mean_no_sp=round(mean(fork_length), digits=1))

ggplot(temp_stomachs)+
  geom_histogram(aes(fork_length, fill=fish_species), position="dodge", binwidth = 5)+
  facet_grid(site_id~year)+
  geom_vline(data=mean_fl_no_sp, aes(xintercept=mean_no_sp), size=1, color="darkred", linetype="dashed")+
  scale_fill_manual(values=c("#d294af", "#516959"))+
  theme_bw()+
  scale_y_continuous(expand = c(0,0), limits = c(0, 9.5))+
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

# maybe later: divide into small and large halfs according to each sample event and species
##### SALMON GRAPH - CONDITION #####

temporalk$survey_date[which(temporalk$survey_date=="2015-06-07")] <- "2015-06-04"
#change for better plotting (so pink comes before chum)

ggplot(data=temporalk, aes(x=survey_date, y=k, fill=fish_species, group=interaction(fish_species, survey_date)))+
  geom_boxplot(data=temporalk, aes(x=survey_date, y=k, fill=fish_species), width=5)+
  labs(title=NULL, y="Fulton's K", x="Date", fill="Fish Species")+
  theme_bw()+
  geom_hline(aes(yintercept=1), color="darkred", linetype="dashed")+
  scale_x_date(date_breaks = "17 days", date_labels = "%b %d")+
  facet_grid(site_id~year, scales="free")+
  scale_fill_manual(values=c("#d294af", "#516959"))+
  theme(panel.grid=element_blank(), strip.text = element_text(size=16),
        axis.title = element_text(size=14), axis.text = element_text(size=12),
        legend.text = element_text(size=12), legend.title = element_text(size=14),
        title = element_text(size=16), plot.title = element_text(hjust=0.5))
#K for temporal

ggsave(here("figs", "temporal_figs", "temporal_condition.png"))

##### SALMON GRAPH - GFI/OVERLAP #####

#per_overlap$date_id <- as.Date(per_overlap$date_id)

temp_overlap_data <- left_join(temp_gfi_all_data, per_overlap, by=c("site_id", "survey_date", "year"))

temp_overlap_data %>% 
  ggplot(aes(survey_date, gfi, group=interaction(fish_species, survey_date)))+
  geom_boxplot(aes(fill=fish_species), width=5)+
  labs(title=NULL, y="GFI (% Body Weight)", fill="Species",
       x=NULL)+
  theme_bw()+
  geom_line(aes(y=overlap*10, x=survey_date, group=NA), color="darkred")+
  scale_y_continuous(sec.axis = sec_axis(~.*10, name="Diet Overlap (%)"))+
  scale_x_date(date_breaks = "17 days", date_labels = "%b %d")+
  scale_fill_manual(values=c("#d294af", "#516959"))+
  theme(panel.grid=element_blank(), strip.text = element_text(size=16),
        axis.title = element_text(size=14), axis.text.y = element_text(size=12),
        legend.text = element_text(size=12), legend.title = element_text(size=14),
        title = element_text(size=16), plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(size=12))+
  facet_grid(site_id~year, scales = "free_x")

ggsave(here("figs", "temporal_figs", "temporal_gfi.png"))

##### SALMON GRAPH - Niche Breadth #####

temp_data_taxa_sum %>% 
  ggplot(aes(survey_date, totals, group=interaction(fish_species, survey_date)))+
  geom_boxplot(aes(fill=fish_species), width=5)+
  labs(title=NULL, y="Number of taxanomic groups", fill="Species",
       x=NULL)+
  theme_bw()+
  scale_x_date(date_breaks = "17 days", date_labels = "%b %d")+
  scale_fill_manual(values=c("#d294af", "#516959"))+
  theme(panel.grid=element_blank(), strip.text = element_text(size=16),
        axis.title = element_text(size=14), axis.text.y = element_text(size=12),
        legend.text = element_text(size=12), legend.title = element_text(size=14),
        title = element_text(size=16), plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(size=12))+
  facet_grid(site_id~year, scales = "free_x")#+
#  scale_x_discrete(labels=c("DI_Early"="May", "DI_June_Early"="Early June", "DI_June_Mid"="Mid-June",
#                            "JS_June_Early"="Early June", "JS_June_Mid"="Mid-June", "JS_Late"="July"))
#boxplot for simple version of niche breadth (just number of taxa in each fish stomach)

ggsave(here("figs","temporal_figs","temporal_niche_breadth.png"))

##### SALMON GRAPHS - NMDS #####

temp_diet_wide_nmds <- temporal_diets %>%
  filter(food_weight_corr!=0) %>% 
  group_by(ufn, fish_species, site_id, survey_date, year, prey_info) %>%
  summarise(biomass=sum(prey_weight_corr)) %>%
  spread(key=prey_info, value = biomass, fill=0) %>%
  ungroup()
#calculate wide data set with new prey groups (detailed and general!)

temp_diet_wide_nmds$site_id <- factor(temp_diet_wide_nmds$site_id, levels=c("D07", "J07"))

site_names_nmds <- temp_diet_wide_nmds$site_id
species_names_nmds <- temp_diet_wide_nmds$fish_species
ufn_names_nmds <- temp_diet_wide_nmds$ufn
year_names_nmds <- temp_diet_wide_nmds$year
site_year_names <- paste()
#create dataframe with UFNs, site and species for reattaching to matrices

# Site + Year

# D07 2015
# D07 2016
# J07 2015
# J07 2016

# D07 - 2015
# D07 - 2016
# J07 - 2015
# J07 - 2016

# D07, 2015
# D07, 2016
# J07, 2015
# J07, 2016

# orange
# red
# purple
# blue

temp_diet_matrix_nmds <- temp_diet_wide_nmds %>%
  select(Acartia:Tortanus_discaudatus) %>% 
  decostand(method="total")
#matrix to calculation relative biomass of 25 different prey groups

temp_diet_rel_bio_nmds <- cbind(ufn_names_nmds, temp_diet_matrix_nmds)
#combine ufn/site/species back onto relative biomass of prey groups data

#create a matrix with ufns as row names
matrixA<-as.matrix(temp_diet_rel_bio_nmds)
row.names(matrixA) <- matrixA[,1]
temp_nmds_matrix <- matrixA[,-1]
class(temp_nmds_matrix)<-"numeric"
temp_trans_nmds <- asin(sqrt(temp_nmds_matrix))
#need to rename in between matrices and dataframes better...

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc<- metaMDS(temp_trans_nmds,distance="bray",labels=region_names_nmds, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)

NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=site_names_nmds)
#plot NMDS, only once (picking Bray because all similar), no presence absence

ord.bc<-ordiellipse(eco.nmds.bc,site_names_nmds,display="sites",kind="sd", conf = 0.95, label=T)
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
  geom_point(stat = "identity", aes(shape=species_names_nmds, color= year_names_nmds), fill="white", size=2, stroke = 1)+
  scale_color_manual(values=c("darkred", "#053061"), name="Site",
                     guide = guide_legend(reverse = F)) +
  new_scale_color()+
  geom_path(data=df_ell.bc, aes(x=NMDS1, y=NMDS2,colour=group), size=1, linetype=2) +
  scale_shape_manual(values=c(21, 19), name="Species")+
  guides(color= guide_legend(override.aes = list(shape=21)),
         #shape=guide_legend(override.aes=list(shape=c(19, 17)))
  #)+
  shape=guide_legend(order = 1))+
  labs(x="NMDS 1", y="NMDS 2"
  )+
  scale_colour_manual(values=c("darkred", "#053061"), name="Region"
                      #guide=NULL
                      ) +
  theme_bw()+
  theme(axis.text.x=element_text(size=10),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(angle=90,size=12),
        axis.text.y=element_text(size=10),
        panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
        axis.ticks = element_blank()) + coord_fixed() +
  annotate("text",x=1.35,y=-1.6,label="(stress = 0.15)",size=4, hjust = 0)
#NMDS graph for the different sites!

a

ggsave(here("figs","temporal_figs","temporal_NMDS.png"), width=15, height=13, units = "cm", dpi=800)
# nmds comes out slightly differently everytime unlike other graphs. save once then forget it!

#try ellipses by species next time
##### SALMON GRAPH - BIO-ENV #####

# community matrix + environmental matrix:

temp_trans_nmds

# comm same as nmds one (can be diet or dissim matrix)

#envr needs summarizing

# 
temp_envr_wide <- temporal_diets %>%
  filter(food_weight_corr!=0) %>%
  select(ufn, fish_species, site_id, survey_date, yday, year, food_weight_corr, #digestion_state, length_avg,
         weight, fork_length, seine_id, survey_id, temperature, salinity, zoop_ww, set_time, time_searching, cloud_cover, sea_state, secchi) %>%
  unique()

#fish taken ... what about sizes? how to incorp that? other stats?

##### SALMON DATA - CPUE #####

number_fish <- temporal_diets %>%
  select(survey_date, site_id, so_total, pi_total, cu_total, co_total, he_total) %>%
  unique() %>%
  mutate(salmon_total=so_total+pi_total+cu_total+co_total+he_total,
         so_per=so_total/salmon_total*100,
         pi_per=pi_total/salmon_total*100,
         cu_per=cu_total/salmon_total*100,
         co_per=co_total/salmon_total*100,
         he_per=he_total/salmon_total*100)

# do I need to do this for all 2015 and 2016 and beyond?! CPUE's? Not sure.

