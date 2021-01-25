#updated temporal analysis code:

#last modified jan 19, 2021

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
#freq occur indices
library(here)
#project oriented workflow
library(kableExtra)
library(knitr)
library(formattable)
#for creating nice tables
library(ggnewscale)
#multiple color schemes on graphs
library(ggridges)
#density plots for FL
library(cowplot)
#for putting two figs together
library(ggpubr)
#for putting *** values on figs
library(ggtext)
#for italicizing in figures

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

temp_diet_copy <- filter(temp_data_combo, !prey_info %in% c("Digested_food", "Digested_food_worms",
                                                            "Detritus", "Pellet_Detritus",
                                                            "Trematoda", "Nematoda",
                                                            "Phaeophyceae", "Coscinodiscophycidae",
                                                            "Crustacea", "Monstrilla_spinosa", "Monstrilla_helgolandica",
                                                            "Cumacea", "Nannastacidae",
                                                            "Copepoda", "Ostracoda",
                                                            "Eumalacostraca_Larvae"))
#make a copy of data before modifying the raw data (remove dig. food, detritus/phytop, parasites)
# also filter out digested crustaceans and rare taxa that can't be combined to a higher taxa level
# "Empty" taxa for empty stomachs only gets filtered when we want to exclude the empty stomachs

##### ENVR + ZOOP DATA - TAXA REGROUP #####

# Update zoop groups for relative abundance / taxa composition graph:

temp_zoop_intermediate <- temp_zoop_data %>%
  mutate(prey_group=if_else(class=="Sagittoidea" | phylum=="Echinodermata" | phylum=="Ochrophyta" | 
                              phylum=="Bryozoa", phylum,
                    if_else(#genus=="Oikopleura" | 
                              class=="Actinopterygii" | class=="Polychaeta" |
                            class=="Insecta"| class=="Bivalvia", class,
                    if_else(life_stage=="trochophore", "Polychaeta",
                    if_else(phylum=="Cnidaria" | phylum=="Ctenophora", phylum, #"Cnidaria_Ctenophora",
                    if_else(family=="Caligidae", "Parasites",
                    if_else(class=="Gastropoda", "Pteropoda",
                    if_else(prey_info=="Copepoda_nauplius", "Small (<2mm)",
                    if_else(order=="Calanoida" & (size_class=="<1" | size_class=="1 to 2"), "Small (<2mm)",
                    if_else(order=="Calanoida" & (size_class=="2 to 5" | size_class=="5 to 10"), "Large (>2mm)",
                    if_else(order=="Decapoda" | order=="Amphipoda" |
                            order=="Harpacticoida" | order=="Cyclopoida", order,
                    if_else(infraorder=="Balanomorpha", infraorder,
                    if_else(family=="Euphausiidae" & life_stage!="egg", "Euphausiidae Larvae",
                    if_else((family=="Euphausiidae" | genus=="Unknown") & life_stage=="egg", "Zooplankton Eggs",
                    if_else(family=="Podonidae", "Cladocera",
                    if_else(genus=="Oikopleura", "\\emph{Oikopleura} spp.", 
                            prey_info))))))))))))))))
#update zooplankton groups for summary and graphs

zoop_group_data <- temp_zoop_intermediate %>%
  mutate(prey_group_simple=if_else(prey_group=="Cnidaria" | prey_group=="Ctenophora", "Gelatinous",
                           if_else(prey_group=="Small (<2mm)" | prey_group=="Large (>2mm)", "Calanoida",
                           if_else(prey_group=="\\emph{Oikopleura} spp.", "*Oikopleura*",
                           if_else(prey_group!="Decapoda" & prey_group!="Echinodermata" &
                                   prey_group!="Euphausiidae" & # prey_group!="Amphipoda" & prey_group!="Harpacticoida" & 
                                   #prey_group!="\\emph{Oikopleura} spp." & 
                                   prey_group!="Zooplankton Eggs" & #prey_group!="Chaetognatha" &
                                   prey_group!="Balanomorpha" & prey_group!="Cladocera", # & prey_group!="Mollusca" & prey_group!="Cyclopoida", 
                                   "Other",
                                   prey_group)))))
# keep prey groups that are substantial, rest = "Other" prey category

zoop_levels <- c("Calanoida", "Decapoda", "Cladocera", "Balanomorpha", "Echinodermata", 
                 "Zooplankton Eggs", "Gelatinous", "*Oikopleura*", "Other")
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
        legend.text = element_text(size=8),
        legend.title = element_text(size=10)#,
        #legend.text = element_markdown(size=8)
        )+
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
        #legend.text = element_text(size=8),
        legend.text = element_markdown(size=8),
        legend.title = element_text(size=10))+
  labs(x=NULL, y="Relative Abundance (%)", fill="Zooplankton Group")
#zoop comp graph 

ggsave(here("figs", "temporal_figs", "zoop_comp_temporal.png"), width = 15, height = 15, units = "cm", dpi=800)
#save the zoop taxa comp. graph

##### ENVR + ZOOP TABLES #####

# Sampling table:

zoop_table <- temp_zoop_ww %>%
  select(site_id, survey_date, size_frac, biomass) %>%
  group_by(site_id, survey_date, size_frac) %>% 
  summarise(biomass=round(sum(biomass), digits = 1)) %>% 
  spread(size_frac, biomass, fill=0) %>%
  mutate(Total=sum(`250`, `1000`, `2000`, `2000 (Gelatinous)`))

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
  #mutate(`Region (Site)`=c("Discovery Islands", "(D07)", rep("", 5), "Johnstone Strait", "(J07)", rep("", 4)),
         mutate(`Site`=c("D07", "", rep("", 5), "J07", "", rep("", 4)),
                Year=c("2015", "", "", "", "2016", "", "", "2015", "", "", "2016", "", "")) %>% 
  select(`Site`, site_id, Date=survey_date, Year, `$\\#$ Pink`=Pink, `$\\#$ Chum`=Chum,
         `Temp. (°C)`=temperature, `Sal. (‰)`=salinity,
         `250 $\\mu$m`=`250`, `1000 $\\mu$m`=`1000`,
         `Non-Gel.`=`2000`, `Gel.`=`2000 (Gelatinous)`, Total)

for (i in 4:ncol(zoop_envr_table)){
  zoop_envr_table[, i][which(is.na(zoop_envr_table[, i]))] <- "No Data"
}

zoop_envr_table$Date <- format(zoop_envr_table$Date, format="%B %d")

select(zoop_envr_table, -site_id) %>%  
  kable("latex", booktabs=TRUE, linesep=c(rep("", 3), '\\addlinespace', "", "", "\\addlinespace", "", "", "\\addlinespace", "", ""),
        escape = FALSE, align=c("l", "l", "l", "c", "c", "c", "c", "r", "r", "r", "r")) %>%
  add_header_above(c(" "=9, "2000 $\\\\mu$m"=2, " "=1), escape = FALSE) %>% 
  add_header_above(c(" "=7, "Zooplankton Biomass (mg/m³)"=5)) %>% 
  kable_styling(latex_options = "hold_position", font_size = 8, full_width = TRUE) %>%
  #column_spec(8:11, width = "0.435in") %>% 
  column_spec(8:11, width = "0.2in") %>% 
  column_spec(2, width="0.4in") %>% 
  save_kable(here("tables", "temporal_tables", "sampling_table.pdf"))
# 95 pink + 117 chum = 212 salmon

temp_envr_surface %>%
  group_by(site_id) %>% 
#  group_by(year) %>% 
  group_by(site_id, year) %>% 
  summarise(meantemp=mean(temperature), sdtemp=sd(temperature),
            meansal=mean(salinity), sdsal=sd(salinity))

zoop_envr_table %>%
  mutate(year=c(rep("2015", 4), rep("2016", 3), rep("2015", 3), rep("2016", 3))) %>% 
  filter(Total!="No Data") %>%
  group_by(year) %>%
  summarise(avebio=mean(as.numeric(Total)),
            sdbio=sd(as.numeric(Total)))

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

prey_level_details <- c("Actinopterygii", "Small (<2mm)", "Large (>2mm)", "Cladocera", "Decapoda", 
                        "Balanomorpha", "Balanomorpha_Nauplii", "Nauplii", "Balanomorpha_Cyprid", "Cyprids", "Adults/Molts", "Balanomorpha_Juvenile", "Balanomorpha_", "Balanomorpha_Molt", 
                        "Echinodermata", "Zooplankton Eggs",  "Eggs", "Larvae", "Adults", "Cnidaria", 
                        "Ctenophora", "Appendicularia", "\\emph{Oikopleura} spp.", "\\emph{Oikopleura}", "*Oikopleura*",
                        "Chaetognatha", "Cyclopoida", "Harpacticoida", "Euphausiidae", "Amphipoda", "Mysida", "Isopoda",
                        "Insecta", "Arachnida", "Pteropoda", "Bivalvia", "Polychaeta", "Bryozoa", "Object")

zoop_comp_long_data <- zoop_comp_percent %>% 
  mutate(site_id=zoop_comp$site_id, Date=zoop_comp$survey_date) %>% 
  select(site_id, Date, everything()) %>%
  rename(Euphausiidae=`Euphausiidae Larvae`) %>% 
  gather("prey", "abd", `\\emph{Oikopleura} spp.`:`Zooplankton Eggs`)

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

zoop_comp_intermediate$`Small (<2mm)`[which(zoop_comp_intermediate$`Small (<2mm)`==" ")] <- "No"
zoop_comp_intermediate$`Large (>2mm)`[which(zoop_comp_intermediate$`Large (>2mm)`==" ")] <- "Data"

zoop_comp_table <- zoop_comp_intermediate %>% 
  select(`Small (<2mm)`:Bryozoa) %>%
  t()

colnames(zoop_comp_table) <- zoop_comp_intermediate$Date

colnames(zoop_comp_table) <-c("21", "05", "07", "13", "19", "03", "16",
                              "02", "14", "29", "03", "20", "05")

kable(zoop_comp_table, "latex", booktabs=TRUE, escape = FALSE, align = c("r"), 
      linesep=rep(c("", "\\addlinespace"))) %>%
  add_header_above(c(" ", "May"=1, "June"=3, "May"=1, "June"=2, "June"=3, "June"=2, "July"=1)) %>% 
  add_header_above(c(" "=1, "2015"=4, "2016"=3, "2015"=3, "2016"=3)) %>% 
  add_header_above(c(" "=1, "Discovery Islands (D07)"=7, "Johnstone Strait (J07)"=6)) %>%
  kable_styling(latex_options = "hold_position", 
                font_size = 8, full_width = TRUE) %>%
  column_spec(2:13, width = "0.1in") %>% 
  column_spec(14, width="0.15in") %>%
  column_spec(1, width="0.99in") %>% 
  pack_rows("Calanoida", 1, 2) %>% 
  pack_rows("Gelatinous", 8, 8) %>% 
  pack_rows("Other", 11, nrow(zoop_comp_table)) %>% 
  add_indent(c(1, 2, 8, 11:nrow(zoop_comp_table))) %>% 
  save_kable(here("tables", "temporal_tables", "zoop_relA_table.pdf"))

##### SALMON DATA - TAXA REGROUP #####

# Merge rare (< 3 stom.) taxonomic groups to higher prey levels:

temp_diet_data <- temp_diet_copy %>%
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
  mutate(taxa_new=if_else(n<3 & genus!="Neotrypaea" & taxa_info!="Cancer_oregonensis" & genus!="Alienacanthomysis" & genus!="Ammodytes",
                  if_else(life_stage=="Object", #| life_stage=="Detritus", 
                          "",
                  if_else(class=="Arachnida" | class=="Insecta", #| class=="Actinopterygii",
                          class,
                  if_else(genus=="Monstrilla", # | order=="Mysida", 
                          subclass,
                  if_else(genus=="Candacia" | genus=="Paraeuchaeta" | genus=="Eurytemora" | genus=="Microcalanus" |
                          genus=="Epilabidocera" | genus=="Oncaea" | order=="Harpacticoida" | genus=="Primno", order,
                  if_else(suborder=="Senticaudata" & infraorder!= "Corophiida", suborder,
                  if_else(family=="Paguridae" | infraorder=="Corophiida", infraorder,
                  if_else(family=="Pinnotheridae", family,
                  #if_else(phylum=="Nematoda", "Parasite",
                  if_else(order=="Pteropoda", "Limacina_helicina", # double checked data for IDs!
                  if_else(species!="", genus,
                          taxa_info))))))))),
                  if_else(phylum=="Echinodermata", phylum,
                  #if_else(class=="Trematoda", "Parasite",
                          taxa_info)),
         life_stage_new=if_else(str_detect(life_stage, "Zoea") | life_stage=="Megalopa" |
                                order=="Decapoda" & life_stage=="Juvenile", "Larvae", 
                        if_else(str_detect(life_stage, "Copepodite"), "Copepodite",
                        if_else(phylum=="Echinodermata", "Larvae",
                        if_else(prey_info=="Senticaudata_Juvenile" | prey_info=="Calanoida_Egg" |
                                #class=="Actinopterygii" & life_stage!="Egg" |
                                  order=="Isopoda", #| taxa_info=="Eumalacostraca",
                                "",
                                life_stage)))),
         prey_new=if_else(life_stage_new=="", taxa_new,
                  if_else(taxa_new=="", life_stage_new, 
                          paste(taxa_new, life_stage_new, sep="_")))) %>%
  ungroup %>% 
  select(prey_info, prey_new)
#update any prey groups that occur in less than three fish stomachs!
# EXCEPT FOR MYSIDS because they're in two stomachs but with > 20% rel bio! (therefore important)
# ^ nvm, that info is still retained in the group summaries (confusing but whatever I guess...)

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
  filter(order!="Cumacea" & class!="Ostracoda") %>% #filt211er out <0.1% rel. biomass prey groups
  mutate(prey_group=if_else(class=="Sagittoidea" | phylum=="Echinodermata" | #phylum=="Mollusca" |
                            phylum=="Bryozoa", # | phylum=="Ochrophyta", 
                            phylum,
                    if_else(#genus=="Oikopleura" |
                              class=="Actinopterygii" | class=="Polychaeta" | class=="Bivalvia", class,
                    if_else(order=="Calanoida" & (size_class=="<1" | size_class=="1 to 2"), "Small (<2mm)",
                    if_else(order=="Calanoida" & (size_class=="2 to 5" | size_class=="5 to 10"), "Large (>2mm)",
                    if_else(order=="Decapoda" | order=="Mysida"
                            | order=="Amphipoda" | order=="Isopoda"# | order=="Cumacea"
                            | order=="Harpacticoida" | order=="Cyclopoida" | order=="Pteropoda", order,
                    if_else(suborder=="Balanomorpha", paste(suborder, life_stage, sep="_"),
                    if_else(family=="Euphausiidae" & life_stage=="", "Adults",
                    if_else(family=="Euphausiidae" & life_stage=="Egg", "Eggs",
                    if_else(family=="Euphausiidae" & (life_stage=="Furcilia" | life_stage=="Calyptopis" | life_stage=="Nauplii"), "Larvae",
                    if_else(family=="Podonidae", "Cladocera",
                    if_else(class=="Insecta" | class=="Arachnida", class, #"Insecta_Arachnida",
                    if_else(phylum=="Cnidaria" | phylum=="Ctenophora", phylum, #"Cnidaria_Ctenophora",
                    if_else(prey_info=="Copepoda", "Crustacea",
                    if_else(life_stage=="Object", life_stage,
                    if_else(genus=="Oikopleura", "\\emph{Oikopleura} spp.",
                            prey_info))))))))))))))))
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
  select(`\\emph{Oikopleura} spp.`:`Small (<2mm)`
  #select(Actinopterygii:`Small (<2mm)`#, -c(Detritus, Digested_food_worms, Crustacea,
                                       #    Eumalacostraca_Larvae, Parasite, Ochrophyta)
         ) %>% 
  decostand(method="total")
#matrix to calculation relative biomass of 25 different prey groups

temp_matrix_percent <- temp_diet_matrix*100

temp_diet_rel_bio <- cbind(temp_diet_info, temp_matrix_percent)
#combine ufn/site/species back onto relative biomass of prey groups data

# Calculate average relative biomass of prey for each site/date and species:

temp_diet_rel_bio %>%
  gather(key="prey", value="rel_bio", `\\emph{Oikopleura} spp.`:`Small (<2mm)`) %>% 
  group_by(fish_species, prey, year, site_id) %>%
  summarise(average=mean(rel_bio)) %>%
  summarise(max=max(average)) %>%
  arrange(desc(max)) %>% 
  filter(max>8) %>%
  arrange(prey)
#this calculation tells me which prey groups are on average >4% relative ww

temp_diet_all <- temp_diet_intermediate %>%
  mutate(prey_group_simple=if_else(prey_group=="Cnidaria" | prey_group=="Ctenophora", "Gelatinous",
                           if_else(prey_group=="Small (<2mm)" | prey_group=="Large (>2mm)", "Calanoida",
                           if_else(prey_group=="Eggs" | prey_group=="Larvae" | prey_group=="Adults", "Euphausiidae",
                           if_else(prey_group=="\\emph{Oikopleura} spp.", "*Oikopleura*",
                           if_else(prey_group=="Balanomorpha_Cyprid" | prey_group=="Balanomorpha_Nauplii" | prey_group=="Balanomorpha_"| prey_group=="Balanomorpha_Molt" | prey_group=="Balanomorpha_Juvenile", "Balanomorpha",
                           if_else(prey_group!="Decapoda"& prey_group!="Echinodermata" & prey_group!="Cladocera" &
                                   #prey_group!="Balanomorpha" & 
                                   #prey_group!="Appendicularia" &
                                   #prey_group!="\\emph{Oikopleura} spp." &
                                   prey_group!="Chaetognatha" & prey_group!="Actinopterygii",
                                   "Other",
                                   prey_group)))))))
# keep prey groups that are substantial, rest = "Other" prey category

prey_levels <- c("Actinopterygii", "Calanoida", "Cladocera", "Decapoda", "Balanomorpha", 
                 "Echinodermata", "Euphausiidae", "Gelatinous", "*Oikopleura*", "Chaetognatha", "Other")
#vector to reorder prey groups into what makes sense for diet comp bargraph

color_temp <- c("#333333", "#E41A1C", "goldenrod1", "#FF7F00", #"#b35806", "#7f3b08",
                 "#A65628",
                "#999999", "#1B9E77", "#80B1D3", "#1F78B4", "#BC80BD", "#6A3D9A")

temp_diet_all$prey_group_simple <- factor(temp_diet_all$prey_group_simple, levels = prey_levels)
#reorder taxa groups into correct order for printing graphs (and tables)

temporal_diets <- select(temp_diet_all, ufn, fish_species, site_id, survey_date, yday, year, week, food_weight_corr, prey_info, prey_group, prey_group_simple,
                        family, count, digestion_state, prey_weight_corr, length_avg, size_class, adipose, weight, fork_length, seine_id, survey_id,
                        temperature, salinity, collected, zoop_ww, set_time, time_searching, so_taken:he_total, precip:wind_direction, secchi)
#delete useless columns (can further simplify later), this=working dataset

temp_stomachs <- temporal_diets %>%
  select(ufn, fish_species, site_id, survey_date, yday, year, week, food_weight_corr, weight, fork_length, fork_length, adipose, seine_id, survey_id,
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
  #select(Calanoida:Other) %>%
  select(Actinopterygii:Other) %>%
  decostand("total")

diet_group_biomass_percent <- diet_group_biomass*100

diet_group_biomass_ave <- diet_group_biomass_percent %>%
  mutate(Species=temp_diet_biomass$fish_species, Site=temp_diet_biomass$site_id,
         Date=temp_diet_biomass$survey_date, Year=temp_diet_biomass$year) %>%
  group_by(Species, Site, Date, Year, .drop = FALSE) %>% 
  #gather("Prey_Group", "Biomass", Calanoida:Other) %>%
  gather("Prey_Group", "Biomass", Actinopterygii:Other) %>%
  group_by(Species, Site, Date, Year, Prey_Group) %>% 
  summarise(Ave_Rel_Bio=mean(Biomass))

diet_group_wide <- diet_group_biomass_ave %>%
  spread(key=Prey_Group, value=Ave_Rel_Bio)

#diet_group_bio_graph <- diet_group_wide %>%
#  transmute(Other=Other,
#            Chaetognatha=Other+Chaetognatha,
#            Appendicularia=Chaetognatha+Appendicularia,
#            Gelatinous=Appendicularia+Gelatinous,
#            Actinopterygii=Gelatinous+Actinopterygii,
#            Echinodermata=Actinopterygii+Echinodermata,
#            Balanomorpha=Echinodermata+Balanomorpha,
#            Cladocera=Balanomorpha+Cladocera,
#            Decapoda=Cladocera+Decapoda,
#            Euphausiidae=Decapoda+Euphausiidae,
#            Calanoida=Euphausiidae+Calanoida) %>%
#  gather("Prey", "Biomass", Other:Calanoida)

diet_group_bio_graph <- diet_group_wide %>%
  transmute(Other=Other,
            Chaetognatha=Other+Chaetognatha,
            `*Oikopleura*`=Chaetognatha+`*Oikopleura*`,
            Gelatinous=`*Oikopleura*`+Gelatinous,
            Euphausiidae=Gelatinous+Euphausiidae,
            Echinodermata=Euphausiidae+Echinodermata,
            Balanomorpha=Echinodermata+Balanomorpha,
            Decapoda=Balanomorpha+Decapoda,
            Cladocera=Decapoda+Cladocera,
            Calanoida=Cladocera+Calanoida,
            Actinopterygii=Calanoida+Actinopterygii) %>%
  gather("Prey", "Biomass", Other:Actinopterygii)

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

temp_gfi_summary_dates <- temp_gfi_all_data %>%
  group_by(fish_species, year, site_id, survey_date) %>%
  summarise(mean_ww=round(mean(weight), digits=1), se_ww=round(sd(weight), digits=1),
  mean_food=round(mean(food_weight_corr), digits=1), se_food=round(sd(food_weight_corr), digits=1),
  mean_gfi=round(mean(gfi), digits=2), se_gfi=round(sd(gfi), digits=2),
  mean_rich=round(mean(totals), digits=1), se_rich=round(sd(totals), digits=1))

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

temp_empty_summary <- temp_indices %>%
  filter(food_weight_corr==0) %>%
  group_by(fish_species, site_id, survey_date, year) %>%
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
proportional_sums$dates[which(proportional_sums$dates=="2015-06-07")] <- "2015-06-06"

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
                          survey_date=as.Date(c("2015-05-21", "2015-06-06", "2015-06-13",
                                        "2016-05-19", "2016-06-03", "2016-06-16",
                                        "2015-06-02", "2015-06-14", "2015-06-29",
                                        "2016-06-03", "2016-06-20", "2016-07-05")),
                          date_id=c("May 21", "June 6", "June 13", "May 19", "June 3", "June 16",
                                    "June 2", "June 14", "June 29", "June 3", "June 20", "July 5"),
                          year=c(rep("2015", 3), rep("2016", 3), rep("2015", 3), rep("2016", 3)),
                          overlap=c(D07simA, D07simB, D07simC, D07simD, D07simE, D07simF,
                                    J07simA, J07simB, J07simC, J07simD, J07simE, J07simF))

no_double_date <- per_overlap %>%
  filter(survey_date!="2015-06-06")
  
original_dates <- data.frame(
  site_id=c("D07", "D07"),
  survey_date=as.Date(c("2015-06-05", "2015-06-07")),
  date_id=c("June 5", "June 7"),
  year=c("2015", "2015"),
  overlap=c(D07simB, D07simB))

percent_overlap <- rbind(original_dates, no_double_date)

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
         #Richness=paste(mean_rich, se_rich, sep=" ± "),
         Year=c("2015", " ", "2016", " ", "2015", " ", "2016", " "),
         #Overlap=c(unique(ave_overlap), rep(" ", 4)),
         Site=c("D07", rep(" ", 3), "J07", rep(" ", 3))) %>%
  select(Species=fish_species, Site, Year, `Fish FL (mm)`=fl, `Fish WW (g)`=fishw, `Condition (K)`=K,
         #`Food WW (mg)`=food,
         `GFI (%BW)`, `# Empty`=n, #`% Empty Stom.`=per_empty,
         Overlap=ave_overlap#, Richness
         ) %>%
  unique()

kable(gfi_all_data_table, "latex", booktabs=TRUE, align=c(rep("l", 7), rep("c", 2), "l"),
      linesep= c('', '\\addlinespace')) %>% 
  save_kable(here("tables", "temporal_tables", "index_table.pdf"))

##### SALMON TABLE - PREY COMP  #####

# summary table (ave % ww by year/site/sp) for main chapter:

rel_bio_sum <- temp_diet_rel_bio %>%
  mutate(`Adults/Molts`=Balanomorpha_+Balanomorpha_Molt+Balanomorpha_Juvenile) %>% 
  select(-c(Balanomorpha_, Balanomorpha_Juvenile, Balanomorpha_Molt)) %>% 
  rename(Nauplii=Balanomorpha_Nauplii, Cyprids=Balanomorpha_Cyprid) %>% 
  gather(key="taxa", value="rel_bio", `\\emph{Oikopleura} spp.`:`Adults/Molts`) %>% 
  group_by(site_id, year, fish_species, taxa) %>%
  summarise(ave_rel_bio=round(mean(rel_bio), digits=1)) 

rel_bio_sum$ave_rel_bio[which(rel_bio_sum$ave_rel_bio==0)] <- "-"

rel_bio_sum$taxa <- factor(rel_bio_sum$taxa, levels=prey_level_details)
# THIS IS WHERE I USUALLY ENCOUNTER PROBLEMS WHEN UPDATING TAXA * 

rel_bio_chr <- rel_bio_sum %>%
  arrange(taxa) %>% 
  spread(taxa, ave_rel_bio) %>%
  rename(` `=fish_species)

group_bio_data <- t(rel_bio_chr)

group_bio_dataframe <- data.frame(group_bio_data)

diet_table <- group_bio_dataframe[4:nrow(group_bio_dataframe), ]

colnames(diet_table) <- rep(c("Pink", "Chum"), 4)

kable(diet_table, "latex", booktabs=TRUE, escape = F) %>%
  kable_styling(latex_options = "hold_position", font_size = 8, full_width = TRUE) %>% 
  column_spec(1, "1in") %>% 
  add_header_above(c(" "=1, "2015"=2, "2016"=2, "2015"=2, "2016"=2)) %>% 
  add_header_above(c(" "=1, "D07"=4, "J07"=4)) %>%
  pack_rows("Calanoida", 2, 3) %>%
  pack_rows("Balanomorpha", 6, 8, latex_gap_space = "0em") %>% 
  pack_rows("Euphausiidae", 10, 12, latex_gap_space = "0em") %>% 
  pack_rows("Gelatinous", 13, 14, latex_gap_space = "0em") %>% 
  pack_rows("Other", 17, nrow(diet_table), latex_gap_space = "0em") %>% 
  add_indent(c(2:3, 6:8, 10:14, 17:nrow(diet_table))) %>% 
  save_kable(here("tables", "temporal_tables", "diet_comp_table.pdf"))

# more detailed table (ave % ww by date) for appendix:

rel_bio_sum_detail <- temp_diet_rel_bio %>%
  mutate(`Adults/Molts`=Balanomorpha_+Balanomorpha_Molt+Balanomorpha_Juvenile) %>% 
  select(-c(Balanomorpha_, Balanomorpha_Juvenile, Balanomorpha_Molt)) %>% 
  rename(Nauplii=Balanomorpha_Nauplii, Cyprids=Balanomorpha_Cyprid) %>% 
  gather(key="taxa", value="rel_bio", `\\emph{Oikopleura} spp.`:`Adults/Molts`) %>% 
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

colnames(diet_table_detail) <- rep(c("PI", "CU"), 12)

kable(diet_table_detail, "latex", booktabs=TRUE, linesep="", escape = F) %>%
  add_header_above(c(" "=1, "May 21"=2, "June 05"=1, "June 07"=1, "June 13"=2, "June 02"=2, "June 14"=2, "June 29"=2, 
                     "May 19"=2, "June 03"=2, "June 16"=2, "June 03"=2, "June 20"=2, "July 05"=2)) %>% 
  add_header_above(c(" "=1, "Discovery Islands (D07)"=6, "Johnstone Strait (J07)"=6,
                     "Discovery Islands (D07)"=6, "Johnstone Strait (J07)"=6), bold=T) %>%
  add_header_above(c(" "=1, "2015"=12, "2016"=12), bold=T, font_size = 12) %>% 
  pack_rows("Calanoida", 2, 3) %>% 
  pack_rows("Balanomorpha", 6, 8, latex_gap_space = "0em") %>% 
  pack_rows("Euphausiidae", 10, 12, latex_gap_space = "0em") %>% 
  pack_rows("Gelatinous", 13, 14, latex_gap_space = "0em") %>% 
  pack_rows("Other", 17, nrow(diet_table_detail), latex_gap_space = "0em") %>% 
  add_indent(c(2:3, 6:8, 10:14, 17:nrow(diet_table_detail))) %>% 
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
  #      legend.text = element_text(size=12),
        legend.text = element_markdown(size=12),
        legend.title = element_text(size=14))+
  facet_grid(Site~Year, scales="free")+
  labs(fill  = "Prey Group", y="Relative Biomass (%)")

ggsave(here("figs", "temporal_figs", "temporal_diet_comp.png"))#, width=22, height=14, units = "cm", dpi=800)

diet_groups_wide <- cbind(select(temp_diet_biomass, ufn, fish_species, survey_date), diet_group_biomass_percent)

diet_graph_detail <- diet_groups_wide %>%
  filter(survey_date=="2015-06-13")  %>%
  gather("Prey_Group", "Ave_Rel_Bio", Actinopterygii:Other) %>%
  group_by(fish_species) %>%
  arrange(Prey_Group) %>%
  mutate(pos = cumsum(Ave_Rel_Bio) - Ave_Rel_Bio / 2)

diet_graph_detail %>% 
  ggplot() + 
  geom_bar(mapping = aes(x = ufn, y = Ave_Rel_Bio, fill = factor(Prey_Group, levels = prey_levels)), 
           stat="identity", position='stack', width = barwidth) + 
  scale_fill_manual(values=color_temp)+
  facet_grid(fish_species~year, scales="free")+
  labs(fill  = "Prey Group", y="Relative Biomass (%)")

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

temp_stomachs %>%
  filter(is.na(fork_length)!=TRUE) %>% 
  group_by(fish_species, site_id, year, week) %>%
  summarise(ave_fl=mean(fork_length)) %>%
  ggplot(aes(week, ave_fl), group=interaction(fish_species, site_id))+
  geom_bar(aes(fill=fish_species), stat="identity", position="dodge")+
  scale_fill_manual(values=c("#516959", "#d294af"))+
  facet_wrap(year~site_id, nrow=2)

#figure out how to compare with average values for DI/JS 2015-2019 sizes... *

# maybe later: divide into small and large halfs according to each sample event and species
##### SALMON GRAPH - CONDITION #####

temporalk

library(ggpubr)
library(rstatix)

k_df <- temporalk %>%
  filter(is.na(k)!=TRUE)

k_df %>% 
  group_by(year, site_id, fish_species) %>%
  get_summary_stats(k)

outlier_vals <- k_df %>%
  select(ufn, year, site_id, fish_species, k) %>% 
  group_by(year, site_id, fish_species) %>%
  identify_outliers(k)

no_outliers <- anti_join(k_df, outlier_vals)

model  <- lm(k ~ year*site_id*fish_species, data = filter(no_outliers, year=="2015" | year=="2016"))
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))

k_df %>%
  group_by(year, site_id, fish_species) %>%
  shapiro_test(k) 
#pink d07 2016 sig = non normal (but rest are good!)

ggqqplot(k_df, "k", ggtheme = theme_bw()) +
  facet_grid(year + site_id ~ fish_species, labeller = "label_both")

k_df$year <- as.character(k_df$year)

k_trans <- k_df %>%
  mutate(logk=log(k))

k_trans <- k_df %>%
  mutate(reck=1/k)

k_trans %>% levene_test(reck ~ year*site_id*fish_species)

#k_df %>% levene_test(k ~ year*site_id)

# do NOT have homogeneity of variance... hmm. does this mean I can't do it?
# try log transforming the data?? then check assumptions then do the ANOVA.
# log transforming doesn't help (still sig), but getting rid of sp. helps!
# reciprocal worked tho!

res.aov <- k_trans %>%
  group_by(fish_species) %>%
  anova_test(reck ~ year*site_id)
res.aov # year is sig; sp is sig; year AND sp is sig. region is not sig in any regard!
summary(res.aov)
# reread brian's comment. he said two way anova for EACH SP... ONLY YEAR = SIG (BOTH SP.)

# i understand 3 way better now tho:
res.aov <- k_trans %>% anova_test(reck ~ year*site_id*fish_species)
res.aov # year is sig; sp is sig; year AND sp is sig. region is not sig in any regard!
summary(res.aov)

model  <- lm(reck ~ year*site_id*fish_species, data = k_trans)
k_trans %>%
  group_by(year) %>%
  anova_test(reck ~ fish_species, error = model)
# when grouping by year, species is significant in 2016 only!
# when grouping by species, year is significant for both sp.
# which do I use then?? does it matter?? depends on my question??
# use both with bonferroni correction? pvalue cut off is now 0.025?
# if that's the case, then all the significances are the same???
# if it's by each table n=2, not each calculation... I need advice
# these analyses are saying 2015>2016 for both species and ...
# chum is significantly higher than pink (in 2016 only) ! cool.

model  <- lm(reck ~ year*site_id*fish_species, data = k_trans)
k_trans %>%
  group_by(fish_species) %>%
  anova_test(reck ~ year, error = model)
# do i have a sig two way interaction?

bxp <- ggboxplot(
  k_df, x = "fish_species", y = "k", 
  color = "year", palette = "jco"#, facet.by = "fish_species"
)
bxp

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

#temporalk$year <- as.character(temporalk$year)

temporalksig <- mutate(temporalk, 
                       sigs=
                         if_else(year==2015, "a", 
                                 if_else(fish_species=="Pink", "b", "c")),
                       reg_labels=if_else(
                         year==2015 & fish_species=="Pink", "A",
                        if_else(year==2015 & fish_species=="Chum", "C",
                                if_else(fish_species=="Pink", "B", "D")
                                )))

temporalksig$reg_labels <- factor(temporalksig$reg_labels)

my_comparisons <- list(c("A", "B"), c("C", "D"), c("B", "D"))

ggplot(data=temporalksig, aes(x=reg_labels, y=k, fill=fish_species, group=interaction(fish_species, year)))+
  geom_boxplot(data=temporalksig, aes(x=reg_labels, y=k, fill=fish_species)#, width=5
               )+
  labs(title=NULL, y="Fulton's K", x = NULL, fill="Fish Species")+
  theme_bw()+
  geom_hline(aes(yintercept=1), color="darkred", linetype="dashed")+
  scale_x_discrete(labels=c("2015", "2016", "2015", "2016"))+
  #scale_x_continuous(labels=NULL)+
      #breaks=c(2015, 2016), 
                     #limits=c(2014.5, 2016.5), expand=c(0, 0),
  #                   labels=c(2015, 2016))+
  #geom_text(aes(label=sigs))+
  #scale_x_date(date_breaks = "1 year", date_labels = "%y")+
  #facet_grid(site_id~year, scales="free")+
  scale_fill_manual(values=c("#d294af", "#516959"))+
  stat_compare_means(comparison=my_comparisons, label = "p.signif"
                     )+
  theme(panel.grid=element_blank(), strip.text = element_text(size=16), axis.ticks.x = element_blank(),
        axis.title = element_text(size=14), axis.text = element_text(size=12),
        legend.text = element_text(size=12), legend.title = element_text(size=14),
        title = element_text(size=16), plot.title = element_text(hjust=0.5))
#K for temporal

ggsave(here("figs", "temporal_figs", "temporal_condition_sig.png"))

##### SALMON GRAPH - GFI/OVERLAP #####

#per_overlap$date_id <- as.Date(per_overlap$date_id)

#temp_gfi_data_modified <- mutate(temp_gfi_all_data, survey_date=if_else(survey_date==""))

temp_gfi_data_modified <- temp_gfi_all_data

temp_gfi_data_modified$survey_date[which(temp_gfi_data_modified$survey_date=="2015-06-05")] <- "2015-06-06"
temp_gfi_data_modified$survey_date[which(temp_gfi_data_modified$survey_date=="2015-06-07")] <- "2015-06-06"

temp_overlap_data <- left_join(temp_gfi_data_modified, per_overlap, by=c("site_id", "survey_date", "year"))

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
  theme(panel.grid=element_blank(), strip.text = element_text(size=14),
        axis.title = element_text(size=12), axis.text.y = element_text(size=10),
        legend.text = element_text(size=8), legend.title = element_text(size=10),
        title = element_text(size=14), plot.title = element_text(hjust=0.5),
        legend.background = element_rect(color = "dark grey", fill = NA),
        legend.position = c(0.90, .875),
        axis.title.y.right = element_text(color = "darkred"), axis.text.y.right = element_text(color="darkred"),
        axis.text.x = element_text(size=10))+
  facet_grid(site_id~year, scales = "free_x")

ggsave(here("figs", "temporal_figs", "temporal_gfi.png"), width=22, height=14, units = "cm", dpi=800)

##### SALMON GRAPH - Niche Breadth #####

temp_data_taxa_sum %>% 
  ggplot(aes(survey_date, totals, group=interaction(fish_species, survey_date)))+
  geom_boxplot(aes(fill=fish_species), width=5)+
  labs(title=NULL, y="Number of taxanomic groups", fill="Species",
       x=NULL)+
  theme_bw()+
  scale_x_date(date_breaks = "17 days", date_labels = "%b %d")+
  scale_fill_manual(values=c("#d294af", "#516959"))+
  theme(panel.grid=element_blank(), strip.text = element_text(size=14),
        axis.title = element_text(size=12), axis.text.y = element_text(size=10),
        legend.text = element_text(size=10), legend.title = element_text(size=12),
        title = element_text(size=14), plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(size=10))+
  facet_grid(site_id~year, scales = "free_x")#+
#  scale_x_discrete(labels=c("DI_Early"="May", "DI_June_Early"="Early June", "DI_June_Mid"="Mid-June",
#                            "JS_June_Early"="Early June", "JS_June_Mid"="Mid-June", "JS_Late"="July"))
#boxplot for simple version of niche breadth (just number of taxa in each fish stomach)

ggsave(here("figs","temporal_figs","temporal_niche_breadth.png"))

##### SALMON GRAPHS - NMDS #####

temp_diet_wide_nmds <- temporal_diets %>%
  filter(food_weight_corr!=0) %>% 
  group_by(ufn, fish_species, site_id, survey_date, year, yday, prey_info) %>%
  summarise(biomass=sum(prey_weight_corr)) %>%
  spread(key=prey_info, value = biomass, fill=0) %>%
  ungroup()
#calculate wide data set with new prey groups (detailed and general!)

temp_diet_wide_nmds$site_id <- factor(temp_diet_wide_nmds$site_id, levels=c("D07", "J07"))

site_names_nmds <- temp_diet_wide_nmds$site_id
species_names_nmds <- temp_diet_wide_nmds$fish_species
ufn_names_nmds <- temp_diet_wide_nmds$ufn
year_names_nmds <- temp_diet_wide_nmds$year
site_year_df <- mutate(temp_diet_wide_nmds, site_years=paste(site_id, year, sep=" - "))
site_year_names <- site_year_df$site_years
site_sp_df <- mutate(temp_diet_wide_nmds, site_sp=paste(site_id, fish_species, sep=" - "))
site_sp_names <- site_sp_df$site_sp
# double check later that this works *****
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

### Caroline's code on sp. arrows to nmds
#making a new dataset containing species data to add species vectors to graph

#temp_diet_overwrite <- temp_diet_intermediate %>%
#  mutate(prey_group_less=if_else(prey_group=="Decapoda" | prey_group=="Balanomorpha" | prey_group=="Echinodermata",
#                                 "Meroplankton", prey_group))

temp_diet_overwrite <- temp_diet_biomass# %>%
#  mutate(Meroplankton=Decapoda+Balanomorpha+Echinodermata, Other=Other+Cladocera+`Euphausiidae Eggs`) %>%
#  mutate(Other=Other+Cladocera+`Euphausiidae Eggs`+Decapoda+Balanomorpha+Echinodermata) %>%
#  select(-c(Decapoda, Balanomorpha, Echinodermata, Cladocera, `Euphausiidae Eggs`)) #%>%
#  rename(Eggs=`Euphausiidae Eggs`, Balan=Balanomorpha, Clado=Cladocera, Calan=Calanoida, Chaet=Chaetognatha, Echin=Echinodermata, Decap=Decapoda, Gelat=Gelatinous, Appen=Appendicularia)
#  rename(EG=`Euphausiidae Eggs`, BA=Balanomorpha, CL=Cladocera, CA=Calanoida, CH=Chaetognatha, EC=Echinodermata, DE=Decapoda, GE=Gelatinous, AP=Appendicularia, OT=Other)

temp_all_groups <- temp_diet_overwrite #%>%
  #filter(food_weight_corr!=0) %>% 
  #select(ufn, prey_weight_corr, prey_group_less) %>%
  #group_by(ufn, prey_group_less) %>%
  #summarise(ww=sum(prey_weight_corr)) %>% 
  #spread(key=prey_group_less, value=ww, fill=0) # ignote this tyoooo

temp_groups_matrix <- temp_all_groups %>%
  ungroup() %>% 
  select(-(ufn:year)) %>%
  decostand("total") # ignore tyhis./..

#rownames(diet_group_biomass) <- temp_diet_biomass$ufn
rownames(temp_groups_matrix) <- temp_all_groups$ufn

group_trans_nmds <- asin(sqrt(temp_groups_matrix))

fit <- envfit(eco.nmds.bc, group_trans_nmds)
spp.scores <- as.data.frame(scores(fit, display = "vectors")) #save species intrinsic values into dataframe
spp.scores <- cbind(spp.scores, Species = rownames(spp.scores)) #add species names to dataframe
spp.scores <- cbind(spp.scores, pval = fit$vectors$pvals) #add pvalues to dataframe so you can select species which are significant
sig.spp.scores <- subset(spp.scores, pval<=0.002) #subset data to show species significant at 0.05
head(sig.spp.scores)

spp.scores$Species <- c(NA, "atop(bold(Calanoida))", NA, NA, NA, NA, NA, "atop(bold(Gelatinous))",
                        "atop(bolditalic(Oikopleura))", "atop(bold(Chaetognatha))", "atop(bold(Other))")

# FIGURE OUT HOW TO MAKE SOME OF THEM BLANK LATERRRRR

#add to ggplot
#geom_segment(data = spp.scores, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + 
#  ggrepel::geom_text_repel(data = spp.scores, aes(x=NMDS1, y=NMDS2, label = Species), cex = 3, direction = "both", segment.size = 0.25, size = 14)
### end of Caroline's code

a <- ggplot(NMDS.bc, aes(NMDS1.bc, NMDS2.bc))+
  geom_point(stat = "identity", aes(shape=species_names_nmds, color= site_names_nmds), fill="white", stroke = 0.75, alpha=0.9)+
  scale_color_manual(values=c("darkred", "#053061"), name="Site") + # check that the colors work! 
  #new_scale_color()+
  #geom_path(data=df_ell.bc, aes(x=NMDS1, y=NMDS2,colour=group), size=1, linetype=2) +
  scale_shape_manual(values=c(21, 19), name="Species")+
  guides(color= guide_legend(override.aes = list(shape=19)), fill=FALSE,
  shape=guide_legend(order = 1))+
  labs(x="NMDS 1", y="NMDS 2")+
  #scale_colour_manual(values=c("darkred", "#053061"), #name="Region", guide=FALSE) +
  theme_bw()+
  theme(#axis.text=element_text(size=14), #axis.title.x=element_text(size=12),
        axis.title.y=element_text(angle=90),
        panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
        axis.ticks = element_blank()) + coord_fixed() +
  geom_segment(data = sig.spp.scores, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.2, "cm")), colour = "grey10", lwd=0.35) + 
  ggrepel::geom_text_repel(data = spp.scores, aes(x=NMDS1, y=NMDS2, label = Species),# cex = 3, 
                           direction = "both",
                           size = 4, fontface="bold", parse=T, segment.colour = NA,
                           nudge_y=-0.3)+
                           #nudge_y = if_else(spp.scores$Species=="Calanoida", -0.25, 
                          #                   if_else(spp.scores$Species=="Gelatinous", -0.1,
                          #                   if_else(spp.scores$Species=="Chaetognatha", 0.5, 0))),
                          # nudge_x=if_else(spp.scores$Species=="Other", -0.3, 0))+
  annotate("text",x=2.5,y=-2.3,label="(stress = 0.15)",size=3, hjust = 0)
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
temp_envr_wide <- temp_gfi_all_data %>%
  filter(food_weight_corr!=0) %>%
  #mutate(fish_total=so_total+pi_total+cu_total+co_total+he_total) %>% 
  select(ufn, fish_species, survey_date, site_id, yday, year, #week, food_weight_corr, #digestion_state, length_avg,
         weight, fork_length, temperature, salinity, zoop_ww, #set_time, time_searching, cloud_cover,
         secchi#, sea_state, k, totals, gfi, pi_total, cu_total, fish_total
         )

temp_envr_all_data <- left_join(temp_envr_wide, percent_overlap, by=c("site_id", "year", "survey_date"))

rownames(temp_envr_all_data) <- temp_envr_all_data$ufn

temp_envr_all_data$fish_species <- as.numeric(temp_envr_all_data$fish_species)

temp_envr_all_data$site_id[which(temp_envr_all_data$site_id=="D07")] <- 1
temp_envr_all_data$site_id[which(temp_envr_all_data$site_id=="J07")] <- 2

temp_envr_all_data$site_id <- as.numeric(temp_envr_all_data$site_id)
temp_envr_all_data$year <- as.numeric(temp_envr_all_data$year)

temp_envr_matrix <- select(temp_envr_all_data, -c(ufn, survey_date, site_id, #set_time, 
                                                  overlap, fish_species, date_id)) %>%
  as.matrix()

#temp_envr_matrix_trans <- scale(temp_envr_matrix)

temp_envr_matrix_trans <- BBmisc::normalize(temp_envr_matrix, method="range", margin=2)

temp_comm_matrix <- as.matrix(temp_trans_nmds)

temp_envr_all_data %>%
  group_by(site_id, year, yday) %>%
  unique() %>%
  ggplot(aes(yday, secchi, group=interaction(year, site_id)))+
  geom_line()+
  facet_grid(site_id~year)

#temp_comm_matrix <- as.numeric(temp_comm_matrix)

#temp_envr_matrix <- as.numeric(temp_envr_matrix)

#fish taken ... what about sizes? how to incorp that? other stats?

bioenv(temp_comm_matrix, temp_envr_matrix_trans)

# try this again at the end of the day (takes so long to run code...)

# try with so_total too? Take out cloud cover?????

# OOH! TRY SEPARATING DI AND JS AND RUNNING TWO BIO-ENVS (and 2 nmds'???)

# site id, year, zoop_ww, cloud cover? pi total! with correl. 0.194681

# year, salinity, zoop_ww = 0.2117294 (w less variables)

# exact same when envr data is centered and scaled. same when normalized

# try again with only environmental variables? no fish #, or other fish stuff?????

#findCorrelation() found weight (of fish) correlated with total chum # ... hmm.

# I THINK I NEED TO STANDARDIZE MY ENVIRONMENTAL MATRIX AGAGREARHJARJKA

# AAAAAND I WANT TO TRY TO ADD WEEK AS A VARIABLE!

str(temp_trans_nmds)

##### BIO-ENV (REG) #####

# use:

#bioenv(temp_comm_matrix, temp_envr_matrix_trans)

# need to transform to two datasets before ditching site column and calculating stuff

temp_diet_rel_bio_regions <- cbind(ufn_names_nmds, site_names_nmds, temp_diet_matrix_nmds)
#combine ufn/site/species back onto relative biomass of prey groups data

temp_diet_rel_bio_di <- temp_diet_rel_bio_regions %>%
  filter(site_names_nmds=="D07") %>%
  select(-site_names_nmds)

rownames(temp_diet_rel_bio_di) <- temp_diet_rel_bio_di$ufn

group_trans_di <- asin(sqrt(select(temp_diet_rel_bio_di, -ufn_names_nmds)))

temp_diet_rel_bio_js <- temp_diet_rel_bio_regions %>%
  filter(site_names_nmds=="J07") %>%
  select(-site_names_nmds)
# note: some species now have 0 throughout... will this affect analysis?
# not sure if I should delete or if that will mess with analysis...

rownames(temp_diet_rel_bio_js) <- temp_diet_rel_bio_js$ufn

group_trans_js <- asin(sqrt(select(temp_diet_rel_bio_js, -ufn_names_nmds)))

temp_envr_matrix_di <- temp_envr_all_data %>% 
  filter(site_id==1) %>% 
  select(-c(ufn, survey_date, #set_time,
            date_id, site_id)) %>%
  as.matrix()

temp_envr_di_trans <- BBmisc::normalize(temp_envr_matrix_di, method="range", margin=2)

temp_envr_matrix_js <- temp_envr_all_data %>% 
  filter(site_id==2) %>% 
  select(-c(ufn, survey_date, #set_time,
            date_id, site_id)) %>%
  as.matrix()

temp_envr_js_trans <- BBmisc::normalize(temp_envr_matrix_js, method="range", margin=2)

bioenv(group_trans_js, temp_envr_js_trans)
# temperature, cloud cover and gfi 0.2960399
# with less variables: fish_sp, temp, sal=0.1900432... GFI/clouds important?

bioenv(group_trans_di, temp_envr_di_trans)
# fish species, weight, salinity, time searching, cloud cover, secchi 0.2575871
# fish_sp, year, weight, salinity, secchi = 0.227402

# THEN TRY THIS FOR SPECIES AS WELL????? Or too much messing around?//

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


##### ANOSIM / SIMPER #####

anosim_diet <- anosim(x=temp_trans_nmds, grouping=interaction(c(site_year_names)),
                      strata = site_names_nmds, 
                      distance = "bray")

summary(anosim_diet)

plot(anosim_diet)

simper_site <- simper(temp_trans_nmds, site_names_nmds)
reg_simper <- summary(simper_site)#, digits=3)
#reg_simper_df <- round(reg_simper$D07_J07 * 100, 2) %>%
reg_simper_df <- reg_simper$D07_J07 * 100 #%>%
  #select(DI=ava, JS=avb, Avg=average, Sum=cumsum) %>%
  #filter(Sum<71)

reg_simper_groups <- reg_simper_df %>%
  mutate(Taxa=rownames(reg_simper_df), Group=c("\\emph{Oikopleura}", "Gelatinous", "Calanoida", "Calanoida", "Euphausiidae Eggs",
                                  "Calanoida", "Gelatinous", "Balanomorpha", "Chaetognatha", "Decapoda", "Calanoida",
                                  "Echinodermata", "Calanoida", "Cladocera", "Calanoida"))

# make this a nice looking table later on ***** !

# Try with rel bio (not transformed)... then ignore the other SIMPER variations below

# oiko, cnidaria, c. marshallae, calanoida, euphausiid egg = 50% of diff b/w di-js

# was marshallae more abundant in JS? probably. tinier stuff in DI! interesting...

simper_sp <- simper(temp_trans_nmds, species_names_nmds)
summary(simper_sp)
#important prey for dissimilarity between species (cnid, oiko, c. marsh, cala, c. pac, euph egg)
# does that mean that pink ate more calas and WHY is euph egg so high? BOTH SP ATE LOTS OF IT...
# overall ave diff values are low-ish = prey partitioning. both sp eat stuff in diff proportions

simper_yr <- simper(temp_trans_nmds, site_year_names)
summary(simper_yr)
# this one don't make sense. don't even use it anywaysss (year comparison within sites cool tho)
#cnid, c marsh, cala, cteno, c pac ; oiko, euph egg, euk hamata, c marsh, barn naup, echi, etc.
# it seems like simper just spits out most dominant prey... look into all this some more later.

simper_site_yr <- simper(temp_trans_nmds, site_year_names)
summary(simper_site_yr)
# DI: oiko, e egg, e hamata, c marsh, b naup, echin, cala, sagit, podonidae, c pac = cum 50%
# DI 15 / JS 16 ? oiko, cnid, c marsh, cten, cala, e hamata = 50% cum diff
#DI/JS 15: cnid, oiko, c marsh, e hamata = 50% cum diff
# DI/JS 16: oiko, cnid, c marsh, e egg, cten = 50% cum diff
# DI 16 / JS 15? cnid, oiko, c marsh, e egg = 50% cum diff
# JS: cnid, c marsh, cala, cten = 50% cum diff

simper_site_sp <- simper(temp_trans_nmds, site_sp_names)
summary(simper_site_sp)
# DI compare sp. oiko, euph egg, echin, b naup, cala, brach, c marsh, e hamat, podon, b cren = 50%
# PI compare reg. c marsh, oiko, cala, c pac, e egg, b naup, aetideus, brach, echin = 50%
# CU compare reg. cnid, oiko, c marsh, cten, e egg, e hamat = 70% (cnid + oiko > 50%!)
# JS compare sp. cnid, c marsh, cala = 50%; c pac, cten, aetid, pseudo = 70%
# ignore DI PI/JS CU and JS PI/DI CU ... is that ok to do??
# DICU/JSPI oiko, c marsh, cala, c pac, aet = 50%
# DIPI/JSCU cnid, oiko, c marsh, e egg, b naup, cten = 50%

temp_trans_perm <- as.data.frame(temp_trans_nmds)

permanova_data <- adonis(temp_trans_nmds ~ site_names_nmds*year_names_nmds*species_names_nmds)

permanova_data

region_names <- str_sub(site_names_nmds, start = 1, end=1)

temp_matrix_anosim <- data.frame(ufn=ufn_names_nmds, site=as.numeric(site_names_nmds),#year=year_names_nmds,
#                                date=as.numeric()
                                 fish=as.numeric(species_names_nmds), temp_trans_nmds)

#write_csv(temp_matrix_anosim, "temp_data.csv")

# YOOOOO R won't do 2 way anosim so I did it using PAST:

#Permutation N: 9999
#Factor site
#R:	0.6145
#p(same):	0.0001
#Factor year
#R:	0.12441
#p(same):	0.0001

#Permutation N: 9999
#Factor site
#R:	0.7064
#p(same):	0.0001
#Factor fish
#R:	0.43452
#p(same):	0.0001

# PINK SIMPER CALC / TABLE

temp_diet_wide_simper <- temp_diet_wide_nmds #select(temp_diet_wide_nmds, -c(Detritus, Digested_food_worms, Crustacea, Parasite, Coscinodiscophycidae))
#make a copy (don't need to filter anymore)

pink_diet_wide <- temp_diet_wide_simper %>%
  filter(fish_species=="Pink")

site_names_pink <- pink_diet_wide$site_id
ufn_names_pink <- pink_diet_wide$ufn

temp_diet_matrix_pink <- pink_diet_wide %>%
  select(Acartia:Tortanus_discaudatus) %>% 
  decostand(method="total")

temp_diet_rel_bio_pink <- cbind(ufn_names_pink, temp_diet_matrix_pink)

matrixP<-as.matrix(temp_diet_rel_bio_pink)
row.names(matrixP) <- matrixP[,1]
temp_pink_matrix <- matrixP[,-1]
class(temp_pink_matrix)<-"numeric"
temp_trans_pink <- asin(sqrt(temp_pink_matrix))

simper_pink <- simper(temp_trans_pink, site_names_pink)
pink_simper <- summary(simper_pink)
pink_simper_df <- pink_simper$D07_J07 * 100

rel_bio_percent_pink <- cbind(ufn=ufn_names_pink, site_id=site_names_pink, temp_diet_matrix_pink*100)

ave_bio_region_pink <- rel_bio_percent_pink %>%
  gather(key="Taxa", value="Bio", Acartia:Tortanus_discaudatus) %>%
  group_by(site_id, Taxa) %>%
  summarise(mean_rel_bio=round(mean(Bio), digits=2)) %>%
  spread(key=site_id, value=mean_rel_bio, fill=0)

simper_pink_summary <- round(pink_simper_df, digits=2) %>%
  mutate(Taxa=rownames(pink_simper_df)) %>%
  select(Taxa, Average=average, Sum=cumsum) %>%
  filter(Sum<72)

simper_table_pink <- left_join(simper_pink_summary, ave_bio_region_pink)

simper_table_pink$Taxa <- c("\\emph{Calanus marshallae}", "\\emph{Oikopleura} spp.", "Calanoida",              
                                 "\\emph{Calanus pacificus}", "Nauplii", "Eggs",        
                                 "\\emph{Aetideus divergens}", "Brachyura Larvae", "Larvae",     
                                 "\\emph{Calanus} spp.", "\\emph{Pseudocalanus} spp.", "Podonidae",
                            "\\emph{Balanus crenatus} Cyprid", "\\emph{Epilabidocera longipedata}", "\\emph{Balanus glandula} Cyprid",  
                                 "Furcilia", "\\emph{Pseudevadne tergestina}", "Sagittoidea")

simper_table_pink_full <- simper_table_pink %>%
  mutate(Groups=c("Calanoida","\\emph{Oikopleura}","Calanoida","Calanoida","Balanomorpha",
                  "Euphausiidae","Calanoida","Decapoda","Echinodermata","Calanoida",
                  "Calanoida","Cladocera","Balanomorpha","Calanoida","Balanomorpha",
                  "Euphausiidae","Cladocera","Chaetognatha")) %>% 
  select(Groups, Taxa, DI=D07, JS=J07, Average, Sum)

kable(simper_table_pink_full, "latex", booktabs=TRUE, linesep="", escape = FALSE) %>%
  kable_styling(latex_options = "hold_position", font_size = 8, full_width = TRUE) %>%
  add_header_above(c("A) Pink salmon SIMPER results"=6)) %>% 
  column_spec(3:6, width = "0.3in") %>% 
  column_spec(2, width="1.5in") %>% 
  save_kable(here("tables", "temporal_tables", "simper_pink.pdf"))

# CHUM SIMPER CALC / TABLE

chum_diet_wide <- temp_diet_wide_simper %>%
  filter(fish_species=="Chum")

site_names_chum <- chum_diet_wide$site_id
ufn_names_chum <- chum_diet_wide$ufn

temp_diet_matrix_chum <- chum_diet_wide %>%
  select(Acartia:Tortanus_discaudatus) %>% 
  decostand(method="total")

temp_diet_rel_bio_chum <- cbind(ufn_names_chum, temp_diet_matrix_chum)

matrixC<-as.matrix(temp_diet_rel_bio_chum)
row.names(matrixC) <- matrixC[,1]
temp_chum_matrix <- matrixC[,-1]
class(temp_chum_matrix)<-"numeric"
temp_trans_chum <- asin(sqrt(temp_chum_matrix))

simper_chum <- simper(temp_trans_chum, site_names_chum)
chum_simper <- summary(simper_chum)
chum_simper_df <- chum_simper$D07_J07 * 100

rel_bio_percent_chum <- cbind(ufn=ufn_names_chum, site_id=site_names_chum, temp_diet_matrix_chum*100)

ave_bio_region_chum <- rel_bio_percent_chum %>%
  gather(key="Taxa", value="Bio", Acartia:Tortanus_discaudatus) %>%
  group_by(site_id, Taxa) %>%
  summarise(mean_rel_bio=round(mean(Bio), digits=2)) %>%
  spread(key=site_id, value=mean_rel_bio, fill=0)

simper_chum_summary <- round(chum_simper_df, digits=2) %>%
  mutate(Taxa=rownames(chum_simper_df)) %>%
  select(Taxa, Average=average, Sum=cumsum) %>%
  filter(Sum<73)

simper_table_chum <- left_join(simper_chum_summary, ave_bio_region_chum)

simper_table_chum$Taxa <- c("Cnidaria", "\\emph{Oikopleura} spp.", "\\emph{Calanus marshallae}",
                            "Ctenophora", "Eggs", "\\emph{Eukrohnia hamata}")

simper_table_chum_full <- simper_table_chum %>%
  mutate(Groups=c("Gelatinous","\\emph{Oikopleura}","Calanoida","Gelatinous",
                  "Euphausiidae", "Chaetognatha")) %>% 
  select(Groups, Taxa, DI=D07, JS=J07, Average, Sum)

kable(simper_table_chum_full, "latex", booktabs=TRUE, linesep="", escape = FALSE) %>%
  kable_styling(latex_options = "hold_position", font_size = 8, full_width = TRUE) %>%
  column_spec(3:6, width = "0.3in") %>% 
  column_spec(2, width="1.5in") %>% 
  add_header_above(c("B) Chum salmon SIMPER results"=6)) %>% 
  save_kable(here("tables", "temporal_tables", "simper_chum.pdf"))

##### CLUSTER #####

Bray_Curtis_Dissimilarity <- vegdist(temp_trans_nmds, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity, method = "average")
#make dendrogram data (heirarchical clustering by average linkages method)

#num_clust <- simprof(temp_trans_nmds, method.cluster = "average", method.distance = "actual-braycurtis", num.expected = 100, num.simulated = 99)

#simprof.plot(num_clust)

#summary(num_clust)

clust <- cutree(bcclust, k = 5)               # find 'cut' clusters
#clust.df <- data.frame(label = names(clust), cluster = clust)
#colnames(clust.df) <- c("ufn", "cluster")

dendr <- dendro_data(bcclust, type = "rectangle") 

library(dendextend)

#dend_list <- get_subdendrograms(dendr, 5)

dend <- as.dendrogram(bcclust)

dend_list <- get_subdendrograms(dend, 5)

ggd1 <- as.ggdend(dend)

sub_dend_di <- dend_list[[1]]
sub_dend_js <- dend_list[[2]]

ggdi <- as.ggdend(sub_dend_di)
ggjs <- as.ggdend(sub_dend_js)

#date_colors <- cbind(percent_overlap, colors=c("#7f2704", "#d94801", "#d94801", "darkred", "#7f2704", "#d94801", "darkred",
#                                               "#542788", "#014636", "#053061", "#542788", "#014636", "#053061"),
                     #codes=c("earlydi", "middi", "middi", "latedi", "earlydi", "middi", "latedi", "earlyjs", "midjs", "latejs", "earlyjs", "midjs", "latejs"))
#                     codes=c("A", "B", "B", "C", "A", "B", "C", "D", "E", "F", "D", "E", "F"))
# brown orange #7f2704
# orange x2 #d94801
# red (repeat BO, O, R) "darkred"
# purple #542788
# dark teal ish #014636
# blue (repeat p, t, b) #053061

date_colors <- data.frame(site_id=c(rep("D07", 7), rep("J07", 6)),
                          survey_date=as.Date(c("2015-05-21", "2015-06-05", "2015-06-07", "2015-06-13",
                                                "2016-05-19", "2016-06-03", "2016-06-16",
                                                "2015-06-02", "2015-06-14", "2015-06-29",
                                                "2016-06-03", "2016-06-20", "2016-07-05")),
                          #codes=c("A", "B", "B", "C", "A", "B", "C", "D", "E", "F", "D", "E", "F"))
                          codes=c("DI - Late May", "DI - Early June", "DI - Early June", "DI - Mid-June", "DI - Late May", "DI - Early June", "DI - Mid-June", "JS - Early June", "JS - Mid-June", "JS - Late June/\nEarly July", "JS - Early June", "JS - Mid-June", "JS - Late June/\nEarly July"))

season_levels <- c("DI - Late May", "DI - Early June", "DI - Mid-June", "JS - Early June", "JS - Mid-June", "JS - Late June/\nEarly July")

date_colors$codes <- factor(date_colors$codes, levels=season_levels)

site_year_colors <- left_join(site_year_df, date_colors, by=c("survey_date", "site_id"))

fishsp <- site_year_colors %>%
  ungroup() %>%
  select(ufn, Sp=fish_species, Site=site_id, Year=year, Site_Year=site_years, Date=survey_date, Day=yday, codes)

ggd1$labels <- select(ggd1$labels, -c(col, cex))

labs <- label(ggd1)

colnames(labs) <- c("x", "y", "ufn")

lab <- left_join(labs, fishsp, by = "ufn")

#ggdi$labels <- select(ggdi$labels, -c(col, cex))
#ggjs$labels <- select(ggjs$labels, -c(col, cex))

labs_di <- label(ggdi)
labs_js <- label(ggjs)

colnames(labs_di) <- c("x", "y", "ufn", "col", "cex")
colnames(labs_js) <- c("x", "y", "ufn", "col", "cex")

lab_di <- left_join(labs_di, fishsp, by = "ufn")
lab_js <- left_join(labs_js, fishsp, by = "ufn")

#lab_clusts <- left_join(lab, clust.df, by="ufn")

#clust_DI <- filter(lab_clusts, cluster==1)

#clust_JS <- filter(lab_clusts, cluster==2)

library(ggdendro)
library(zoo)
#cluster dendrograms

cut <- 5# Number of clusters
#hc <- hclust(dist(df), "ave")       # bcclust        # hierarchical clustering
dendr <- dendro_data(bcclust, type = "rectangle") 
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
#clust.df$label <- factor(clust.df$label, levels = dendr$labels$label)
#clust.df <- arrange(clust.df, label)
#clust.df$cluster <- factor((clust.df$cluster), levels = unique(clust.df$cluster), labels = (1:cut) + 1)
#dendr[["labels"]] <- merge(dendr[["labels"]], clust.df, by = "label")

# Positions for cluster labels
#n.rle <- rle(dendr$segments$cluster)
#N <- cumsum(n.rle$lengths)
#N <- N[seq(1, length(N), 2)] + 1
#N.df <- dendr$segments[N, ]
#N.df$cluster <- N.df$cluster - 1

color_df <- data.frame(cluster=c(1, 2, 3, 4), values=c("grey", "grey", "red", "blue"))

clust_colors <- c("grey", "grey", "red", "blue")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color=as.factor(dendr$segments$cluster)), show.legend = FALSE, size=0.5) +
  scale_color_manual(values=c("darkgrey", "darkgrey", "darkgrey","darkred", "darkgrey", "#053061"))+
  scale_x_continuous(expand=c(0.01, 0.01))+
  scale_y_continuous(expand=c(0.01, 0.01))+
  new_scale_color()+
  geom_point(data=label(dendr), aes(x=x, y=y, shape=lab$Sp, color=lab$Site),
             fill="white", size=1, stroke = 1)+
  scale_shape_manual(values=c(21, 19), name="Species")+
  scale_color_manual(values=c("darkred", "#053061"), name="Site")+
  guides(color= guide_legend(override.aes = list(shape=21)),
         shape=guide_legend(order = 1))+
  new_scale_color()+
  theme_bw()+
  theme(axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.text = element_text(size=6),
        axis.title = element_text(size=8),
        legend.text = element_text(size=6),
        legend.title = element_text(size=8),
        legend.background = element_rect(color = "dark grey", fill = NA),
        panel.grid=element_blank(), legend.position = c(0.89, .9),
        legend.box = "horizontal")+
  labs(y="Dissimilarity")
  #plot the dendrogram data for the different fish ID's (middle outlier clust = 94% dissim.)

ggsave(here("figs", "temporal_figs", "temporal_cluster_all.png"), width=22, height = 14.5, units = "cm", dpi=800)

ggplot()+
  geom_segment(data = segment(ggdi), aes(x=x, y=y, xend=xend, yend=yend
  ), color="#666666",
  show.legend = FALSE, 
  size=0.5) +
  #geom_hline(yintercept = 0.93, linetype="dashed")+
  #geom_hline(yintercept = 0.733, linetype="dashed")+
  scale_x_continuous(expand=c(0.01, 0.01))+
  scale_y_continuous(expand=c(0.15, 0.01))+
  geom_text(data=label(ggdi), aes(x=x, y=y, label=lab_di$Date, hjust=1.1, color=lab_di$codes
  ), size=2.25, angle=90, fontface="bold") +
  scale_color_manual(values=c("darkgoldenrod", "#d94801", "darkred",
                              "#016c59", "#542788", "#053061"), name="Date", guide_legend(order=3))+
  new_scale_color()+
  geom_point(data=label(ggdi), aes(x=x, y=y, shape=lab_di$Sp, color=lab_di$Site),
             fill="white", size=1, stroke = 1)+
  scale_shape_manual(values=c(21, 19), name="Species")+
  scale_color_manual(values=c("darkred", "#053061")#, #name="Site", labels=NULL, 
                     #guide=NULL
  )+
  guides(color="none", #guide_legend(override.aes = list(shape=21)),
         shape=guide_legend(#override.aes=list(shape=c(16, 1)), 
           order = 1))+
  theme_bw()+
  theme(axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.text = element_text(size=6),
        axis.title = element_text(size=8),
        legend.text = element_text(size=6),
        legend.title = element_text(size=8),
        legend.background = element_rect(color = "dark grey", fill = NA),
        panel.grid=element_blank(), legend.position = c(0.85, .89),
        legend.box = "horizontal")+
  labs(y="Dissimilarity")
#plot the dendrogram data for the different fish ID's

ggsave(here("figs", "temporal_figs", "temporal_subcluster_DI.png"), width=22, height = 14.5, units = "cm", dpi=800)

ggplot()+
  geom_segment(data = segment(ggjs), aes(x=x, y=y, xend=xend, yend=yend
  ), color="#666666",
  show.legend = FALSE, 
  size=0.5) +
  scale_x_continuous(expand=c(0.01, 0.01))+
  scale_y_continuous(expand=c(0.1, 0.05))+
  #guides(color="none",
  #       shape=guide_legend(override.aes=list(shape=c(16, 1)), 
  #         order = 2)
  #       )+
  geom_point(data=label(ggjs), aes(x=x, y=y, shape=lab_js$Sp, color=lab_js$Site),
             fill="white", size=1, stroke = 1)+
  scale_shape_manual(values=c(21, 19), name="Species", guide_legend(order=2))+
  scale_color_manual(values=c("darkred", "#053061"), name="Site", guide=F)+
  new_scale_color()+
  geom_text(data=label(ggjs), aes(x=x, y=y, label=lab_js$Date, hjust=1.1, color=lab_js$codes
  ), size=2.25, angle=90, fontface="bold") +
  scale_color_manual(values=c("darkgoldenrod", "#d94801", "darkred",
                              "#016c59", "#542788", "#053061"), name="Date", guide_legend(order=1))+
  guides(color=guide_legend(order=1), shape=guide_legend(order=2))+
  theme_bw()+
  theme(axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.text = element_text(size=6),
        axis.title = element_text(size=8),
        legend.text = element_text(size=5),
        legend.title = element_text(size=7),
        legend.background = element_rect(color = "dark grey", fill = NA),
        panel.grid=element_blank(), legend.position = c(0.125, .8),
        legend.box = "horizontal")+
  labs(y="Dissimilarity")
#plot the dendrogram data for the different fish ID's

ggsave(here("figs", "temporal_figs", "temporal_subcluster_JS.png"), width=22.5, height = 14.25, units = "cm", dpi=800)

# 7 DI subclusts? 5 JS subclusts? Then do simper between them>??? or ind val. to quantify important prey. 

df <- data.frame(temp_trans_nmds)

res<- simprof((df), method.distance = "braycurtis", alpha=0.01)#,
#                          #num.expected = 10, num.simulated = 10)

#res<- simprof((df), method.distance = "actual-braycurtis", alpha=0.05)

simprof.plot(res)

mydf <- as.data.frame(unlist(res$significantclusters))

clustdf <- mutate(mydf, clusts=c(1, 2, 3, rep(4, 7), 5, 6, rep(7, 5), rep(8, 10), rep(9, 2), rep(10, 7), rep(11, 8), 12, 13,
                                 rep(14, 2), rep(15, 2), 16, rep(16, 2), 18, 19, 20, rep(21, 6), 22, rep(23, 12), rep(24, 2), rep(25, 6), rep(26, 2), rep(27, 5), 28, rep(29, 3), 30, 31,
                                 rep(32, 3), rep(33, 3), rep(34, 6), rep(35, 3), rep(36, 2), rep(37, 15), rep(38, 5), rep(39, 3), rep(40, 3), rep(41, 2), rep(42, 6), rep(43, 7), rep(44, 4), rep(45, 7), rep(46, 4), 47,
                                 rep(48, 7), rep(49, 9), 50, 51, rep(52, 12), rep(53, 4)))
# 7 9 1 1 12 4
# see if this works then run simper on significant clusters (as a quantitative way to say that
# middle cluster is because of the presence of euphausiids without just looking at data...)
# same with when I divide up DI and JS subclusters, it's obvious what diet comp is but need quant.

clustdf <- mutate(mydf, clusts=c(1, 2, 3, rep(4, 7), 5, rep(6,6), rep(7, 10), rep(8, 2),
                                 rep(9, 7), rep(10, 8), 11, 12, rep(13, 2), rep(14, 2),
                                 15, rep(16, 2), rep(17,22), rep(18, 2), rep(19, 6), rep(20, 2),
                                 rep(21, 5), 22, rep(23, 3), 24, rep(25, 38),
                                 rep(26, 3), rep(27, 3), rep(28, 8), rep(29, 7), 
                                 rep(30, 11), rep(31, 4), 32, rep(33, 16),
                                 rep(34, 14), rep(35, 4)))

##### FREQ OCCUR ######

pa_diet_data <- cbind(temp_data_wide_info, temp_data_pa)

freq_occur_samples <- pa_diet_data %>% 
  group_by(site_id, fish_species, year) %>%
  mutate(n=n()) %>%
  group_by(site_id, fish_species, year, n) %>%
  summarise_at(vars(Acartia:Tortanus_discaudatus), sum)
  
freq_occur_matrix <- freq_occur_samples %>%
  ungroup() %>%
  select(-c(site_id, fish_species, year))

#freq_occur_samples$n <- as.numeric(freq_occur_samples$n)

freq_occur_nums <- freq_occur_matrix/freq_occur_matrix$n*100
#expressed as a percent rather than decimals
#freq occur for all taxa, sites and species (temporal)

freq_occur_data <- cbind(site_id=freq_occur_samples$site_id,
                         fish_species=freq_occur_samples$fish_species,
                         year=freq_occur_samples$year,
                         freq_occur_nums)

# maybe need to change to long data to do some calcs to weed out low freq occur? too much taxa!

freq_occur_long_data <- freq_occur_data %>%
  gather(key="Taxa", value="Occur", Acartia:Tortanus_discaudatus) %>%
  group_by(Taxa) %>%
  arrange(desc(Occur), Taxa, by_group=TRUE)

# do for all samples (as a way to list taxa. if appendix - add spatial...?)

freq_occur_all_samples <- pa_diet_data %>% 
  group_by(fish_species, site_id) %>%
  mutate(n=n()) %>%
  group_by(fish_species, site_id, n) %>%
  summarise_at(vars(Acartia:Tortanus_discaudatus), sum)

freq_occur_matrix_all <- freq_occur_all_samples %>%
  ungroup() %>%
  select(-c(fish_species, site_id))

freq_occur_info <- select(freq_occur_all_samples, site_id, fish_species)

#freq_occur_samples$n <- as.numeric(freq_occur_samples$n)

freq_occur_nums_all <- freq_occur_matrix_all/freq_occur_matrix_all$n*100
#expressed as a percent rather than decimals
#freq occur for all taxa, sites and species (temporal)

freq_occur_data_all <- cbind(freq_occur_info,
                             freq_occur_nums_all)

freq_occur_long_data_all <- freq_occur_data_all %>%
  gather(key="Taxa", value="Occur", Acartia:Tortanus_discaudatus) %>%
  group_by(Taxa) %>%
  arrange(desc(Occur), Taxa, by_group=TRUE) %>%
  mutate(fish_site=paste(fish_species, site_id, sep="_")) %>% 
  select(-c(n, fish_species, site_id)) %>%
  spread(key=fish_site, value=Occur, fill=0)

prey_taxa <- unique(select(temp_diet_intermediate, genus, species, prey_group, Taxa=prey_info))

freq_taxa <- left_join(freq_occur_long_data_all, prey_taxa)

freq_taxa$prey_group[which(freq_taxa$prey_group=="Small (<2mm)")] <- "Calanoids"
freq_taxa$prey_group[which(freq_taxa$prey_group=="Large (>2mm)")] <- "Calanoids"

no_duplicates <- unique(freq_taxa)

freq_taxa_table <- no_duplicates %>%
  filter(!prey_group %in% c("Crustacea", "Digested_food_worms", "Detritus", "Empty", "Ochrophyta",
                      "Parasite", "Eumalacostraca_Larvae")) %>% 
  select(Group=prey_group, Taxa, Pink_D07, Pink_J07, Chum_D07, Chum_J07)
  
freq_taxa_table$Taxa <- gsub("_", " ", freq_taxa_table$Taxa)

unique(no_duplicates$Taxa)

##### SIZE #####

# want to look at fish size versus average (DS1) prey size

# what if there is no DS1 - completely ignore as if empty?

# this is tricky/impossible for chum... with big jellies
# who ccan't really be measured and then tiny copes (not representative)
# OR i include all DS and it's not representative again...
# WHY did i measure digested prey... too much data aint good here
# how do I measure average prey? Weight? ABD? FREQ OCCUR? ...

# Maybe work off of the frequency of occurance and work by PREY
# rather than arbitrary measurements, go with an average prey size
# visit dataset to get the size range

# then plot fish size versus prey size as scatterplot and see if anything
# wait hang on, if i'm doing a scatterplot then each prey and stom = point
# so I don't want it to be summarized by site/date/whatever or freq occur

# just each prey size plotted against fish size (should have LOTS of points)

size_levels=c("<1", "1 to 2", "2 to 5", "5 to 10", ">10")

size_levels_desc=c(">10", "5 to 10", "2 to 5", "1 to 2", "<1")

temporal_diets$size_class <- factor(temporal_diets$size_class, levels=size_levels)

#temporal_diets$size_class <- factor(temporal_diets$size_class, levels=size_levels_desc)

size_data <- temporal_diets %>%
  filter(length_avg>0, digestion_state==1#, length_avg<20
         #fish_species=="Chum"
         #site_id=="J07"
         ) %>% 
  select(ufn, digestion_state, size_class, length_avg, fork_length, fish_species, year, survey_date, site_id, count, prey_weight_corr) %>%
  group_by(ufn, fork_length, fish_species, site_id, year, survey_date) %>%
  summarize(length_avg=weighted.mean(length_avg, prey_weight_corr)) #%>% 
#  filter(digestion_state==1 #& size_class!="<1" & length_avg<20
#         ) %>%

size_season_colors <- left_join(size_data, date_colors, by=c("survey_date", "site_id"))

pink_size_data <- filter(size_data, site_id=="J07" & year=="2016" & fish_species=="Pink")
chum_size_data <- filter(size_data, site_id=="J07" & year=="2016" & fish_species=="Chum" & length_avg<20)

pink_size_lm <- lm(length_avg~fork_length, pink_size_data)
chum_size_lm <- lm(length_avg~fork_length, chum_size_data)

summary(pink_size_lm)
summary(chum_size_lm)

# CU DI 2015: 0.7107 (adj R2) p < 0.001 *
# Y=-16.2+0.21X
# PI DI 2015: 0.5466 p = 0.001 *
# Y=-12.4+0.16X

# CU DI 2016: 0.4615 p < 0.001 * 
#-2.5+0.05X
# PI DI 2016: -0.03181 p = 0.636 
#0.6+0.01X

# CU JS 2015: -0.01614 p = 0.469
#-3.2+0.05x
# PI JS 2015: -0.01477 p = 0.402
#y=2.0+0.01x

# CU JS 2016: 0.02504 p = 0.201
# 4.9-0.01x
# PI JS 2016: 0.3365 p = 0.001 *
# -2.8+0.05x

linear_results <- data.frame(site_id=c(rep("D07", 4), rep("J07", 4)),
                             year=c(rep(2015, 2), rep(2016, 2), rep(2015, 2), rep(2016, 2)),
                             fish_species=c(rep(c("Chum", "Pink"), 4)),
                             vals=c(#"\\b{R^2*`=0.71,\np<0.001*`}",
                                    "R^2*`=0.71,\np<0.001*`",
                                    #"\\b{Rsup{2}=0.71,\np<0.001*}",
                                    #as.expression(bquote(~ R^2 ~ "=0.71,\np<0.001*")),
                                    "R^2*`=0.55,\np=0.001*`",
                                    "R^2*`=0.46,\np<0.001*`", "R^2*`=-0.03,\np=0.64`",
                                    "R^2*`=-0.02,\np=0.47`", "R^2*`=-0.01,\np=0.40`",
                                    "R^2*`=0.03,\np=0.20`", "R^2*`=0.34,\np=0.001*`"),
                             eqn=c("y=-16.2+0.21x", "y=-12.4+0.16x", "y=-2.5+0.05x", "y=0.6+0.01x",
                                   "y=-3.2+0.05x", "y=2.0+0.01x", "y=4.9-0.01x", "y=-2.8+0.05x"),
                             rs=c("R^2*`=0.71`", "R^2*`=0.55`", "R^2*`=0.46`", "R^2*`=-0.03`",
                                  "R^2*`=-0.02`", "R^2*`=-0.01`", "R^2*`=0.03`", "R^2*`=0.34`"),
                             ps=c("p<0.001*", "p=0.001*", "p<0.001*",
                                  "p=0.64", "p=0.47", "p=0.40", "p=0.20",
                                  "p=0.001*"),
                             sig=c(rep("Y", 3), rep("N", 4), "Y"))

size_season_colors_text <- left_join(size_season_colors, linear_results)

# ~ 0.5 R2 for pink and ~0.7 for chum (DI 2015; rest ~ 0 or hella low from looking at graph)

a1 <- size_season_colors %>%
  filter(fish_species=="Chum" & length_avg<20) %>%
  ggplot(aes(fork_length, length_avg))+
  geom_jitter(aes(color=codes), size=2#, show.legend=FALSE
  )+
  scale_color_manual(values=c("darkgoldenrod", "#d94801", "darkred", "#016c59", "#542788", "#053061"),
                     name="Site & Date"#, guide_legend(order=NULL)
  )+
  #guides(fill= guide_legend(override.aes = list(shape=21)))+
  #guides(color=FALSE)+
  facet_grid(site_id~year, scales = "free")+
  scale_x_continuous(limits=c(50, 150))+
  scale_y_continuous(limits=c(0, 15))+
  new_scale_color()+
  theme_bw()+
  labs(x="Salmon fork length (mm)", y="Mean prey size (mm)")+
  geom_smooth(method="lm", aes(color=fish_species), show.legend=FALSE
  )+
  scale_color_manual(values="#516959", name="Species", guide_legend(order=1))+
  geom_text(linear_results, mapping=aes(x=75, y=10, label=vals))+
  #guides(color=FALSE)+
  theme(legend.position = c(0.75, 0.8), panel.grid=element_blank(),
        strip.text = element_text(size=12),
        axis.title = element_text(size=10), axis.text = element_text(size=8),
        legend.text = element_text(size=8), legend.title = element_text(size=10))
#theme(legend.position = NULL, panel.grid=element_blank())
#a1
#size_season_colors$codes[which(size_season_colors$codes=="JS - Late June/\nEarly July")] <- "JS - Late June / Early July"

a2 <- size_season_colors %>%
  filter(fish_species=="Chum" & length_avg<20) %>% # rid of MAJOR outlier
  ggplot(aes(fork_length, length_avg))+
  geom_jitter(aes(color=codes), size=2, show.legend=FALSE
  )+
  scale_color_manual(values=c("darkgoldenrod", "#d94801", "darkred", "#016c59", "#542788", "#053061"),
                     name="Site & Date", guide_legend(order=NULL)
  )+
  #guides(fill= guide_legend(override.aes = list(shape=21)))+
  #guides(color=FALSE)+
  facet_grid(site_id~year, scales="free")+
  scale_x_continuous(limits=c(50, 150))+
  scale_y_continuous(expand=c(0, 0), limits=c(-0.1, 13.5))+
  new_scale_color()+
  theme_bw()+
  labs(x="Salmon fork length (mm)", y="Mean prey size (mm)")+
  geom_smooth(method="lm", aes(color=fish_species), show.legend=FALSE
  )+
  scale_color_manual(values="#516959", name="Species", guide_legend(order=1))+
  #guides(color=FALSE)+ 
  geom_text(filter(linear_results, fish_species=="Chum" & site_id=="D07"), mapping=aes(x=85, y=12.5, label=eqn), size=3, fontface=2)+
  geom_text(filter(linear_results, fish_species=="Chum" & site_id=="D07" & year=="2015"), mapping=aes(x=85, y=11.5),
            label=expression(bold("R"^"2"*"=0.71")), color="black", size=3, parse = TRUE )+
  geom_text(filter(linear_results, fish_species=="Chum" & site_id=="D07" & year=="2016"), mapping=aes(x=85, y=11.5),
            label=expression(bold("R"^"2"*"=0.46")), color="black", size=3, parse = TRUE)+
  geom_text(filter(linear_results, fish_species=="Chum" & site_id=="D07"), mapping=aes(x=85, y=10.5, label=ps), size=3, fontface=2)+
  geom_text(filter(linear_results, fish_species=="Chum" & site_id=="J07"), mapping=aes(x=85, y=12.5, label=eqn), size=3)+
  geom_text(filter(linear_results, fish_species=="Chum" & site_id=="J07"), mapping=aes(x=85, y=11.5, label=rs), size=3, parse = TRUE)+
  geom_text(filter(linear_results, fish_species=="Chum" & site_id=="J07"), mapping=aes(x=85, y=10.5, label=ps), size=3)+
  theme(legend.position = c(0.85, 0.93), panel.grid=element_blank(),
        axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"),
        strip.text = element_text(size=12),
        axis.title = element_text(size=10), axis.text = element_text(size=8),
        legend.text = element_text(size=8), legend.title = element_blank())
#theme(legend.position = NULL, panel.grid=element_blank())
a2
#size_season_colors$codes[which(size_season_colors$codes=="JS - Late June/\nEarly July")] <- "JS - Late June / Early July"

b <- size_season_colors %>%
  filter(fish_species=="Pink") %>%
  ggplot(aes(fork_length, length_avg))+
  geom_jitter(aes(color=codes), size=2, show.legend=FALSE
              )+
  scale_color_manual(values=c("darkgoldenrod", "#d94801", "darkred", "#016c59", "#542788", "#053061"),
                     name="Site & Date", guide_legend(order=2))+
  #guides(color=FALSE)+
  #guides(fill= guide_legend(override.aes = list(shape=21)))+
  facet_grid(site_id~year)+
  scale_x_continuous(limits=c(50, 150))+
  scale_y_continuous(expand=c(0, 0), limits=c(-0.1, 13.5))+
  guides(color=NULL)+
  new_scale_color()+
  theme_bw()+
  labs(x="Salmon fork length (mm)", y="Mean prey size (mm)")+
  geom_smooth(method="lm", aes(color=fish_species), show.legend=FALSE
              )+
  geom_text(filter(linear_results, fish_species=="Pink" & sig=="Y"), mapping=aes(x=85, y=12.5, label=eqn), size=3, fontface=2)+
  geom_text(filter(linear_results, fish_species=="Pink" & site_id=="D07" & year=="2015"), mapping=aes(x=85, y=11.5),
            label=expression(bold("R"^"2"*"=0.55")), color="black", size=3, parse = TRUE )+
  geom_text(filter(linear_results, fish_species=="Pink" & site_id=="J07" & year=="2016"), mapping=aes(x=85, y=11.5),
            label=expression(bold("R"^"2"*"=0.34")), color="black", size=3, parse = TRUE)+
  geom_text(filter(linear_results, fish_species=="Pink" & sig=="Y"), mapping=aes(x=85, y=10.5, label=ps), size=3, fontface=2)+
  geom_text(filter(linear_results, fish_species=="Pink" & sig=="N"), mapping=aes(x=85, y=12.5, label=eqn), size=3)+
  geom_text(filter(linear_results, fish_species=="Pink" & sig=="N"), mapping=aes(x=85, y=11.5, label=rs), size=3, parse = TRUE)+
  geom_text(filter(linear_results, fish_species=="Pink" & sig=="N"), mapping=aes(x=85, y=10.5, label=ps), size=3)+
  scale_color_manual(values="#d294af", name="Species", guide_legend(order=1))+
  theme(legend.position = c(0.85, 0.93), 
        panel.grid=element_blank(),
        legend.title = element_blank(),
        #legend.text=element_text(size=4),
        axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"),
        strip.text = element_text(size=12),
        axis.title = element_text(size=10), axis.text = element_text(size=8),
        legend.text = element_text(size=8)#, legend.title = element_text(size=12)
        )
b

#ggarrange(a, b, common.legend = TRUE)

legend_a1 <- ggpubr::get_legend(a1 + theme(legend.position="right", legend.box.margin = margin(0, 0, 0, 0)))
#legend_b <- ggpubr::get_legend(b + theme(legend.position="right"))

plot_grid(a2, b, labels = c("A)", "B)"), legend_a1, ncol = 3, rel_widths = c(1, 1, 0.3))

ggsave(here("figs", "temporal_figs", "predator_prey_ratios.png"), width=22, height=13, units = "cm", dpi=400)

temporal_diets %>%
  filter(length_avg>0)

size_comp_long <- temporal_diets %>%
  filter(food_weight_corr!=0, digestion_state==1) %>% 
  select(ufn, site_id, survey_date, year, fish_species, prey_weight_corr, size_class) %>%
  group_by(ufn, site_id, survey_date, year, fish_species, size_class) %>% 
  summarise(ww=sum(prey_weight_corr))

size_comp_wide <- size_comp_long %>%
  spread(key=size_class, value=ww, fill=0)

size_matrix <- size_comp_wide %>%
  ungroup() %>%
  select(-c(ufn, site_id, survey_date, fish_species, year)) %>%
  decostand("total") #%>%
  #gather(key=size, value=ww)

size_percent <- size_matrix*100

#size_info <- select(size_comp_long, ufn, site_id, survey_date, fish_species, year)

diet_size_biomass <- size_percent %>%
  mutate(Species=size_comp_wide$fish_species, Site=size_comp_wide$site_id,
         Date=size_comp_wide$survey_date, Year=size_comp_wide$year,
         ufn=size_comp_wide$ufn)

diet_size_biomass_ave <- diet_size_biomass %>%
  group_by(Species, Site, Date, Year, .drop = FALSE) %>% 
  gather("Size", "Biomass", `<1`:`>10`) %>%
  group_by(Species, Site, Date, Year, Size) %>% 
  summarise(Ave_Rel_Bio=mean(Biomass))

#size_trans <- cbind(size_info, size_matrix)

#diet_size_biomass_ave$Date <- format.Date(diet_size_biomass_ave$Date, "%m %b")

diet_size_biomass_ave$Date[which(diet_size_biomass_ave$Date=="2015-06-05")] <- "2015-06-04"
diet_size_biomass_ave$Date[which(diet_size_biomass_ave$Date=="2015-06-07")] <- "2015-06-04"
# merge these two groups for the sake of graphing and move back a bit to limit crowding

my_palette <- brewer.pal(name="Greys",n=9)[c(4:7, 9)]

my_palette_rev <- c("#000000", "#525252", "#737373", "#969696", "#BDBDBD")

my_palette_red <- c("#000000", "#a50f15",  "#ef3b2c", "#fc9272", "#fcbba1")

diet_size_biomass_ave %>%
  ggplot(aes(Date, Ave_Rel_Bio))+
  geom_bar(data = filter(diet_size_biomass_ave, Species=="Pink"), 
           mapping = aes(x = Date, y = Ave_Rel_Bio, fill = factor(Size, levels = size_levels_desc)), 
           stat="identity", position='stack', width = barwidth) + 
  geom_bar(data = filter(diet_size_biomass_ave, Species=="Chum"), 
           mapping = aes(x = Date + barwidth + 0.5, y = Ave_Rel_Bio, fill = factor(Size, levels = size_levels_desc)), 
           stat="identity", position='stack', width = barwidth)+
  scale_x_date(date_breaks = "17 days", date_labels = "%b %d", limits = c(NULL, NULL))+ 
  facet_grid(Site~Year, scales="free")+
  theme_bw()+
  scale_y_continuous(expand = c(0,0), limits=c(0, 110))+
  #geom_rect(data = diet_pink_graph, aes(xmin = Date - 1, xmax=Date+1, ymin = 0, ymax = 100), color="#d294af", fill=NA)+
  #geom_rect(data = diet_chum_graph, aes(xmin = Date+ barwidth - 0.5, xmax=Date+ barwidth+1.5, ymin = 0, ymax = 100), color="#516959", fill=NA)+
  geom_text(data = diet_pink_graph, aes(x = Date - 0.55, y = 105), label = "PI", color="#d294af")+
  geom_text(data = diet_chum_graph, aes(x = Date + barwidth + 1.2, y = 105), label = "CU", color="#516959")+
  #geom_vline(aes(xintercept=Date+1.25), linetype="dashed")+
  labs(y="Relative Biomass (%)", fill="Size Class")+
  scale_fill_manual(values=my_palette_red)+
  guides(fill=guide_legend(reverse = TRUE))+
  theme(panel.grid=element_blank(), axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"), strip.text = element_text(size=16),
        axis.ticks.x = element_blank(), axis.title = element_text(size=14),
        axis.text = element_text(size=12), legend.text = element_text(size=12),
        legend.title = element_text(size=14))

ggsave(here("figs", "temporal_figs", "temporal_size_comp.png"), width=22, height=13, units = "cm", dpi=400)

# figure out a better way to do this... I do think it needs to be per stom not per indiv prey...

# averaging by stomach doesn't work either since it depends on how many big/small prey groups...

# CALCUL:ATE A BARGRAPH OF REL BIO OF DIETS BY SIZE CLASS

length_histo_picu <- read_csv(here("processed", "all_years_lengths.csv"))

length_study_fish <- temp_stomachs %>% 
  filter(is.na(fork_length)!=TRUE) %>% 
  ungroup() %>% 
  select(species=fish_species, fork_length, year) %>%
  mutate(code="B) Study salmon")

length_histo_picu$species <- as.character(length_histo_picu$species)
length_histo_picu$species[which(length_histo_picu$species=="PI")] <- "Pink"
length_histo_picu$species[which(length_histo_picu$species=="CU")] <- "Chum"

length_study_fish$species <- length_study_fish$species %>% 
  fct_relevel("Pink", "Chum")

length_histo_picu$species <- length_histo_picu$species %>% 
  fct_relevel("Pink", "Chum")

length_all_fish <- length_histo_picu %>%
  select(species, fork_length, year) %>% 
  mutate(code="A) All salmon")

combined_fl_data <- rbind(length_all_fish, length_study_fish)

ggplot(combined_fl_data, aes(fork_length, y = (factor(year))), fill = "grey") +
  geom_density_ridges(color='black', scale = 3, alpha = 0.75,
                      aes(x=fork_length, y=factor(year), fill=species))+
  xlab("Fork Length (mm)") +
  facet_grid(species ~ code#, labeller = labeller(region = spp_labels, species = spp_labels)
             ) +
  ylab("Year") +
  scale_fill_manual(values=c("#d294af", "#516959"))+
  #  scale_fill_hakai() +
  scale_y_discrete(expand = expand_scale(add = c(0.2, 2.8))) +
  coord_cartesian(xlim = c(60, 160)) +
  guides(fill = FALSE) +
  theme_bw()+
  theme(panel.grid=element_blank(),
        axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"),
        strip.text = element_text(size=14),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size=12), axis.text = element_text(size=10),
        legend.text = element_text(size=10), legend.title = element_text(size=12))
  #ggtitle("Fork Length Frequency Distributions")
ggsave(here("figs", "temporal_figs", "lengths.png"), width=15, height=15, units="cm", dpi=800)

ggplot(temp_stomachs, aes(fork_length, y=(factor(year)), fill=fish_species))+
  geom_density_ridges(color="black", scale=3, alpha=1)+
  xlab("Fork Length (mm)") +
  facet_grid(fish_species ~ .#, labeller = labeller(region = spp_labels, species = spp_labels)
             ) +
  ylab("Year") +
  scale_fill_manual(values=c("#d294af", "#516959"))+
  #  scale_fill_hakai() +
  scale_y_discrete(expand = expand_scale(add = c(0.2, 2.8))) +
  coord_cartesian(xlim = c(60, 160)) +
  guides(fill = FALSE) +
  ggtitle("Fork Length Frequency Distributions")

##### INDVAL #####

library(labdsv)

row.names(clustdf) <- clustdf$`unlist(res$significantclusters)`

clust_names <- select(arrange(clustdf, `unlist(res$significantclusters)`), clusts)

clust_names$clusts

site_indval <- indval(temp_comm_matrix, clust_names$clusts)

summary(site_indval)

View(site_indval[["maxcls"]])

##### CALANOIDS #####

cala_comp_long <- temporal_diets %>%
  filter(food_weight_corr!=0, prey_group=="Calanoida") %>% 
  select(ufn, site_id, survey_date, year, fish_species, prey_weight_corr, prey_info) %>%
  group_by(ufn, site_id, survey_date, year, fish_species, prey_info) %>% 
  summarise(ww=sum(prey_weight_corr))

cala_comp_wide <- cala_comp_long %>%
  spread(key=prey_info, value=ww, fill=0)

cala_matrix <- cala_comp_wide %>%
  ungroup() %>%
  select(-c(ufn, site_id, survey_date, fish_species, year)) %>%
  decostand("total") #%>%
#gather(key=size, value=ww)

cala_percent <- cala_matrix*100

#size_info <- select(size_comp_long, ufn, site_id, survey_date, fish_species, year)

cala_biomass_ave <- cala_percent %>%
  mutate(Species=cala_comp_wide$fish_species, Site=cala_comp_wide$site_id,
         Date=cala_comp_wide$survey_date, Year=cala_comp_wide$year) %>%
  group_by(Species, Site, Date, Year, .drop = FALSE) %>% 
  gather("Cala", "Biomass", Acartia:Tortanus_discaudatus) %>%
  group_by(Species, Site, Date, Year, Cala) %>% 
  summarise(Ave_Rel_Bio=mean(Biomass))

#size_trans <- cbind(size_info, size_matrix)

#diet_size_biomass_ave$Date <- format.Date(diet_size_biomass_ave$Date, "%m %b")

cala_biomass_ave$Date[which(cala_biomass_ave$Date=="2015-06-05")] <- "2015-06-04"
cala_biomass_ave$Date[which(cala_biomass_ave$Date=="2015-06-07")] <- "2015-06-04"
# merge these two groups for the sake of graphing and move back a bit to limit crowding

#my_palette <- brewer.pal(name="Greys",n=9)[c(4:7, 9)]

cala_split <- str_split_fixed(cala_biomass_ave$Cala, "_", n=2)

all_calanoids <- cbind(cala_biomass_ave, Genus=cala_split[, 1])

calanoid_levels <- c("Acartia", "Aetideus", "Calanidae", "Calanoida", "Calanus", "Centropages", "Epilabidocera", "Eucalanus", "Mesocalanus", "Metridia", "Neocalanus", "Paracalanus", "Pseudocalanus", "Tortanus_discaudatus")

all_calanoids %>%
  group_by(Cala) %>%
  summarise(max=max(Ave_Rel_Bio)) %>%
  View()

all_calanoids %>%
  ggplot(aes(Date, Ave_Rel_Bio))+
  geom_bar(data = filter(all_calanoids, Species=="Pink"), 
           mapping = aes(x = Date, y = Ave_Rel_Bio, fill = Genus), 
           stat="identity", position='stack', width = barwidth) + 
  geom_bar(data = filter(all_calanoids, Species=="Chum"), 
           mapping = aes(x = Date + barwidth + 0.5, y = Ave_Rel_Bio, fill = Genus), 
           stat="identity", position='stack', width = barwidth)+
  scale_x_date(date_breaks = "17 days", date_labels = "%b %d", limits = c(NULL, NULL))+ 
  facet_grid(Site~Year, scales="free")+
  theme_bw()+
  scale_y_continuous(expand = c(0,0), limits=c(0, 110))+
  #geom_rect(data = diet_pink_graph, aes(xmin = Date - 1, xmax=Date+1, ymin = 0, ymax = 100), color="#d294af", fill=NA)+
  #geom_rect(data = diet_chum_graph, aes(xmin = Date+ barwidth - 0.5, xmax=Date+ barwidth+1.5, ymin = 0, ymax = 100), color="#516959", fill=NA)+
  geom_text(data = diet_pink_graph, aes(x = Date - 0.55, y = 105), label = "PI", color="#d294af")+
  geom_text(data = diet_chum_graph, aes(x = Date + barwidth + 1.2, y = 105), label = "CU", color="#516959")+
  #geom_vline(aes(xintercept=Date+1.25), linetype="dashed")+
  labs(y="Relative Biomass (%)", fill="Calanoids")+
  #scale_fill_manual(values=my_palette)+
  theme(panel.grid=element_blank(), axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"), strip.text = element_text(size=16),
        axis.ticks.x = element_blank(), axis.title = element_text(size=14),
        axis.text = element_text(size=12), legend.text = element_text(size=12),
        legend.title = element_text(size=14))

ggsave(here("figs", "temporal_figs", "temporal_cala_comp.png"))


##### PREY CUM CURVES #####

PI_D07_15_matrix <- temp_diet_wide_nmds %>%
  filter(site_id=="D07" & fish_species=="Pink" & year=="2015") %>% 
  select(Acartia:Tortanus_discaudatus)

PI_D07_16_matrix <- temp_diet_wide_nmds %>%
  filter(site_id=="D07" & fish_species=="Pink" & year=="2016") %>% 
  select(Acartia:Tortanus_discaudatus)

PI_J07_15_matrix <- temp_diet_wide_nmds %>%
  filter(site_id=="J07" & fish_species=="Pink" & year=="2015") %>% 
  select(Acartia:Tortanus_discaudatus)

PI_J07_16_matrix <- temp_diet_wide_nmds %>%
  filter(site_id=="J07" & fish_species=="Pink" & year=="2016") %>% 
  select(Acartia:Tortanus_discaudatus)

CU_D07_15_matrix <- temp_diet_wide_nmds %>%
  filter(site_id=="D07" & fish_species=="Chum" & year=="2015") %>% 
  select(Acartia:Tortanus_discaudatus)

CU_D07_16_matrix <- temp_diet_wide_nmds %>%
  filter(site_id=="D07" & fish_species=="Chum" & year=="2016") %>% 
  select(Acartia:Tortanus_discaudatus)

CU_J07_15_matrix <- temp_diet_wide_nmds %>%
  filter(site_id=="J07" & fish_species=="Chum" & year=="2015") %>% 
  select(Acartia:Tortanus_discaudatus)

CU_J07_16_matrix <- temp_diet_wide_nmds %>%
  filter(site_id=="J07" & fish_species=="Chum" & year=="2016") %>% 
  select(Acartia:Tortanus_discaudatus)

PI_D07_15_curve <- specaccum(PI_D07_15_matrix)
PI_D07_16_curve <- specaccum(PI_D07_16_matrix)
PI_J07_15_curve <- specaccum(PI_J07_15_matrix)
PI_J07_16_curve <- specaccum(PI_J07_16_matrix)
CU_D07_15_curve <- specaccum(CU_D07_15_matrix)
CU_D07_16_curve <- specaccum(CU_D07_16_matrix)
CU_J07_15_curve <- specaccum(CU_J07_15_matrix)
CU_J07_16_curve <- specaccum(CU_J07_16_matrix)

PI_D07_15_curve_data <- data.frame(Fish=PI_D07_15_curve$sites, Richness=PI_D07_15_curve$richness, SD=PI_D07_15_curve$sd, Species="Pink", Site="D07", Year="2015")
PI_D07_16_curve_data <- data.frame(Fish=PI_D07_16_curve$sites, Richness=PI_D07_16_curve$richness, SD=PI_D07_16_curve$sd, Species="Pink", Site="D07", Year="2016")
PI_J07_15_curve_data <- data.frame(Fish=PI_J07_15_curve$sites, Richness=PI_J07_15_curve$richness, SD=PI_J07_15_curve$sd, Species="Pink", Site="J07", Year="2015")
PI_J07_16_curve_data <- data.frame(Fish=PI_J07_16_curve$sites, Richness=PI_J07_16_curve$richness, SD=PI_J07_16_curve$sd, Species="Pink", Site="J07", Year="2016")
CU_D07_15_curve_data <- data.frame(Fish=CU_D07_15_curve$sites, Richness=CU_D07_15_curve$richness, SD=CU_D07_15_curve$sd, Species="Chum", Site="D07", Year="2015")
CU_D07_16_curve_data <- data.frame(Fish=CU_D07_16_curve$sites, Richness=CU_D07_16_curve$richness, SD=CU_D07_16_curve$sd, Species="Chum", Site="D07", Year="2016")
CU_J07_15_curve_data <- data.frame(Fish=CU_J07_15_curve$sites, Richness=CU_J07_15_curve$richness, SD=CU_J07_15_curve$sd, Species="Chum", Site="J07", Year="2015")
CU_J07_16_curve_data <- data.frame(Fish=CU_J07_16_curve$sites, Richness=CU_J07_16_curve$richness, SD=CU_J07_16_curve$sd, Species="Chum", Site="J07", Year="2016")

all_curve_data <- rbind(PI_D07_15_curve_data, PI_D07_16_curve_data, PI_J07_15_curve_data, PI_J07_16_curve_data,
                        CU_D07_15_curve_data, CU_D07_16_curve_data, CU_J07_15_curve_data, CU_J07_16_curve_data)

#all_curve_data$Site <- factor(all_curve_data$Site, levels=spat_site_order)
#all_curve_data$Year <- factor(all_curve_data$Site, levels=spat_site_order)
all_curve_data$Species <- factor(all_curve_data$Species, levels=c("Pink", "Chum"))

ggplot(all_curve_data)+
  geom_line(aes(x=Fish, y=Richness, color=Site, group=interaction(Site, Species, Year))) +
  geom_point(aes(x=Fish, y=Richness, shape=Species,
                 color=Site), fill="white", 
             size=3, stroke=2) +
  #geom_ribbon(aes(x=all_curve_data$Fish, ymin=(all_curve_data$Richness-1*all_curve_data$SD),ymax=(all_curve_data$Richness+1*all_curve_data$SD), fill=Site, group=interaction(Site, Species)),alpha=0.2)+
  scale_color_manual(values=c("darkred", "#053061"), name="Site",
                     guide = guide_legend(reverse = F)) +
  scale_shape_manual(values=c(21, 16), name="Species")+
  labs(y="Richness (# of Prey Taxa)", x="# of Stomachs")+
  theme_bw()+
  facet_wrap(~Year)+
  theme(panel.grid=element_blank(),
        axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"),
        strip.text = element_text(size=14),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size=12), axis.text = element_text(size=10),
        legend.text = element_text(size=10), legend.title = element_text(size=12))

ggsave(here("figs", "temporal_figs", "temporal_cum_prey_curves.png"), width=15, height=15, units="cm", dpi=800)

#separated by each date (might not work with sample sizes tho...?)

PI_D07_15_matrix_A <- temp_diet_wide_nmds %>%
  filter(site_id=="D07" & fish_species=="Pink" & year=="2015" & survey_date=="2015-05-21") %>% 
  select(Acartia:Tortanus_discaudatus)

PI_D07_15_matrix_B <- temp_diet_wide_nmds %>%
  filter(site_id=="D07" & fish_species=="Pink" & year=="2015" & survey_date=="2015-06-07") %>% 
  select(Acartia:Tortanus_discaudatus)

PI_D07_15_matrix_C <- temp_diet_wide_nmds %>%
  filter(site_id=="D07" & fish_species=="Pink" & year=="2015" & survey_date=="2015-06-13") %>% 
  select(Acartia:Tortanus_discaudatus)

PI_D07_16_matrix_A <- temp_diet_wide_nmds %>%
  filter(site_id=="D07" & fish_species=="Pink" & year=="2016" & survey_date=="2016-05-19") %>% 
  select(Acartia:Tortanus_discaudatus)

PI_D07_16_matrix_B <- temp_diet_wide_nmds %>%
  filter(site_id=="D07" & fish_species=="Pink" & year=="2016" & survey_date=="2016-06-03") %>% 
  select(Acartia:Tortanus_discaudatus)

PI_D07_16_matrix_C <- temp_diet_wide_nmds %>%
  filter(site_id=="D07" & fish_species=="Pink" & year=="2016" & survey_date=="2016-06-16") %>% 
  select(Acartia:Tortanus_discaudatus)

PI_J07_15_matrix_A <- temp_diet_wide_nmds %>%
  filter(site_id=="J07" & fish_species=="Pink" & year=="2015" & survey_date=="2015-06-02") %>% 
  select(Acartia:Tortanus_discaudatus)

PI_J07_15_matrix_B <- temp_diet_wide_nmds %>%
  filter(site_id=="J07" & fish_species=="Pink" & year=="2015" & survey_date=="2015-06-14") %>% 
  select(Acartia:Tortanus_discaudatus)

PI_J07_15_matrix_C <- temp_diet_wide_nmds %>%
  filter(site_id=="J07" & fish_species=="Pink" & year=="2015" & survey_date=="2015-06-29") %>% 
  select(Acartia:Tortanus_discaudatus)

PI_J07_16_matrix_A <- temp_diet_wide_nmds %>%
  filter(site_id=="J07" & fish_species=="Pink" & year=="2016" & survey_date=="2016-06-03") %>% 
  select(Acartia:Tortanus_discaudatus)

PI_J07_16_matrix_B <- temp_diet_wide_nmds %>%
  filter(site_id=="J07" & fish_species=="Pink" & year=="2016" & survey_date=="2016-06-20") %>% 
  select(Acartia:Tortanus_discaudatus)

PI_J07_16_matrix_C <- temp_diet_wide_nmds %>%
  filter(site_id=="J07" & fish_species=="Pink" & year=="2016" & survey_date=="2016-07-05") %>% 
  select(Acartia:Tortanus_discaudatus)

CU_D07_15_matrix_A <- temp_diet_wide_nmds %>%
  filter(site_id=="D07" & fish_species=="Chum" & year=="2015" & survey_date=="2015-05-21") %>% 
  select(Acartia:Tortanus_discaudatus)

CU_D07_15_matrix_B <- temp_diet_wide_nmds %>%
  filter(site_id=="D07" & fish_species=="Chum" & year=="2015" & survey_date=="2015-06-05") %>% 
  select(Acartia:Tortanus_discaudatus)

CU_D07_15_matrix_C <- temp_diet_wide_nmds %>%
  filter(site_id=="D07" & fish_species=="Chum" & year=="2015" & survey_date=="2015-06-13") %>% 
  select(Acartia:Tortanus_discaudatus)

CU_D07_16_matrix_A <- temp_diet_wide_nmds %>%
  filter(site_id=="D07" & fish_species=="Chum" & year=="2016" & survey_date=="2016-05-19") %>% 
  select(Acartia:Tortanus_discaudatus)

CU_D07_16_matrix_B <- temp_diet_wide_nmds %>%
  filter(site_id=="D07" & fish_species=="Chum" & year=="2016" & survey_date=="2016-06-03") %>% 
  select(Acartia:Tortanus_discaudatus)

CU_D07_16_matrix_C <- temp_diet_wide_nmds %>%
  filter(site_id=="D07" & fish_species=="Chum" & year=="2016" & survey_date=="2016-06-16") %>% 
  select(Acartia:Tortanus_discaudatus)

CU_J07_15_matrix_A <- temp_diet_wide_nmds %>%
  filter(site_id=="J07" & fish_species=="Chum" & year=="2015" & survey_date=="2015-06-02") %>% 
  select(Acartia:Tortanus_discaudatus)

CU_J07_15_matrix_B <- temp_diet_wide_nmds %>%
  filter(site_id=="J07" & fish_species=="Chum" & year=="2015" & survey_date=="2015-06-14") %>% 
  select(Acartia:Tortanus_discaudatus)

CU_J07_15_matrix_C <- temp_diet_wide_nmds %>%
  filter(site_id=="J07" & fish_species=="Chum" & year=="2015" & survey_date=="2015-06-29") %>% 
  select(Acartia:Tortanus_discaudatus)

CU_J07_16_matrix_A <- temp_diet_wide_nmds %>%
  filter(site_id=="J07" & fish_species=="Chum" & year=="2016" & survey_date=="2016-06-03") %>% 
  select(Acartia:Tortanus_discaudatus)

CU_J07_16_matrix_B <- temp_diet_wide_nmds %>%
  filter(site_id=="J07" & fish_species=="Chum" & year=="2016" & survey_date=="2016-06-20") %>% 
  select(Acartia:Tortanus_discaudatus)

CU_J07_16_matrix_C <- temp_diet_wide_nmds %>%
  filter(site_id=="J07" & fish_species=="Chum" & year=="2016" & survey_date=="2016-07-05") %>% 
  select(Acartia:Tortanus_discaudatus)

PI_D07_15_curve_A <- specaccum(PI_D07_15_matrix_A)
PI_D07_16_curve_A <- specaccum(PI_D07_16_matrix_A)
PI_J07_15_curve_A <- specaccum(PI_J07_15_matrix_A)
PI_J07_16_curve_A <- specaccum(PI_J07_16_matrix_A)
CU_D07_15_curve_A <- specaccum(CU_D07_15_matrix_A)
CU_D07_16_curve_A <- specaccum(CU_D07_16_matrix_A)
CU_J07_15_curve_A <- specaccum(CU_J07_15_matrix_A)
CU_J07_16_curve_A <- specaccum(CU_J07_16_matrix_A)

PI_D07_15_curve_B <- specaccum(PI_D07_15_matrix_B)
PI_D07_16_curve_B <- specaccum(PI_D07_16_matrix_B)
PI_J07_15_curve_B <- specaccum(PI_J07_15_matrix_B)
PI_J07_16_curve_B <- specaccum(PI_J07_16_matrix_B)
CU_D07_15_curve_B <- specaccum(CU_D07_15_matrix_B)
CU_D07_16_curve_B <- specaccum(CU_D07_16_matrix_B)
CU_J07_15_curve_B <- specaccum(CU_J07_15_matrix_B)
CU_J07_16_curve_B <- specaccum(CU_J07_16_matrix_B)

PI_D07_15_curve_C <- specaccum(PI_D07_15_matrix_C)
PI_D07_16_curve_C <- specaccum(PI_D07_16_matrix_C)
PI_J07_15_curve_C <- specaccum(PI_J07_15_matrix_C)
PI_J07_16_curve_C <- specaccum(PI_J07_16_matrix_C)
CU_D07_15_curve_C <- specaccum(CU_D07_15_matrix_C)
CU_D07_16_curve_C <- specaccum(CU_D07_16_matrix_C)
CU_J07_15_curve_C <- specaccum(CU_J07_15_matrix_C)
CU_J07_16_curve_C <- specaccum(CU_J07_16_matrix_C)

PI_D07_15_curve_data_A <- data.frame(Fish=PI_D07_15_curve_A$sites, Richness=PI_D07_15_curve_A$richness, #SD=PI_D07_15_curve_A$sd,
                                     Species="Pink", Site="D07", Year="2015", code="A")
PI_D07_16_curve_data_A <- data.frame(Fish=PI_D07_16_curve_A$sites, Richness=PI_D07_16_curve_A$richness, #SD=PI_D07_16_curve_A$sd,
                                     Species="Pink", Site="D07", Year="2016", code="A")
PI_J07_15_curve_data_A <- data.frame(Fish=PI_J07_15_curve_A$sites, Richness=PI_J07_15_curve_A$richness,# SD=PI_J07_15_curve_A$sd,
                                     Species="Pink", Site="J07", Year="2015", code="D")
PI_J07_16_curve_data_A <- data.frame(Fish=PI_J07_16_curve_A$sites, Richness=PI_J07_16_curve_A$richness,# SD=PI_J07_16_curve_A$sd,
                                     Species="Pink", Site="J07", Year="2016", code="D")
CU_D07_15_curve_data_A <- data.frame(Fish=CU_D07_15_curve_A$sites, Richness=CU_D07_15_curve_A$richness,# SD=CU_D07_15_curve_A$sd,
                                     Species="Chum", Site="D07", Year="2015", code="A")
CU_D07_16_curve_data_A <- data.frame(Fish=CU_D07_16_curve_A$sites, Richness=CU_D07_16_curve_A$richness,# SD=CU_D07_16_curve_A$sd,
                                     Species="Chum", Site="D07", Year="2016", code="A")
CU_J07_15_curve_data_A <- data.frame(Fish=CU_J07_15_curve_A$sites, Richness=CU_J07_15_curve_A$richness,# SD=CU_J07_15_curve_A$sd,
                                     Species="Chum", Site="J07", Year="2015", code="D")
CU_J07_16_curve_data_A <- data.frame(Fish=CU_J07_16_curve_A$sites, Richness=CU_J07_16_curve_A$richness,# SD=CU_J07_16_curve_A$sd,
                                     Species="Chum", Site="J07", Year="2016", code="D")

PI_D07_15_curve_data_B <- data.frame(Fish=PI_D07_15_curve_B$sites, Richness=PI_D07_15_curve_B$richness,# SD=PI_D07_15_curve_B$sd,
                                     Species="Pink", Site="D07", Year="2015", code="B")
PI_D07_16_curve_data_B <- data.frame(Fish=PI_D07_16_curve_B$sites, Richness=PI_D07_16_curve_B$richness,# SD=PI_D07_16_curve_B$sd,
                                     Species="Pink", Site="D07", Year="2016", code="B")
PI_J07_15_curve_data_B <- data.frame(Fish=PI_J07_15_curve_B$sites, Richness=PI_J07_15_curve_B$richness,# SD=PI_J07_15_curve_B$sd,
                                     Species="Pink", Site="J07", Year="2015", code="E")
PI_J07_16_curve_data_B <- data.frame(Fish=PI_J07_16_curve_B$sites, Richness=PI_J07_16_curve_B$richness,# SD=PI_J07_16_curve_B$sd,
                                     Species="Pink", Site="J07", Year="2016", code="E")
CU_D07_15_curve_data_B <- data.frame(Fish=CU_D07_15_curve_B$sites, Richness=CU_D07_15_curve_B$richness,# SD=CU_D07_15_curve_B$sd,
                                     Species="Chum", Site="D07", Year="2015", code="B")
CU_D07_16_curve_data_B <- data.frame(Fish=CU_D07_16_curve_B$sites, Richness=CU_D07_16_curve_B$richness,# SD=CU_D07_16_curve_B$sd,
                                     Species="Chum", Site="D07", Year="2016", code="B")
CU_J07_15_curve_data_B <- data.frame(Fish=CU_J07_15_curve_B$sites, Richness=CU_J07_15_curve_B$richness,# SD=CU_J07_15_curve_B$sd,
                                     Species="Chum", Site="J07", Year="2015", code="E")
CU_J07_16_curve_data_B <- data.frame(Fish=CU_J07_16_curve_B$sites, Richness=CU_J07_16_curve_B$richness,# SD=CU_J07_16_curve_B$sd,
                                     Species="Chum", Site="J07", Year="2016", code="E")

PI_D07_15_curve_data_C <- data.frame(Fish=PI_D07_15_curve_C$sites, Richness=PI_D07_15_curve_C$richness,# SD=PI_D07_15_curve_C$sd,
                                     Species="Pink", Site="D07", Year="2015", code="C")
PI_D07_16_curve_data_C <- data.frame(Fish=PI_D07_16_curve_C$sites, Richness=PI_D07_16_curve_C$richness,# SD=PI_D07_16_curve_C$sd,
                                     Species="Pink", Site="D07", Year="2016", code="C")
PI_J07_15_curve_data_C <- data.frame(Fish=PI_J07_15_curve_C$sites, Richness=PI_J07_15_curve_C$richness,# SD=PI_J07_15_curve_C$sd,
                                     Species="Pink", Site="J07", Year="2015", code="F")
PI_J07_16_curve_data_C <- data.frame(Fish=PI_J07_16_curve_C$sites, Richness=PI_J07_16_curve_C$richness,# SD=PI_J07_16_curve_C$sd,
                                     Species="Pink", Site="J07", Year="2016", code="F")
CU_D07_15_curve_data_C <- data.frame(Fish=CU_D07_15_curve_C$sites, Richness=CU_D07_15_curve_C$richness,# SD=CU_D07_15_curve_C$sd,
                                     Species="Chum", Site="D07", Year="2015", code="C")
CU_D07_16_curve_data_C <- data.frame(Fish=CU_D07_16_curve_C$sites, Richness=CU_D07_16_curve_C$richness,# SD=CU_D07_16_curve_C$sd,
                                     Species="Chum", Site="D07", Year="2016", code="C")
CU_J07_15_curve_data_C <- data.frame(Fish=CU_J07_15_curve_C$sites, Richness=CU_J07_15_curve_C$richness,# SD=CU_J07_15_curve_C$sd,
                                     Species="Chum", Site="J07", Year="2015", code="F")
CU_J07_16_curve_data_C <- data.frame(Fish=CU_J07_16_curve_C$sites, Richness=CU_J07_16_curve_C$richness,# SD=CU_J07_16_curve_C$sd,
                                     Species="Chum", Site="J07", Year="2016", code="F")

all_curve_data <- rbind(PI_D07_15_curve_data_A, PI_D07_16_curve_data_A, PI_J07_15_curve_data_A, PI_J07_16_curve_data_A,
                        CU_D07_15_curve_data_A, CU_D07_16_curve_data_A, CU_J07_15_curve_data_A, CU_J07_16_curve_data_A,
                        PI_D07_15_curve_data_B, PI_D07_16_curve_data_B, PI_J07_15_curve_data_B, PI_J07_16_curve_data_B,
                        CU_D07_15_curve_data_B, CU_D07_16_curve_data_B, CU_J07_15_curve_data_B, CU_J07_16_curve_data_B,
                        PI_D07_15_curve_data_C, PI_D07_16_curve_data_C, PI_J07_15_curve_data_C, PI_J07_16_curve_data_C,
                        CU_D07_15_curve_data_C, CU_D07_16_curve_data_C, CU_J07_15_curve_data_C, CU_J07_16_curve_data_C)

#all_curve_data$Site <- factor(all_curve_data$Site, levels=spat_site_order)
#all_curve_data$Year <- factor(all_curve_data$Site, levels=spat_site_order)
all_curve_data$Species <- factor(all_curve_data$Species, levels=c("Pink", "Chum"))

ggplot(all_curve_data)+
  geom_line(aes(x=Fish, y=Richness, color=code, #color=Site,
                group=interaction(Site, Species, Year, code))) +
  geom_point(aes(x=Fish, y=Richness, shape=Species,
                 color=code#color=Site
                 ), fill="white", 
             size=3, stroke=2) +
  #geom_ribbon(aes(x=all_curve_data$Fish, ymin=(all_curve_data$Richness-1*all_curve_data$SD),ymax=(all_curve_data$Richness+1*all_curve_data$SD), fill=Site, group=interaction(Site, Species)),alpha=0.2)+
  scale_color_manual(values=c(#"darkred", "#053061"), name="Site",
                     "darkgoldenrod", "#d94801", "darkred",
                     "#016c59", "#542788", "#053061"), name="Site & Date",
                     guide = guide_legend(reverse = F)) +
  scale_shape_manual(values=c(21, 16), name="Species")+
  labs(y="Richness (# of Prey Taxa)", x="# of Stomachs")+
  theme_bw()+
  facet_wrap(~Year)+
  theme(panel.grid=element_blank(),
        axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"),
        strip.text = element_text(size=14),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size=12), axis.text = element_text(size=10),
        legend.text = element_text(size=10), legend.title = element_text(size=12))

ggsave(here("figs", "temporal_figs", "temporal_cum_prey_curves_detail.png"), width=15, height=15, units="cm", dpi=800)

#overwhelming amount of detail (don't use unless reeeeeally want to dive into date specifics...)

##### PLASTICS #####

plastics <- temporal_diets %>%
  mutate(weight_corr= weight*1000, # grams to milligrams? * FIX IN RAW DATA LATER ! *
         gfi=food_weight_corr/weight_corr*100) %>% 
  filter(prey_group=="Object")

#u5240 glass; u5305 macro; u5273 film; U5413 tag

# figure out freq of occurance; relationship to gfi/empties; rel bio; other stuff?