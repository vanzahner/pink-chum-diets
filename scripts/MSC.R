#First Draft of MSC Data Analysis Script! For basic in progress graphs!#

#updated Dec 31 2019
# will be taking this long script apart into relevant smaller scripts for analyses *

##### Set up --> data_wrangling.R #####

rm(list=ls())
#remove other R stuff

library(readr)
#read in files
library(ggplot2)
#graphs
#library(cowplot)
#pretty graphs
library(tidyverse)
#data manip
library(RColorBrewer)
#nicer colors
library(lemon)
#repeat axis in facet wrap and grid

#devtools::install_github("HakaiInstitute/hakaisalmon", auth_token = "23cbdd116ee9b4186200c2461834faee67dae43c")

setwd("/Users/Vanessa/Desktop/msc_project")
#set working directory

mscdata <- read_csv("data/pink_chum_diets_raw_data.csv")
#read in file (simplified version of excel sheet)

metadata <- read_csv("data/pink_chum_fish_info_filtered_data.csv")
#new version - edited hakai_id to be ufn

seinedata <- read_csv("data/pink_chum_seine_raw_data.csv")
#seine data for lat long info

fishdata <- left_join(mscdata, metadata, by=c("ufn", "semsp_id")) %>%
  filter(taxa_detail_calc!="Goop")
                      #by="semsp_id")
#join tables to merge the meta data with the diet data!

temp_fish <- filter(fishdata, analysis!="Spatial" & taxa_detail_calc!="Goop")

spat_fish <- filter(fishdata, analysis!="Temporal"& taxa_detail_calc!="Goop")

##### Transforming Data #####

#load in file with old and new taxa names to be assigned
fish_names<-read.csv("data/taxa_groups_all_fish.csv") 

#for loop doesn't like data as factors
fishdata$taxa_detail_calc <- as.character(fishdata$taxa_detail_calc) 
fish_names$taxa_detail_calc <- as.character(fish_names$taxa_detail_calc)
fish_names$taxa_group <- as.character(fish_names$taxa_group)

#for loop that will go through all the organism names in the data spreadsheet 
#and for each one it will go to the names spreadsheet and reassign the name accordingly
for (n in fish_names$taxa_detail_calc) {
  fishdata$taxa_detail_calc[which(fishdata$taxa_detail_calc %in% n)] <- fish_names$taxa_group[which(fish_names$taxa_detail_calc == n)]
}

unique(fishdata$taxa_detail_calc)

# NEED TO RESOLVE CALANOID SIZE ISSUE AND MONSTRILLOIDS ONLY N=2 ????? MYSIDS=3 IS OK?

numbers_taxa <- fishdata %>%
  ungroup() %>%
  count(taxa_detail_calc)

biomass <- fishdata %>%
  group_by(ufn, fish_species, sample_date, sample_site, taxa_group, analysis,# taxa_detail_calc, 
           semsp_id, year, sampling_week, bolus_weight, weight, fork_length) %>%
  summarise(prey_weight_sum=sum(prey_weight))
#summarize biomass for each fish

#%>%
#filter(Biomass!=0) #remove empty stom

mscwide <- biomass %>%
  ungroup() %>%
  group_by(ufn, fish_species, sample_site) %>% 
  pivot_wider(names_from = taxa_group, values_from = prey_weight_sum, values_fill = list(prey_weight_sum = 0))

spat_sites <- biomass %>%
  filter(sample_site %in% c("D11", "D09", "J02", "J06", "J08"))

temp_site <- biomass %>% 
  filter(sample_site == "D07" & sample_date=="2016-06-16")

biom_graph <- rbind(spat_sites, temp_site)

biom_graph$sample_site <- as.factor(biom_graph$sample_site)

biom_graph$sample_site <- factor(biom_graph$sample_site, levels(biom_graph$sample_site)[c(4, 6, 5, 3, 2, 1)])

#date_order <- c("May 5", "May 12", "May 19", "May 26", "June 2", "June 9",
#								"June 16", "June 23", "June 30", "July 6", "July 13")

date_order <- c("05-May", "12-May", "19-May", "26-May", "02-Jun", "09-Jun",
                "16-Jun", "23-Jun", "30-Jun", "06-Jul", "13-Jul")
#weird formatting for some reason - fix later
biomass$sampling_week <- factor(biomass$sampling_week, levels = date_order)

taxa_order <- c("Barnacles", "Calanoids", "Decapods", "Euphausiids",
                "Euph_eggs", "Cladocerans", "Echinoderms",
                "Chaetognaths", "Gelatinous", "Larvaceans", "Other")

#biomass$`Taxonomic Group` <- factor(biomass$`Taxonomic Group`, levels = taxa_order)

#biom_graph$`Taxonomic Group` <- factor(biom_graph$`Taxonomic Group`, levels = taxa_order)

#need to resolve taxa_order and list of groups before reordering the levels and whatnot

##### Overlap #####

biom_psi <- fish %>%
  filter(Analysis!="Temporal" & `Total Stomach Content Weight (mg)` > 0.5
         & semsp_id!="2016-06-14-D09-CU-7" & `Taxonomic Group` != "Digested"
         & `Taxonomic Group` != "Parasites" & `Taxonomic Group` != "Detritus"
         & `Taxonomic Group` != "Objects" & `Taxonomic Group` !="Echinoderms") %>% 
  group_by(UFN, `Sample Site`, `Sample Date`, `Fish Species`, semsp_id, `Taxonomic Group`) %>%
  summarise(WWeight=sum(`Prey Total Weight (mg)`)) %>%
  spread(`Taxonomic Group`, WWeight, fill = 0)

biom_labels <- biom_psi$semsp_id

#labels_date <- biom_psi$`Sample Date`

labels_site <- biom_psi$`Sample Site`

labels_species <- biom_psi$`Fish Species`

biom_nums <- biom_psi %>%
  ungroup() %>% 
  select(Amphipods:Decapods, Diatoms, Euphausiids:Larvaceans, Ostracods:Pteropods)

biom_matrix <- as.matrix(biom_nums)

#rownames(biom_matrix) <- biom_labels

biom_totals <- rowSums(biom_matrix)

biom_totalmat <- (biom_matrix/biom_totals*100)

biom_df <- as.data.frame(biom_totalmat)

biom_join <- cbind(labels_site, labels_species, biom_df) %>%
  group_by(labels_site, labels_species) %>% 
  summarise(Amphipods=sum(Amphipods),
            Barnacles=sum(Barnacles),
            Bivalves=sum(Bivalves),
            Calanoids=sum(Calanoids),
            Chaetognaths=sum(Chaetognaths),
            Cladocerans=sum(Cladocerans),
            Cumacean=sum(Cumacean),
            Cyclopoids=sum(Cyclopoids),
            Decapods=sum(Decapods),
            Diatoms=sum(Diatoms),
            Euphausiids=sum(Euphausiids),
            Fish=sum(Fish),
            Gelatinous=sum(Gelatinous),
            Harpacticoids=sum(Harpacticoids),
            Insects=sum(Insects),
            Isopods=sum(Isopods), 
            Larvaceans=sum(Larvaceans),
            Ostracods=sum(Ostracods),
            Polychaetes=sum(Polychaetes),
            Pteropods=sum(Pteropods))

newlabels <- c("D07CU", "D07PI", "D09CU", "D09PI", "D11CU", "D11PI", "J02CU", "J02PI", "J06CU", "J06PI", "J08CU", "J08PI")

new_nums <- biom_join %>%
  ungroup %>%
  select(Amphipods:Pteropods)

new_matrix <- as.matrix(new_nums)

new_totals <- rowSums(new_matrix)

new_proportions <- new_matrix/new_totals

new_df <- as.data.frame(new_proportions)

new_combined <- cbind(newlabels, new_df)

D07CU <- new_combined[1, ]
D07PI <- new_combined[2, ]
D09CU <- new_combined[3, ]
D09PI <- new_combined[4, ]
D11CU <- new_combined[5, ]
D11PI <- new_combined[6, ]
J02CU <- new_combined[7, ]
J02PI <- new_combined[8, ]
J06CU <- new_combined[9, ]
J06PI <- new_combined[10, ]
J08CU <- new_combined[11, ]
J08PI <- new_combined[12, ]

D07psi <- 1-(0.5*(
  abs(D07PI$Amphipods-D07CU$Amphipods) +
    abs(D07PI$Barnacles-D07CU$Barnacles) +
    abs(D07PI$Bivalves-D07CU$Bivalves) +
    abs(D07PI$Calanoids-D07CU$Calanoids) +
    abs(D07PI$Chaetognaths-D07CU$Chaetognaths) +
    abs(D07PI$Cladocerans-D07CU$Cladocerans) +
    abs(D07PI$Cumacean-D07CU$Cumacean) +
    abs(D07PI$Cyclopoids-D07CU$Cyclopoids) +
    abs(D07PI$Decapods-D07CU$Decapods) +
    abs(D07PI$Diatoms-D07CU$Diatoms) +
    abs(D07PI$Euphausiids-D07CU$Euphausiids) +
    abs(D07PI$Fish-D07CU$Fish) +
    abs(D07PI$Gelatinous-D07CU$Gelatinous) +
    abs(D07PI$Harpacticoids-D07CU$Harpacticoids) +
    abs(D07PI$Insects-D07CU$Insects) +
    abs(D07PI$Isopods-D07CU$Isopods) +
    abs(D07PI$Larvaceans-D07CU$Larvaceans) +
    abs(D07PI$Ostracods-D07CU$Ostracods) +
    abs(D07PI$Polychaetes-D07CU$Polychaetes) +
    abs(D07PI$Pteropods-D07CU$Pteropods)
))

D09psi <- 1-(0.5*(
  abs(D09PI$Amphipods-D09CU$Amphipods) +
    abs(D09PI$Barnacles-D09CU$Barnacles) +
    abs(D09PI$Bivalves-D09CU$Bivalves) +
    abs(D09PI$Calanoids-D09CU$Calanoids) +
    abs(D09PI$Chaetognaths-D09CU$Chaetognaths) +
    abs(D09PI$Cladocerans-D09CU$Cladocerans) +
    abs(D09PI$Cumacean-D09CU$Cumacean) +
    abs(D09PI$Cyclopoids-D09CU$Cyclopoids) +
    abs(D09PI$Decapods-D09CU$Decapods) +
    abs(D09PI$Diatoms-D09CU$Diatoms) +
    abs(D09PI$Euphausiids-D09CU$Euphausiids) +
    abs(D09PI$Fish-D09CU$Fish) +
    abs(D09PI$Gelatinous-D09CU$Gelatinous) +
    abs(D09PI$Harpacticoids-D09CU$Harpacticoids) +
    abs(D09PI$Insects-D09CU$Insects) +
    abs(D09PI$Isopods-D09CU$Isopods) +
    abs(D09PI$Larvaceans-D09CU$Larvaceans) +
    abs(D09PI$Ostracods-D09CU$Ostracods) +
    abs(D09PI$Polychaetes-D09CU$Polychaetes) +
    abs(D09PI$Pteropods-D09CU$Pteropods)
))

D11psi <- 1-(0.5*(
  abs(D11PI$Amphipods-D11CU$Amphipods) +
    abs(D11PI$Barnacles-D11CU$Barnacles) +
    abs(D11PI$Bivalves-D11CU$Bivalves) +
    abs(D11PI$Calanoids-D11CU$Calanoids) +
    abs(D11PI$Chaetognaths-D11CU$Chaetognaths) +
    abs(D11PI$Cladocerans-D11CU$Cladocerans) +
    abs(D11PI$Cumacean-D11CU$Cumacean) +
    abs(D11PI$Cyclopoids-D11CU$Cyclopoids) +
    abs(D11PI$Decapods-D11CU$Decapods) +
    abs(D11PI$Diatoms-D11CU$Diatoms) +
    abs(D11PI$Euphausiids-D11CU$Euphausiids) +
    abs(D11PI$Fish-D11CU$Fish) +
    abs(D11PI$Gelatinous-D11CU$Gelatinous) +
    abs(D11PI$Harpacticoids-D11CU$Harpacticoids) +
    abs(D11PI$Insects-D11CU$Insects) +
    abs(D11PI$Isopods-D11CU$Isopods) +
    abs(D11PI$Larvaceans-D11CU$Larvaceans) +
    abs(D11PI$Ostracods-D11CU$Ostracods) +
    abs(D11PI$Polychaetes-D11CU$Polychaetes) +
    abs(D11PI$Pteropods-D11CU$Pteropods)
))

J06psi <- 1-(0.5*(
  abs(J06PI$Amphipods-J06CU$Amphipods) +
    abs(J06PI$Barnacles-J06CU$Barnacles) +
    abs(J06PI$Bivalves-J06CU$Bivalves) +
    abs(J06PI$Calanoids-J06CU$Calanoids) +
    abs(J06PI$Chaetognaths-J06CU$Chaetognaths) +
    abs(J06PI$Cladocerans-J06CU$Cladocerans) +
    abs(J06PI$Cumacean-J06CU$Cumacean) +
    abs(J06PI$Cyclopoids-J06CU$Cyclopoids) +
    abs(J06PI$Decapods-J06CU$Decapods) +
    abs(J06PI$Diatoms-J06CU$Diatoms) +
    abs(J06PI$Euphausiids-J06CU$Euphausiids) +
    abs(J06PI$Fish-J06CU$Fish) +
    abs(J06PI$Gelatinous-J06CU$Gelatinous) +
    abs(J06PI$Harpacticoids-J06CU$Harpacticoids) +
    abs(J06PI$Insects-J06CU$Insects) +
    abs(J06PI$Isopods-J06CU$Isopods) +
    abs(J06PI$Larvaceans-J06CU$Larvaceans) +
    abs(J06PI$Ostracods-J06CU$Ostracods) +
    abs(J06PI$Polychaetes-J06CU$Polychaetes) +
    abs(J06PI$Pteropods-J06CU$Pteropods)
))

J08psi <- 1-(0.5*(
  abs(J08PI$Amphipods-J08CU$Amphipods) +
    abs(J08PI$Barnacles-J08CU$Barnacles) +
    abs(J08PI$Bivalves-J08CU$Bivalves) +
    abs(J08PI$Calanoids-J08CU$Calanoids) +
    abs(J08PI$Chaetognaths-J08CU$Chaetognaths) +
    abs(J08PI$Cladocerans-J08CU$Cladocerans) +
    abs(J08PI$Cumacean-J08CU$Cumacean) +
    abs(J08PI$Cyclopoids-J08CU$Cyclopoids) +
    abs(J08PI$Decapods-J08CU$Decapods) +
    abs(J08PI$Diatoms-J08CU$Diatoms) +
    abs(J08PI$Euphausiids-J08CU$Euphausiids) +
    abs(J08PI$Fish-J08CU$Fish) +
    abs(J08PI$Gelatinous-J08CU$Gelatinous) +
    abs(J08PI$Harpacticoids-J08CU$Harpacticoids) +
    abs(J08PI$Insects-J08CU$Insects) +
    abs(J08PI$Isopods-J08CU$Isopods) +
    abs(J08PI$Larvaceans-J08CU$Larvaceans) +
    abs(J08PI$Ostracods-J08CU$Ostracods) +
    abs(J08PI$Polychaetes-J08CU$Polychaetes) +
    abs(J08PI$Pteropods-J08CU$Pteropods)
))

J02psi <- 1-(0.5*(
  abs(J02PI$Amphipods-J02CU$Amphipods) +
    abs(J02PI$Barnacles-J02CU$Barnacles) +
    abs(J02PI$Bivalves-J02CU$Bivalves) +
    abs(J02PI$Calanoids-J02CU$Calanoids) +
    abs(J02PI$Chaetognaths-J02CU$Chaetognaths) +
    abs(J02PI$Cladocerans-J02CU$Cladocerans) +
    abs(J02PI$Cumacean-J02CU$Cumacean) +
    abs(J02PI$Cyclopoids-J02CU$Cyclopoids) +
    abs(J02PI$Decapods-J02CU$Decapods) +
    abs(J02PI$Diatoms-J02CU$Diatoms) +
    abs(J02PI$Euphausiids-J02CU$Euphausiids) +
    abs(J02PI$Fish-J02CU$Fish) +
    abs(J02PI$Gelatinous-J02CU$Gelatinous) +
    abs(J02PI$Harpacticoids-J02CU$Harpacticoids) +
    abs(J02PI$Insects-J02CU$Insects) +
    abs(J02PI$Isopods-J02CU$Isopods) +
    abs(J02PI$Larvaceans-J02CU$Larvaceans) +
    abs(J02PI$Ostracods-J02CU$Ostracods) +
    abs(J02PI$Polychaetes-J02CU$Polychaetes) +
    abs(J02PI$Pteropods-J02CU$Pteropods)
))

biom_psi %>%
  group_by(`Sample Site`, `Fish Species`) %>%
  tally()

D07sums <- (D07PI$Amphipods+D07CU$Amphipods)*log(D07PI$Amphipods+D07CU$Amphipods)+
  (D07PI$Barnacles+D07CU$Barnacles)*log(D07PI$Barnacles+D07CU$Barnacles)+
  (D07PI$Bivalves+D07CU$Bivalves)*log(D07PI$Bivalves+D07CU$Bivalves) +
  (D07PI$Calanoids+D07CU$Calanoids)*log(D07PI$Calanoids+D07CU$Calanoids) +
  #(D07PI$Chaetognaths+D07CU$Chaetognaths)*log(D07PI$Chaetognaths+D07CU$Chaetognaths) +
  (D07PI$Cladocerans+D07CU$Cladocerans)*log(D07PI$Cladocerans+D07CU$Cladocerans) +
  #(D07PI$Cumacean+D07CU$Cumacean)*log(D07PI$Cumacean+D07CU$Cumacean) +
  (D07PI$Cyclopoids+D07CU$Cyclopoids)*log(D07PI$Cyclopoids+D07CU$Cyclopoids) +
  (D07PI$Decapods+D07CU$Decapods)*log(D07PI$Decapods+D07CU$Decapods) +
  (D07PI$Diatoms+D07CU$Diatoms)*log(D07PI$Diatoms+D07CU$Diatoms) +
  (D07PI$Euphausiids+D07CU$Euphausiids)*log(D07PI$Euphausiids+D07CU$Euphausiids) +
  #(D07PI$Fish+D07CU$Fish)*log(D07PI$Fish+D07CU$Fish) +
  (D07PI$Gelatinous+D07CU$Gelatinous)*log(D07PI$Gelatinous+D07CU$Gelatinous) +
  (D07PI$Harpacticoids+D07CU$Harpacticoids)*log(D07PI$Harpacticoids+D07CU$Harpacticoids) +
  (D07PI$Insects+D07CU$Insects)*log(D07PI$Insects+D07CU$Insects) +
  #(D07PI$Isopods+D07CU$Isopods)*log(D07PI$Isopods+D07CU$Isopods) +
  (D07PI$Larvaceans+D07CU$Larvaceans)*log(D07PI$Larvaceans+D07CU$Larvaceans) +
  #(D07PI$Ostracods+D07CU$Ostracods)*log(D07PI$Ostracods+D07CU$Ostracods) +
  (D07PI$Polychaetes+D07CU$Polychaetes)*log(D07PI$Polychaetes+D07CU$Polychaetes) +
  (D07PI$Pteropods+D07CU$Pteropods)*log(D07PI$Pteropods+D07CU$Pteropods)

D07pinksum <- (D07PI$Amphipods)*log(D07PI$Amphipods)+
  (D07PI$Barnacles)*log(D07PI$Barnacles)+
  (D07PI$Bivalves)*log(D07PI$Bivalves) +
  (D07PI$Calanoids)*log(D07PI$Calanoids) +
  #(D07PI$Chaetognaths)*log(D07PI$Chaetognaths) +
  (D07PI$Cladocerans)*log(D07PI$Cladocerans) +
  #(D07PI$Cumacean)*log(D07PI$Cumacean) +
  (D07PI$Cyclopoids)*log(D07PI$Cyclopoids) +
  (D07PI$Decapods)*log(D07PI$Decapods) +
  (D07PI$Diatoms)*log(D07PI$Diatoms) +
  (D07PI$Euphausiids)*log(D07PI$Euphausiids) +
  #(D07PI$Fish)*log(D07PI$Fish) +
  #(D07PI$Gelatinous)*log(D07PI$Gelatinous) +
  (D07PI$Harpacticoids)*log(D07PI$Harpacticoids) +
  (D07PI$Insects)*log(D07PI$Insects) +
  #(D07PI$Isopods)*log(D07PI$Isopods) +
  (D07PI$Larvaceans)*log(D07PI$Larvaceans) +
  #(D07PI$Ostracods)*log(D07PI$Ostracods) +
  (D07PI$Polychaetes)*log(D07PI$Polychaetes) +
  (D07PI$Pteropods)*log(D07PI$Pteropods)

D07chumsum <- #(D07CU$Amphipods)*log(D07CU$Amphipods)+
  (D07CU$Barnacles)*log(D07CU$Barnacles)+
  #(D07CU$Bivalves)*log(D07CU$Bivalves) +
  (D07CU$Calanoids)*log(D07CU$Calanoids) +
  #(D07CU$Chaetognaths)*log(D07CU$Chaetognaths) +
  #(D07CU$Cladocerans)*log(D07CU$Cladocerans) +
  #(D07CU$Cumacean)*log(D07CU$Cumacean) +
  (D07CU$Cyclopoids)*log(D07CU$Cyclopoids) +
  (D07CU$Decapods)*log(D07CU$Decapods) +
  (D07CU$Diatoms)*log(D07CU$Diatoms) +
  (D07CU$Euphausiids)*log(D07CU$Euphausiids) +
  #(D07CU$Fish)*log(D07CU$Fish) +
  (D07CU$Gelatinous)*log(D07CU$Gelatinous) +
  #(D07CU$Harpacticoids)*log(D07CU$Harpacticoids) +
  #(D07CU$Insects)*log(D07CU$Insects) +
  #(D07CU$Isopods)*log(D07CU$Isopods) +
  (D07CU$Larvaceans)*log(D07CU$Larvaceans) +
  #(D07CU$Ostracods)*log(D07CU$Ostracods) +
  (D07CU$Polychaetes)*log(D07CU$Polychaetes) +
  (D07CU$Pteropods)*log(D07CU$Pteropods)

D07hornes <- (D07sums - D07pinksum - D07chumsum)/(2*log(2))

D09sums <- (D09PI$Amphipods+D09CU$Amphipods)*log(D09PI$Amphipods+D09CU$Amphipods)+
  (D09PI$Barnacles+D09CU$Barnacles)*log(D09PI$Barnacles+D09CU$Barnacles)+
  #(D09PI$Bivalves+D09CU$Bivalves)*log(D09PI$Bivalves+D09CU$Bivalves) +
  (D09PI$Calanoids+D09CU$Calanoids)*log(D09PI$Calanoids+D09CU$Calanoids) +
  #(D09PI$Chaetognaths+D09CU$Chaetognaths)*log(D09PI$Chaetognaths+D09CU$Chaetognaths) +
  (D09PI$Cladocerans+D09CU$Cladocerans)*log(D09PI$Cladocerans+D09CU$Cladocerans) +
  (D09PI$Cumacean+D09CU$Cumacean)*log(D09PI$Cumacean+D09CU$Cumacean) +
  (D09PI$Cyclopoids+D09CU$Cyclopoids)*log(D09PI$Cyclopoids+D09CU$Cyclopoids) +
  (D09PI$Decapods+D09CU$Decapods)*log(D09PI$Decapods+D09CU$Decapods) +
  (D09PI$Diatoms+D09CU$Diatoms)*log(D09PI$Diatoms+D09CU$Diatoms) +
  (D09PI$Euphausiids+D09CU$Euphausiids)*log(D09PI$Euphausiids+D09CU$Euphausiids) +
  (D09PI$Fish+D09CU$Fish)*log(D09PI$Fish+D09CU$Fish) +
  (D09PI$Gelatinous+D09CU$Gelatinous)*log(D09PI$Gelatinous+D09CU$Gelatinous) +
  (D09PI$Harpacticoids+D09CU$Harpacticoids)*log(D09PI$Harpacticoids+D09CU$Harpacticoids) +
  (D09PI$Insects+D09CU$Insects)*log(D09PI$Insects+D09CU$Insects) +
  (D09PI$Isopods+D09CU$Isopods)*log(D09PI$Isopods+D09CU$Isopods) +
  (D09PI$Larvaceans+D09CU$Larvaceans)*log(D09PI$Larvaceans+D09CU$Larvaceans) +
  #(D09PI$Ostracods+D09CU$Ostracods)*log(D09PI$Ostracods+D09CU$Ostracods) +
  (D09PI$Polychaetes+D09CU$Polychaetes)*log(D09PI$Polychaetes+D09CU$Polychaetes) +
  (D09PI$Pteropods+D09CU$Pteropods)*log(D09PI$Pteropods+D09CU$Pteropods)

D09pinksum <- (D09PI$Amphipods)*log(D09PI$Amphipods)+
  (D09PI$Barnacles)*log(D09PI$Barnacles)+
  #(D09PI$Bivalves)*log(D09PI$Bivalves) +
  (D09PI$Calanoids)*log(D09PI$Calanoids) +
  #(D09PI$Chaetognaths)*log(D09PI$Chaetognaths) +
  (D09PI$Cladocerans)*log(D09PI$Cladocerans) +
  (D09PI$Cumacean)*log(D09PI$Cumacean) +
  (D09PI$Cyclopoids)*log(D09PI$Cyclopoids) +
  (D09PI$Decapods)*log(D09PI$Decapods) +
  (D09PI$Diatoms)*log(D09PI$Diatoms) +
  (D09PI$Euphausiids)*log(D09PI$Euphausiids) +
  (D09PI$Fish)*log(D09PI$Fish) +
  (D09PI$Gelatinous)*log(D09PI$Gelatinous) +
  (D09PI$Harpacticoids)*log(D09PI$Harpacticoids) +
  (D09PI$Insects)*log(D09PI$Insects) +
  (D09PI$Isopods)*log(D09PI$Isopods) +
  (D09PI$Larvaceans)*log(D09PI$Larvaceans) +
  #(D09PI$Ostracods)*log(D09PI$Ostracods) +
  (D09PI$Polychaetes)*log(D09PI$Polychaetes) +
  (D09PI$Pteropods)*log(D09PI$Pteropods)

D09chumsum <- #(D09CU$Amphipods)*log(D09CU$Amphipods)+
  (D09CU$Barnacles)*log(D09CU$Barnacles)+
  #(D09CU$Bivalves)*log(D09CU$Bivalves) +
  (D09CU$Calanoids)*log(D09CU$Calanoids) +
  #(D09CU$Chaetognaths)*log(D09CU$Chaetognaths) +
  (D09CU$Cladocerans)*log(D09CU$Cladocerans) +
  #(D09CU$Cumacean)*log(D09CU$Cumacean) +
  (D09CU$Cyclopoids)*log(D09CU$Cyclopoids) +
  (D09CU$Decapods)*log(D09CU$Decapods) +
  (D09CU$Diatoms)*log(D09CU$Diatoms) +
  (D09CU$Euphausiids)*log(D09CU$Euphausiids) +
  (D09CU$Fish)*log(D09CU$Fish) +
  (D09CU$Gelatinous)*log(D09CU$Gelatinous) +
  (D09CU$Harpacticoids)*log(D09CU$Harpacticoids) +
  (D09CU$Insects)*log(D09CU$Insects) +
  #(D09CU$Isopods)*log(D09CU$Isopods) +
  (D09CU$Larvaceans)*log(D09CU$Larvaceans) +
  #(D09CU$Ostracods)*log(D09CU$Ostracods) +
  #(D09CU$Polychaetes)*log(D09CU$Polychaetes) +
  (D09CU$Pteropods)*log(D09CU$Pteropods)

D09hornes <- (D09sums - D09pinksum - D09chumsum)/(2*log(2))

D11sums <- (D11PI$Amphipods+D11CU$Amphipods)*log(D11PI$Amphipods+D11CU$Amphipods)+
  (D11PI$Barnacles+D11CU$Barnacles)*log(D11PI$Barnacles+D11CU$Barnacles)+
  #(D11PI$Bivalves+D11CU$Bivalves)*log(D11PI$Bivalves+D11CU$Bivalves) +
  (D11PI$Calanoids+D11CU$Calanoids)*log(D11PI$Calanoids+D11CU$Calanoids) +
  #(D11PI$Chaetognaths+D11CU$Chaetognaths)*log(D11PI$Chaetognaths+D11CU$Chaetognaths) +
  (D11PI$Cladocerans+D11CU$Cladocerans)*log(D11PI$Cladocerans+D11CU$Cladocerans) +
  (D11PI$Cumacean+D11CU$Cumacean)*log(D11PI$Cumacean+D11CU$Cumacean) +
  (D11PI$Cyclopoids+D11CU$Cyclopoids)*log(D11PI$Cyclopoids+D11CU$Cyclopoids) +
  (D11PI$Decapods+D11CU$Decapods)*log(D11PI$Decapods+D11CU$Decapods) +
  #(D11PI$Diatoms+D11CU$Diatoms)*log(D11PI$Diatoms+D11CU$Diatoms) +
  (D11PI$Euphausiids+D11CU$Euphausiids)*log(D11PI$Euphausiids+D11CU$Euphausiids) +
  (D11PI$Fish+D11CU$Fish)*log(D11PI$Fish+D11CU$Fish) +
  #(D11PI$Gelatinous+D11CU$Gelatinous)*log(D11PI$Gelatinous+D11CU$Gelatinous) +
  (D11PI$Harpacticoids+D11CU$Harpacticoids)*log(D11PI$Harpacticoids+D11CU$Harpacticoids) +
  (D11PI$Insects+D11CU$Insects)*log(D11PI$Insects+D11CU$Insects) +
  (D11PI$Isopods+D11CU$Isopods)*log(D11PI$Isopods+D11CU$Isopods) +
  (D11PI$Larvaceans+D11CU$Larvaceans)*log(D11PI$Larvaceans+D11CU$Larvaceans) +
  #(D11PI$Ostracods+D11CU$Ostracods)*log(D11PI$Ostracods+D11CU$Ostracods) +
  (D11PI$Polychaetes+D11CU$Polychaetes)*log(D11PI$Polychaetes+D11CU$Polychaetes) #+
  #(D11PI$Pteropods+D11CU$Pteropods)*log(D11PI$Pteropods+D11CU$Pteropods)

D11pinksum <- (D11PI$Amphipods)*log(D11PI$Amphipods)+
  (D11PI$Barnacles)*log(D11PI$Barnacles)+
  #(D11PI$Bivalves)*log(D11PI$Bivalves) +
  (D11PI$Calanoids)*log(D11PI$Calanoids) +
  #(D11PI$Chaetognaths)*log(D11PI$Chaetognaths) +
  (D11PI$Cladocerans)*log(D11PI$Cladocerans) +
  (D11PI$Cumacean)*log(D11PI$Cumacean) +
  (D11PI$Cyclopoids)*log(D11PI$Cyclopoids) +
  (D11PI$Decapods)*log(D11PI$Decapods) +
  #(D11PI$Diatoms)*log(D11PI$Diatoms) +
  (D11PI$Euphausiids)*log(D11PI$Euphausiids) +
  (D11PI$Fish)*log(D11PI$Fish) +
  #(D11PI$Gelatinous)*log(D11PI$Gelatinous) +
  (D11PI$Harpacticoids)*log(D11PI$Harpacticoids) +
  (D11PI$Insects)*log(D11PI$Insects) +
  (D11PI$Isopods)*log(D11PI$Isopods) +
  (D11PI$Larvaceans)*log(D11PI$Larvaceans) +
  #(D11PI$Ostracods)*log(D11PI$Ostracods) +
  (D11PI$Polychaetes)*log(D11PI$Polychaetes) #+
  #(D11PI$Pteropods)*log(D11PI$Pteropods)

D11chumsum <- #(D11CU$Amphipods)*log(D11CU$Amphipods)+
  (D11CU$Barnacles)*log(D11CU$Barnacles)+
  #(D11CU$Bivalves)*log(D11CU$Bivalves) +
  (D11CU$Calanoids)*log(D11CU$Calanoids) +
  #(D11CU$Chaetognaths)*log(D11CU$Chaetognaths) +
  #(D11CU$Cladocerans)*log(D11CU$Cladocerans) +
  (D11CU$Cumacean)*log(D11CU$Cumacean) +
  #(D11CU$Cyclopoids)*log(D11CU$Cyclopoids) +
  #(D11CU$Decapods)*log(D11CU$Decapods) +
  #(D11CU$Diatoms)*log(D11CU$Diatoms) +
  (D11CU$Euphausiids)*log(D11CU$Euphausiids) +
  (D11CU$Fish)*log(D11CU$Fish) +
  #(D11CU$Gelatinous)*log(D11CU$Gelatinous) +
  #(D11CU$Harpacticoids)*log(D11CU$Harpacticoids) +
  (D11CU$Insects)*log(D11CU$Insects) +
  #(D11CU$Isopods)*log(D11CU$Isopods) +
  (D11CU$Larvaceans)*log(D11CU$Larvaceans) #+
  #(D11CU$Ostracods)*log(D11CU$Ostracods) +
  #(D11CU$Polychaetes)*log(D11CU$Polychaetes) +
  #(D11CU$Pteropods)*log(D11CU$Pteropods)

D11hornes <- (D11sums - D11pinksum - D11chumsum)/(2*log(2))

J06sums <- (J06PI$Amphipods+J06CU$Amphipods)*log(J06PI$Amphipods+J06CU$Amphipods)+
  (J06PI$Barnacles+J06CU$Barnacles)*log(J06PI$Barnacles+J06CU$Barnacles)+
  #(J06PI$Bivalves+J06CU$Bivalves)*log(J06PI$Bivalves+J06CU$Bivalves) +
  (J06PI$Calanoids+J06CU$Calanoids)*log(J06PI$Calanoids+J06CU$Calanoids) +
  (J06PI$Chaetognaths+J06CU$Chaetognaths)*log(J06PI$Chaetognaths+J06CU$Chaetognaths) +
  #(J06PI$Cladocerans+J06CU$Cladocerans)*log(J06PI$Cladocerans+J06CU$Cladocerans) +
  #(J06PI$Cumacean+J06CU$Cumacean)*log(J06PI$Cumacean+J06CU$Cumacean) +
  (J06PI$Cyclopoids+J06CU$Cyclopoids)*log(J06PI$Cyclopoids+J06CU$Cyclopoids) +
  (J06PI$Decapods+J06CU$Decapods)*log(J06PI$Decapods+J06CU$Decapods) +
  #(J06PI$Diatoms+J06CU$Diatoms)*log(J06PI$Diatoms+J06CU$Diatoms) +
  (J06PI$Euphausiids+J06CU$Euphausiids)*log(J06PI$Euphausiids+J06CU$Euphausiids) +
  #(J06PI$Fish+J06CU$Fish)*log(J06PI$Fish+J06CU$Fish) +
  (J06PI$Gelatinous+J06CU$Gelatinous)*log(J06PI$Gelatinous+J06CU$Gelatinous) +
  (J06PI$Harpacticoids+J06CU$Harpacticoids)*log(J06PI$Harpacticoids+J06CU$Harpacticoids) +
  (J06PI$Insects+J06CU$Insects)*log(J06PI$Insects+J06CU$Insects) +
  (J06PI$Isopods+J06CU$Isopods)*log(J06PI$Isopods+J06CU$Isopods) +
  (J06PI$Larvaceans+J06CU$Larvaceans)*log(J06PI$Larvaceans+J06CU$Larvaceans) +
  (J06PI$Ostracods+J06CU$Ostracods)*log(J06PI$Ostracods+J06CU$Ostracods) +
  #(J06PI$Polychaetes+J06CU$Polychaetes)*log(J06PI$Polychaetes+J06CU$Polychaetes) +
  (J06PI$Pteropods+J06CU$Pteropods)*log(J06PI$Pteropods+J06CU$Pteropods)

J06pinksum <- (J06PI$Amphipods)*log(J06PI$Amphipods)+
  (J06PI$Barnacles)*log(J06PI$Barnacles)+
  #(J06PI$Bivalves)*log(J06PI$Bivalves) +
  (J06PI$Calanoids)*log(J06PI$Calanoids) +
  (J06PI$Chaetognaths)*log(J06PI$Chaetognaths) +
  #(J06PI$Cladocerans)*log(J06PI$Cladocerans) +
  #(J06PI$Cumacean)*log(J06PI$Cumacean) +
  (J06PI$Cyclopoids)*log(J06PI$Cyclopoids) +
  (J06PI$Decapods)*log(J06PI$Decapods) +
  #(J06PI$Diatoms)*log(J06PI$Diatoms) +
  (J06PI$Euphausiids)*log(J06PI$Euphausiids) +
  #(J06PI$Fish)*log(J06PI$Fish) +
  (J06PI$Gelatinous)*log(J06PI$Gelatinous) +
  (J06PI$Harpacticoids)*log(J06PI$Harpacticoids) +
  (J06PI$Insects)*log(J06PI$Insects) +
  (J06PI$Isopods)*log(J06PI$Isopods) +
  #(J06PI$Larvaceans)*log(J06PI$Larvaceans) +
  (J06PI$Ostracods)*log(J06PI$Ostracods) +
  #(J06PI$Polychaetes)*log(J06PI$Polychaetes) +
  (J06PI$Pteropods)*log(J06PI$Pteropods)

J06chumsum <- #(J06CU$Amphipods)*log(J06CU$Amphipods)+
  #(J06CU$Barnacles)*log(J06CU$Barnacles)+
  #(J06CU$Bivalves)*log(J06CU$Bivalves) +
  (J06CU$Calanoids)*log(J06CU$Calanoids) +
  (J06CU$Chaetognaths)*log(J06CU$Chaetognaths) +
  #(J06CU$Cladocerans)*log(J06CU$Cladocerans) +
  #(J06CU$Cumacean)*log(J06CU$Cumacean) +
  #(J06CU$Cyclopoids)*log(J06CU$Cyclopoids) +
  #(J06CU$Decapods)*log(J06CU$Decapods) +
  #(J06CU$Diatoms)*log(J06CU$Diatoms) +
  (J06CU$Euphausiids)*log(J06CU$Euphausiids) +
  #(J06CU$Fish)*log(J06CU$Fish) +
  (J06CU$Gelatinous)*log(J06CU$Gelatinous) +
  #(J06CU$Harpacticoids)*log(J06CU$Harpacticoids) +
  #(J06CU$Insects)*log(J06CU$Insects) +
  #(J06CU$Isopods)*log(J06CU$Isopods) +
  (J06CU$Larvaceans)*log(J06CU$Larvaceans) #+
  #(J06CU$Ostracods)*log(J06CU$Ostracods) +
  #(J06CU$Polychaetes)*log(J06CU$Polychaetes) +
  #(J06CU$Pteropods)*log(J06CU$Pteropods)

J06hornes <- (J06sums - J06pinksum - J06chumsum)/(2*log(2))

J08sums <- (J08PI$Amphipods+J08CU$Amphipods)*log(J08PI$Amphipods+J08CU$Amphipods)+
  (J08PI$Barnacles+J08CU$Barnacles)*log(J08PI$Barnacles+J08CU$Barnacles)+
  (J08PI$Bivalves+J08CU$Bivalves)*log(J08PI$Bivalves+J08CU$Bivalves) +
  (J08PI$Calanoids+J08CU$Calanoids)*log(J08PI$Calanoids+J08CU$Calanoids) +
  (J08PI$Chaetognaths+J08CU$Chaetognaths)*log(J08PI$Chaetognaths+J08CU$Chaetognaths) +
  (J08PI$Cladocerans+J08CU$Cladocerans)*log(J08PI$Cladocerans+J08CU$Cladocerans) +
  #(J08PI$Cumacean+J08CU$Cumacean)*log(J08PI$Cumacean+J08CU$Cumacean) +
  (J08PI$Cyclopoids+J08CU$Cyclopoids)*log(J08PI$Cyclopoids+J08CU$Cyclopoids) +
  (J08PI$Decapods+J08CU$Decapods)*log(J08PI$Decapods+J08CU$Decapods) +
  (J08PI$Diatoms+J08CU$Diatoms)*log(J08PI$Diatoms+J08CU$Diatoms) +
  (J08PI$Euphausiids+J08CU$Euphausiids)*log(J08PI$Euphausiids+J08CU$Euphausiids) +
  (J08PI$Fish+J08CU$Fish)*log(J08PI$Fish+J08CU$Fish) +
  (J08PI$Gelatinous+J08CU$Gelatinous)*log(J08PI$Gelatinous+J08CU$Gelatinous) +
  (J08PI$Harpacticoids+J08CU$Harpacticoids)*log(J08PI$Harpacticoids+J08CU$Harpacticoids) +
  (J08PI$Insects+J08CU$Insects)*log(J08PI$Insects+J08CU$Insects) +
  (J08PI$Isopods+J08CU$Isopods)*log(J08PI$Isopods+J08CU$Isopods) +
  (J08PI$Larvaceans+J08CU$Larvaceans)*log(J08PI$Larvaceans+J08CU$Larvaceans) +
  #(J08PI$Ostracods+J08CU$Ostracods)*log(J08PI$Ostracods+J08CU$Ostracods) +
  (J08PI$Polychaetes+J08CU$Polychaetes)*log(J08PI$Polychaetes+J08CU$Polychaetes) +
  (J08PI$Pteropods+J08CU$Pteropods)*log(J08PI$Pteropods+J08CU$Pteropods)

J08pinksum <- (J08PI$Amphipods)*log(J08PI$Amphipods)+
  (J08PI$Barnacles)*log(J08PI$Barnacles)+
  (J08PI$Bivalves)*log(J08PI$Bivalves) +
  (J08PI$Calanoids)*log(J08PI$Calanoids) +
  (J08PI$Chaetognaths)*log(J08PI$Chaetognaths) +
  #(J08PI$Cladocerans)*log(J08PI$Cladocerans) +
  #(J08PI$Cumacean)*log(J08PI$Cumacean) +
  (J08PI$Cyclopoids)*log(J08PI$Cyclopoids) +
  (J08PI$Decapods)*log(J08PI$Decapods) +
  (J08PI$Diatoms)*log(J08PI$Diatoms) +
  (J08PI$Euphausiids)*log(J08PI$Euphausiids) +
  (J08PI$Fish)*log(J08PI$Fish) +
  #(J08PI$Gelatinous)*log(J08PI$Gelatinous) +
  (J08PI$Harpacticoids)*log(J08PI$Harpacticoids) +
  (J08PI$Insects)*log(J08PI$Insects) +
  (J08PI$Isopods)*log(J08PI$Isopods) +
  #(J08PI$Larvaceans)*log(J08PI$Larvaceans) +
  #(J08PI$Ostracods)*log(J08PI$Ostracods) +
  #(J08PI$Polychaetes)*log(J08PI$Polychaetes) +
  (J08PI$Pteropods)*log(J08PI$Pteropods)

J08chumsum <- (J08CU$Amphipods)*log(J08CU$Amphipods)+
  #(J08CU$Barnacles)*log(J08CU$Barnacles)+
  #(J08CU$Bivalves)*log(J08CU$Bivalves) +
  (J08CU$Calanoids)*log(J08CU$Calanoids) +
  (J08CU$Chaetognaths)*log(J08CU$Chaetognaths) +
  (J08CU$Cladocerans)*log(J08CU$Cladocerans) +
  #(J08CU$Cumacean)*log(J08CU$Cumacean) +
  (J08CU$Cyclopoids)*log(J08CU$Cyclopoids) +
  (J08CU$Decapods)*log(J08CU$Decapods) +
  (J08CU$Diatoms)*log(J08CU$Diatoms) +
  (J08CU$Euphausiids)*log(J08CU$Euphausiids) +
  (J08CU$Fish)*log(J08CU$Fish) +
  (J08CU$Gelatinous)*log(J08CU$Gelatinous) +
  #(J08CU$Harpacticoids)*log(J08CU$Harpacticoids) +
  (J08CU$Insects)*log(J08CU$Insects) +
  #(J08CU$Isopods)*log(J08CU$Isopods) +
  (J08CU$Larvaceans)*log(J08CU$Larvaceans) +
  #(J08CU$Ostracods)*log(J08CU$Ostracods) +
  (J08CU$Polychaetes)*log(J08CU$Polychaetes) #+
  #(J08CU$Pteropods)*log(J08CU$Pteropods)

J08hornes <- (J08sums - J08pinksum - J08chumsum)/(2*log(2))

J02sums <- (J02PI$Amphipods+J02CU$Amphipods)*log(J02PI$Amphipods+J02CU$Amphipods)+
  (J02PI$Barnacles+J02CU$Barnacles)*log(J02PI$Barnacles+J02CU$Barnacles)+
  #(J02PI$Bivalves+J02CU$Bivalves)*log(J02PI$Bivalves+J02CU$Bivalves) +
  (J02PI$Calanoids+J02CU$Calanoids)*log(J02PI$Calanoids+J02CU$Calanoids) +
  (J02PI$Chaetognaths+J02CU$Chaetognaths)*log(J02PI$Chaetognaths+J02CU$Chaetognaths) +
  #(J02PI$Cladocerans+J02CU$Cladocerans)*log(J02PI$Cladocerans+J02CU$Cladocerans) +
  (J02PI$Cumacean+J02CU$Cumacean)*log(J02PI$Cumacean+J02CU$Cumacean) +
  (J02PI$Cyclopoids+J02CU$Cyclopoids)*log(J02PI$Cyclopoids+J02CU$Cyclopoids) +
  (J02PI$Decapods+J02CU$Decapods)*log(J02PI$Decapods+J02CU$Decapods) +
  #(J02PI$Diatoms+J02CU$Diatoms)*log(J02PI$Diatoms+J02CU$Diatoms) +
  (J02PI$Euphausiids+J02CU$Euphausiids)*log(J02PI$Euphausiids+J02CU$Euphausiids) +
  (J02PI$Fish+J02CU$Fish)*log(J02PI$Fish+J02CU$Fish) +
  #(J02PI$Gelatinous+J02CU$Gelatinous)*log(J02PI$Gelatinous+J02CU$Gelatinous) +
  (J02PI$Harpacticoids+J02CU$Harpacticoids)*log(J02PI$Harpacticoids+J02CU$Harpacticoids) +
  (J02PI$Insects+J02CU$Insects)*log(J02PI$Insects+J02CU$Insects) +
  (J02PI$Isopods+J02CU$Isopods)*log(J02PI$Isopods+J02CU$Isopods) +
  #(J02PI$Larvaceans+J02CU$Larvaceans)*log(J02PI$Larvaceans+J02CU$Larvaceans) +
  #(J02PI$Ostracods+J02CU$Ostracods)*log(J02PI$Ostracods+J02CU$Ostracods) +
  (J02PI$Polychaetes+J02CU$Polychaetes)*log(J02PI$Polychaetes+J02CU$Polychaetes) +
  (J02PI$Pteropods+J02CU$Pteropods)*log(J02PI$Pteropods+J02CU$Pteropods)

J02pinksum <- (J02PI$Amphipods)*log(J02PI$Amphipods)+
  (J02PI$Barnacles)*log(J02PI$Barnacles)+
  #(J02PI$Bivalves)*log(J02PI$Bivalves) +
  (J02PI$Calanoids)*log(J02PI$Calanoids) +
  (J02PI$Chaetognaths)*log(J02PI$Chaetognaths) +
  #(J02PI$Cladocerans)*log(J02PI$Cladocerans) +
  (J02PI$Cumacean)*log(J02PI$Cumacean) +
  (J02PI$Cyclopoids)*log(J02PI$Cyclopoids) +
  (J02PI$Decapods)*log(J02PI$Decapods) +
  #(J02PI$Diatoms)*log(J02PI$Diatoms) +
  (J02PI$Euphausiids)*log(J02PI$Euphausiids) +
  (J02PI$Fish)*log(J02PI$Fish) +
  #(J02PI$Gelatinous)*log(J02PI$Gelatinous) +
  (J02PI$Harpacticoids)*log(J02PI$Harpacticoids) +
  (J02PI$Insects)*log(J02PI$Insects) +
  (J02PI$Isopods)*log(J02PI$Isopods) +
  #(J02PI$Larvaceans)*log(J02PI$Larvaceans) +
  #(J02PI$Ostracods)*log(J02PI$Ostracods) +
  (J02PI$Polychaetes)*log(J02PI$Polychaetes) +
  (J02PI$Pteropods)*log(J02PI$Pteropods)

J02chumsum <- (J02CU$Amphipods)*log(J02CU$Amphipods)+
  #(J02CU$Barnacles)*log(J02CU$Barnacles)+
  #(J02CU$Bivalves)*log(J02CU$Bivalves) +
  (J02CU$Calanoids)*log(J02CU$Calanoids) +
  (J02CU$Chaetognaths)*log(J02CU$Chaetognaths) +
  #(J02CU$Cladocerans)*log(J02CU$Cladocerans) +
  #(J02CU$Cumacean)*log(J02CU$Cumacean) +
  (J02CU$Cyclopoids)*log(J02CU$Cyclopoids) +
  (J02CU$Decapods)*log(J02CU$Decapods) +
  #(J02CU$Diatoms)*log(J02CU$Diatoms) +
  (J02CU$Euphausiids)*log(J02CU$Euphausiids) +
  (J02CU$Fish)*log(J02CU$Fish) +
  #(J02CU$Gelatinous)*log(J02CU$Gelatinous) +
  #(J02CU$Harpacticoids)*log(J02CU$Harpacticoids) +
  (J02CU$Insects)*log(J02CU$Insects) +
  (J02CU$Isopods)*log(J02CU$Isopods)# +
  #(J02CU$Larvaceans)*log(J02CU$Larvaceans) +
  #(J02CU$Ostracods)*log(J02CU$Ostracods) +
  #(J02CU$Polychaetes)*log(J02CU$Polychaetes) +
  #(J02CU$Pteropods)*log(J02CU$Pteropods)

J02hornes <- (J02sums - J02pinksum - J02chumsum)/(2*log(2))

##### Prey composition by WW #####

biom_graph %>%
	ggplot(aes(`Sample Site`, Biomass))+
	geom_bar(aes(fill=`Taxonomic Group`), stat = "identity", position = "fill")+
	scale_fill_manual(values = c("#FB9A99", "#E31A1C", "#FDBF6F",
															 "#FF7F00", #bar, cal, dec, euph
															 #"#FFFF99", #euph_eggs
															 "#CAB3D6", #cladocerans
															 "#6A3D9A", #echinoderms
															 "#A6CEE3", "#1F78B4", #chae, gel
															 "#33A02C", "grey60"))+#lar, oth
  facet_wrap(~`Fish Species`, dir = "v", scales = "free")+
	ggtitle("Spatial Diets (2016)")+
  theme_bw()+
	scale_y_continuous(labels=scales::unit_format("%", 100))+
  theme(panel.grid=element_blank(), strip.text = element_text(size=16),
  			axis.title = element_text(size=14), axis.text = element_text(size=12),
  			legend.text = element_text(size=12), legend.title = element_text(size=14),
  			title = element_text(size=16), plot.title = element_text(hjust=0.5))
#plot biomass comparisons for each site! (spatial)
#relevant prey: calanoids, euphausiids, larvaceans, decapods, amphipods, chaetognaths
#and insects, polychaetes, gelatinous, fish

#kill cladocerans and echinoderms! *****

biomass %>%
	filter(Analysis != "Spatial" & Year=="2015") %>%
	ggplot(aes(sampling_week, Biomass))+
	geom_bar(aes(fill=`Taxonomic Group`), stat = "identity", position = "fill")+
	scale_fill_manual(values = c("#FB9A99", "#E31A1C", "#FDBF6F",
															 "#FF7F00", #bar, cal, dec, euph
															 "#FFFF99", #euph_eggs
															 "#CAB3D6", #cladocerans
															 "#6A3D9A", #echinoderms
															 "#A6CEE3", "#1F78B4", #chae, gel
															 "#33A02C", "grey60"))+#lar, oth
  facet_grid(`Fish Species`~`Sample Site`, scales = "free_x")+
	ggtitle("2015 Temporal Diets")+
	labs(x="Sample Week")+
  theme_bw()+
	scale_y_continuous(labels=scales::unit_format("%", 100))+
	theme(panel.grid=element_blank(), strip.text = element_text(size=16),
				axis.title = element_text(size=14), axis.text = element_text(size=12),
				legend.text = element_text(size=12), legend.title = element_text(size=14),
				title = element_text(size=16), plot.title = element_text(hjust=0.5))
#plot biomass comparisons for 2015! (temporal)
#relevant prey: amphipods, barnacles, calanoids, chaetognaths, cladocerans, decapods
#and echinoderms, euph_eggs, euphausiids, larvaceans, gelatinous, fish ???

#kill euph_eggs and echinoderms ***** 

biomass %>%
	filter(Analysis != "Spatial" & Year=="2016") %>%
	ggplot(aes(sampling_week, Biomass))+
	geom_bar(aes(fill=`Taxonomic Group`), stat = "identity", position = "fill")+
	scale_fill_manual(values = c("#FB9A99", "#E31A1C", "#FDBF6F",
															 "#FF7F00", #bar, cal, dec, euph
															 "#FFFF99", #euph_eggs
															 "#CAB3D6", #cladocerans
															 "#6A3D9A", #echinoderms
															 "#A6CEE3", "#1F78B4", #chae, gel
															 "#33A02C", "grey60"))+#lar, oth
	facet_grid(`Fish Species`~`Sample Site`, scales = "free_x")+
	ggtitle("2016 Temporal Diets")+
	labs(x="Sample Week")+
	theme_bw()+
	scale_y_continuous(labels=scales::unit_format("%", 100))+
	theme(panel.grid=element_blank(), strip.text = element_text(size=16),
				axis.title = element_text(size=14), axis.text = element_text(size=12),
				legend.text = element_text(size=12), legend.title = element_text(size=14),
				title = element_text(size=16), plot.title = element_text(hjust=0.5))
#plot biomass comparisons for 2016! (temporal)
#relevant prey: calanoids, gelatinous, euphausiids, chaetognaths, decapods (thats it)

#kill cladocerans! *****

#summary stuff
biomass %>%
	#filter(`Taxonomic Group`=="Other") %>% 
  group_by(UFN, semsp_id, `Fish Species`, `Sample Date`, `Sample Site`
           #, `Taxonomic Detail`
           ) %>%
  summarise(Biomass=mean(Biomass)) %>%
  View()
#yeah this shit isn't great because it takes the mean of non-zero values... ERROR!

unique(biomass$semsp_id)



biomass %>%
	filter(`Sample Site`=="J02") %>% 
	ggplot(aes(UFN, Biomass))+
	geom_bar(aes(fill=`Taxonomic Group`), stat = "identity", position = "fill")+
	#scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02",
	#														 "#666666"))+
	facet_wrap(~`Fish Species`, dir = "v", scales = "free")+
	labs(title="J02 Diets")
#format for checking out detail of a specific site - J02 as example

##### Diets by size class (WW) #####

biom_graph %>%
	ggplot(aes(`Sample Site`, Biomass))+
	geom_bar(aes(fill=`Size Class`), stat = "identity", position = "fill")+
	scale_fill_brewer(palette = "YlGnBu")+
	facet_wrap(~`Fish Species`, dir = "v")+
	ggtitle("Prey Size Spatially (2016)")+
	theme_bw()+
	scale_y_continuous(labels=scales::unit_format("%", 100))+
	theme(panel.grid=element_blank(), strip.text = element_text(size=16),
				axis.title = element_text(size=14), axis.text = element_text(size=12),
				legend.text = element_text(size=12), legend.title = element_text(size=14),
				title = element_text(size=16), plot.title = element_text(hjust=0.5))
#plot biomass comparisons for each site! (spatial)

biomass %>%
	filter(Analysis != "Spatial" & Year=="2015") %>% 
	ggplot(aes(sampling_week, Biomass))+
	geom_bar(aes(fill=`Size Class`), stat = "identity", position = "fill")+
	scale_fill_brewer(palette = "YlGnBu")+
	facet_grid(`Fish Species`~`Sample Site`, scales = "free_x")+
	ggtitle("Prey Size Temporally (2015)")+
	theme_bw()+
	labs(x="Sample Week")+
	scale_y_continuous(labels=scales::unit_format("%", 100))+
	theme(panel.grid=element_blank(), strip.text = element_text(size=16),
				axis.title = element_text(size=14), axis.text = element_text(size=12),
				legend.text = element_text(size=12), legend.title = element_text(size=14),
				title = element_text(size=16), plot.title = element_text(hjust=0.5))
#plot biomass comparisons for 2015! (temporal)

biomass %>%
	filter(Analysis != "Spatial" & Year=="2016") %>% 
	ggplot(aes(sampling_week, Biomass))+
	geom_bar(aes(fill=`Size Class`), stat = "identity", position = "fill")+
	scale_fill_brewer(palette = "YlGnBu")+
	facet_grid(~ `Fish Species`~`Sample Site`, scales = "free")+ 
	ggtitle("Prey Size Temporally (2016)")+
	theme_bw()+
	labs(x="Sample Week")+
	scale_y_continuous(labels=scales::unit_format("%", 100))+
	theme(panel.grid=element_blank(), strip.text = element_text(size=16),
				axis.title = element_text(size=14), axis.text = element_text(size=12),
				legend.text = element_text(size=12), legend.title = element_text(size=14),
				title = element_text(size=16), plot.title = element_text(hjust=0.5))
#plot biomass comparisons for 2016! (temporal)

##### Prey composition by ABD #####

abdmod <- fish %>%
	filter(`Taxonomic Group`!="Digested")

abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Empty")] <- "Other"
abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Bivalves")] <- "Other"
abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Cumacean")] <- "Other"
abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Cyphonauts")] <- "Other"
abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Detritus")] <- "Other"
abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Diatoms")] <- "Other"
abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Isopods")] <- "Other"
abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Objects")] <- "Other"
abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Parasites")] <- "Other"
abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Polychaetes")] <- "Other"
abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Pteropods")] <- "Other"
abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Siphonophore")] <- "Other"
abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Ostracods")] <- "Other"
#abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Cladocerans")] <- "Other"
abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Cyclopoids")] <- "Other"
abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Amphipods")] <- "Other"
#abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Decapods")] <- "Other"
abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Fish")] <- "Other"
#changed from barnacles to fish to see barnacle trends, change back later!
#abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Chaetognaths")] <- "Other"
abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Digested")] <- "Other"
#abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Echinoderms")] <- "Other"
#abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Euph_eggs")] <- "Other"
#abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Gelatinous")] <- "Other"
abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Harpacticoids")] <- "Other"
abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Ostracod")] <- "Other"
abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Mysids")] <- "Other"
abdmod$`Taxonomic Group`[which(abdmod$`Taxonomic Group`=="Insects")] <- "Other"
#simplify groups

abundance <- abdmod %>%
	group_by(UFN, `Fish Species`, `Sample Date`, `Sample Site`, `Taxonomic Group`, Analysis, `Taxonomic Detail`,
					 Year, sampling_week, `Bolus Weight (mg)`, weight, fork_length, lat, long, `Size Class`) %>%
	summarise(Abundance=sum(`Prey total`))
#%>%
#filter(Biomass!=0)
#summarize proportional abundance for each fish (& remove empty stom)

#abundance$`Taxonomic Group` <- as.factor(abundance$`Taxonomic Group`)
#abundance$`Taxonomic Group` <- factor(abundance$`Taxonomic Group`, levels(abundance$`Taxonomic Group`)[c(1, 2, 5, 8, 7, 4, 6, 3, 9, 10, 11)])

taxa_order <- c("Barnacles", "Calanoids", "Decapods", "Euphausiids",
								"Euph_eggs", "Cladocerans", "Echinoderms",
								"Chaetognaths", "Gelatinous", "Larvaceans", "Other")

abundance$`Taxonomic Group` <- factor(abundance$`Taxonomic Group`, levels = taxa_order)

abundance$`Size Class` <- as.factor(abundance$`Size Class`)
abundance$`Size Class` <- factor(abundance$`Size Class`, levels(abundance$`Size Class`)[c(1, 3, 4, 5, 2)])

mscwide <- abundance %>%
	group_by(UFN, `Fish Species`, `Sample Site`) %>%
	spread(key = `Taxonomic Group`, value = Abundance, fill = 0)
#make dataframe wide to check out data

spat_sites_abd <- abundance %>%
	filter(`Sample Site` %in% c("D11", "D09", "J02", "J06", "J08"))

temp_site_abd <- abundance %>% 
	filter(`Sample Site` == "D07" & `Sample Date`=="2016-06-16")

abd_graph <- rbind(spat_sites_abd, temp_site_abd)

abd_graph$`Sample Site` <- as.factor(abd_graph$`Sample Site`)

abd_graph$`Sample Site` <- factor(abd_graph$`Sample Site`, levels(abd_graph$`Sample Site`)[c(4, 6, 5, 3, 2, 1)])

abundance$sampling_week <- factor(abundance$sampling_week, levels = date_order)
abd_graph$sampling_week <- factor(abd_graph$sampling_week, levels = date_order)
#abd_graph$`Taxonomic Group` <- as.factor(abd_graph$`Taxonomic Group`)
#abd_graph$`Taxonomic Group` <- factor(abd_graph$`Taxonomic Group`, levels(abd_graph$`Taxonomic Group`)[c(1, 2, 5, 8, 7, 4, 6, 3, 9, 10, 11)])

abd_graph %>%
	ggplot(aes(`Sample Site`, Abundance))+
	geom_bar(aes(fill=`Taxonomic Group`), stat = "identity", position = "fill")+
	scale_fill_manual(values = c("#FB9A99", "#E31A1C", "#FDBF6F",
															 "#FF7F00", #bar, cal, dec, euph
															 #"#FFFF99", #euph_eggs
															 "#CAB3D6", #cladocerans
															 "#6A3D9A", #echinoderms
															 "#A6CEE3", "#1F78B4", #chae, gel
															 "#33A02C", "grey60"))+#lar, oth
	facet_wrap(~`Fish Species`, dir = "v", scales = "free")+
	ggtitle("Spatial Diets (2016)")+
	theme_bw()+
	scale_y_continuous(labels=scales::unit_format("%", 100))+
	theme(panel.grid=element_blank(), strip.text = element_text(size=16),
				axis.title = element_text(size=14), axis.text = element_text(size=12),
				legend.text = element_text(size=12), legend.title = element_text(size=14),
				title = element_text(size=16), plot.title = element_text(hjust=0.5))
#plot abundance comparisons for each site! (spatial)
#relevant prey: 

#kill cladocerans and echinoderms! *****

abundance %>%
	filter(Analysis != "Spatial" & Year=="2015") %>% 
	ggplot(aes(sampling_week, Abundance))+
	geom_bar(aes(fill=`Taxonomic Group`), stat = "identity", position = "fill")+
	scale_fill_manual(values = c("#FB9A99", "#E31A1C", "#FDBF6F",
															 "#FF7F00", #bar, cal, dec, euph
															 "#FFFF99", #euph_eggs
															 "#CAB3D6", #cladocerans
															 "#6A3D9A", #echinoderms
															 "#A6CEE3", "#1F78B4", #chae, gel
															 "#33A02C", "grey60"))+#lar, oth
	facet_grid(`Fish Species`~`Sample Site`, scales = "free_x"
						 )+
	ggtitle("2015 Temporal Diets")+
	labs(x="Sample Week")+
	theme_bw()+
	scale_y_continuous(labels=scales::unit_format("%", 100))+
	theme(panel.grid=element_blank(), strip.text = element_text(size=16),
				axis.title = element_text(size=14), axis.text = element_text(size=12),
				legend.text = element_text(size=12), legend.title = element_text(size=14),
				title = element_text(size=16), plot.title = element_text(hjust=0.5))
#plot abundance comparisons for D07 site! (temporal)
#relevant prey: 

#kill euph_eggs and echinoderms! *****

abundance %>%
	filter(Analysis != "Spatial" & Year=="2016") %>% 
	ggplot(aes(sampling_week, Abundance))+
	geom_bar(aes(fill=`Taxonomic Group`), stat = "identity", position = "fill")+
	scale_fill_manual(values = c("#FB9A99", "#E31A1C", "#FDBF6F",
															 "#FF7F00", #bar, cal, dec, euph
															 "#FFFF99", #euph_eggs
															 "#CAB3D6", #cladocerans
															 "#6A3D9A", #echinoderms
															 "#A6CEE3", "#1F78B4", #chae, gel
															 "#33A02C", "grey60"))+#lar, oth
	facet_grid(~ `Fish Species`~`Sample Site`, scales = "free")+
	ggtitle("2016 Temporal Diets")+
	labs(x = "Sample Week")+
	theme_bw()+
	scale_y_continuous(labels=scales::unit_format("%", 100))+
	theme(panel.grid=element_blank(), strip.text = element_text(size=16),
				axis.title = element_text(size=14), axis.text = element_text(size=12),
				legend.text = element_text(size=12), legend.title = element_text(size=14),
				title = element_text(size=16), plot.title = element_text(hjust=0.5))
#keep everything (incl cladocerans)

#summary stuff
#abundance %>%
#	filter(`Taxonomic Group`=="Other") %>% 
#	group_by(`Fish Species`, `Sample Date`, `Sample Site`, `Taxonomic Detail`) %>%
#	summarise(Abundance=mean(Abundance)) %>%
#	View()

##### Diets by size class (ABD) #####

abd_graph %>%
	ggplot(aes(`Sample Site`, Abundance))+
	geom_bar(aes(fill=`Size Class`), stat = "identity", position = "fill")+
	scale_fill_brewer(palette = "YlGnBu")+
	facet_wrap(~`Fish Species`, dir = "v")+
	ggtitle("Prey Size Spatially (2016)")+
	theme_bw()+
	scale_y_continuous(labels=scales::unit_format("%", 100))+
	theme(panel.grid=element_blank(), strip.text = element_text(size=16),
				axis.title = element_text(size=14), axis.text = element_text(size=12),
				legend.text = element_text(size=12), legend.title = element_text(size=14),
				title = element_text(size=16), plot.title = element_text(hjust=0.5))
#plot abd comparisons for each site! (spatial)

abundance %>%
	filter(Analysis != "Spatial" & Year=="2015") %>% 
	ggplot(aes(sampling_week, Abundance))+
	geom_bar(aes(fill=`Size Class`), stat = "identity", position = "fill")+
	scale_fill_brewer(palette = "YlGnBu")+
	facet_grid(`Fish Species`~`Sample Site`, scales = "free_x")+
	ggtitle("Prey Size Temporally (2015)")+
	theme_bw()+
	labs(x="Sample Week")+
	scale_y_continuous(labels=scales::unit_format("%", 100))+
	theme(panel.grid=element_blank(), strip.text = element_text(size=16),
				axis.title = element_text(size=14), axis.text = element_text(size=12),
				legend.text = element_text(size=12), legend.title = element_text(size=14),
				title = element_text(size=16), plot.title = element_text(hjust=0.5))
#plot abd comparisons for 2015! (temporal)

abundance %>%
	filter(Analysis != "Spatial" & Year=="2016") %>% 
	ggplot(aes(sampling_week, Abundance))+
	geom_bar(aes(fill=`Size Class`), stat = "identity", position = "fill")+
	scale_fill_brewer(palette = "YlGnBu")+
	facet_grid(~ `Fish Species`~`Sample Site`, scales = "free")+ 
	ggtitle("Prey Size Temporally (2016)")+
	theme_bw()+
	labs(x="Sample Week")+
	scale_y_continuous(labels=scales::unit_format("%", 100))+
	theme(panel.grid=element_blank(), strip.text = element_text(size=16),
				axis.title = element_text(size=14), axis.text = element_text(size=12),
				legend.text = element_text(size=12), legend.title = element_text(size=14),
				title = element_text(size=16), plot.title = element_text(hjust=0.5))
#plot abd comparisons for 2016! (temporal)


##### Diets by digested state (WW) #####

class(biom_graph$`Prey Digestion Index`) <- "character"
class(biomass$`Prey Digestion Index`) <- "character"

biom_graph %>%
	ggplot(aes(`Sample Site`, Biomass))+
	geom_bar(aes(fill=`Prey Digestion Index`), stat = "identity", position = "fill")+
	scale_fill_brewer(palette = "RdPu", labels=c("0 - Undigestable", "1 - Fresh", "2 - Semi-Fresh", "3 - Semi-Digested", "4 - Digested"))+
	facet_wrap(~`Fish Species`, dir = "v")+
	ggtitle("Digestion Index Spatially (2016)")+
	labs(fill="Digestion Index")+
	theme_bw()+
	scale_y_continuous(labels=scales::unit_format("%", 100))+
	theme(panel.grid=element_blank(), strip.text = element_text(size=16),
				axis.title = element_text(size=14), axis.text = element_text(size=12),
				legend.text = element_text(size=12), legend.title = element_text(size=14),
				title = element_text(size=16), plot.title = element_text(hjust=0.5))
#plot DS comparisons for each site! (spatial)

biomass %>%
	filter(Analysis != "Spatial" & Year=="2015") %>% 
	ggplot(aes(sampling_week, Biomass))+
	geom_bar(aes(fill=`Prey Digestion Index`), stat = "identity", position = "fill")+
	scale_fill_brewer(palette = "RdPu", labels=c("0 - Undigestable", "1 - Fresh", "2 - Semi-Fresh", "3 - Semi-Digested", "4 - Digested"))+
	facet_grid(`Fish Species`~`Sample Site`, scales = "free_x")+
	ggtitle("Digestion Index Temporally (2015)")+
	labs(x="Sample Week", fill="Digestion Index")+
	theme_bw()+
	scale_y_continuous(labels=scales::unit_format("%", 100))+
	theme(panel.grid=element_blank(), strip.text = element_text(size=16),
				axis.title = element_text(size=14), axis.text = element_text(size=12),
				legend.text = element_text(size=12), legend.title = element_text(size=14),
				title = element_text(size=16), plot.title = element_text(hjust=0.5))
#plot DS comparisons for 2015! (temporal)

biomass %>%
	filter(Analysis != "Spatial" & Year=="2016") %>% 
	ggplot(aes(sampling_week, Biomass))+
	geom_bar(aes(fill=`Prey Digestion Index`), stat = "identity", position = "fill")+
	scale_fill_brewer(palette = "RdPu", labels=c("0 - Undigestable", "1 - Fresh", "2 - Semi-Fresh", "3 - Semi-Digested", "4 - Digested"))+
	facet_grid(~ `Fish Species`~`Sample Site`, scales = "free")+ 
	ggtitle("Digestion Index Temporally (2016)")+
	labs(x="Sample Week", fill="Digestion Index")+
	theme_bw()+
	scale_y_continuous(labels=scales::unit_format("%", 100))+
	theme(panel.grid=element_blank(), strip.text = element_text(size=16),
				axis.title = element_text(size=14), axis.text = element_text(size=12),
				legend.text = element_text(size=12), legend.title = element_text(size=14),
				title = element_text(size=16), plot.title = element_text(hjust=0.5))
#plot DS comparisons for 2016! (temporal)

#DS plots by % abundance isn't relevant because can't count heavily digested items/jellies

##### GFI and Lengths #####

notaxadata <- biom_graph %>%
  ungroup() %>% 
	select(UFN, `Fish Species`, `Sample Site`, `Sample Date`, weight, fork_length, `Bolus Weight (mg)`,
	       #, region, lat, lon, `Clipped Adipose Fin`
	       lat, long) %>%
	unique()

notaxadata$`Fish Species` <- as.factor(notaxadata$`Fish Species`)

notaxadata$`Fish Species` <- factor(notaxadata$`Fish Species`, levels(notaxadata$`Fish Species`)[c(2, 1)])

updatedfishdata <- notaxadata %>%
  mutate(food_w_g = `Bolus Weight (mg)`/1000) %>% 
  mutate(calc_gfi=food_w_g/weight*100)
#double check this calculation later...

updatedfishdata %>% 
	ggplot(aes(`Sample Site`, calc_gfi))+
	geom_boxplot(aes(fill=`Fish Species`))+
	labs(title="Spatial Gut Fullness Index", y="GFI (% Body Weight)")+
  theme_bw()+
	scale_fill_discrete(labels=c("Pink", "Chum"))+
	theme(panel.grid=element_blank(), strip.text = element_text(size=16),
				axis.title = element_text(size=14), axis.text = element_text(size=12),
				legend.text = element_text(size=12), legend.title = element_text(size=14),
				title = element_text(size=16), plot.title = element_text(hjust=0.5))
#GFI for spatial

notaxadata %>% 
	ggplot(aes(`Sample Site`, fork_length))+
	geom_boxplot(aes(fill=`Fish Species`))+
	labs(title="Spatial Fish Length", y="Fork Length (mm)")+
  theme_bw()+
	scale_fill_discrete(labels=c("Pink", "Chum"))+
	theme(panel.grid=element_blank(), strip.text = element_text(size=16),
				axis.title = element_text(size=14), axis.text = element_text(size=12),
				legend.text = element_text(size=12), legend.title = element_text(size=14),
				title = element_text(size=16), plot.title = element_text(hjust=0.5))
#Length for spatial

notaxatemp <- abundance %>%
  ungroup() %>% 
  select(UFN, `Fish Species`, `Sample Site`, `Sample Date`, weight, fork_length,
         `Bolus Weight (mg)`, Analysis, sampling_week, Year
         #, region, lat, lon, `Clipped Adipose Fin`
  ) %>%
  filter(Analysis!="Spatial" & `Sample Site` %in% c("D07", "J07")) %>% 
  unique()

notaxatemp$`Fish Species` <- as.factor(notaxatemp$`Fish Species`)

notaxatemp$`Fish Species` <- factor(notaxatemp$`Fish Species`, levels(notaxatemp$`Fish Species`)[c(2, 1)])

updatedtempdata <- notaxatemp %>%
  mutate(food_w_g = `Bolus Weight (mg)`/1000) %>% 
  mutate(calc_gfi=food_w_g/weight*100)

updatedtempdata %>% 
  ggplot(aes(sampling_week, calc_gfi))+
  geom_boxplot(aes(fill=`Fish Species`))+
  labs(title="Temporal Gut Fullness Index", y="GFI (% Body Weight)", x="Sample Week")+
  facet_grid(Year~`Sample Site`, scales = "free")+
  theme_bw()+
	scale_fill_discrete(labels=c("Pink", "Chum"))+
	theme(panel.grid=element_blank(), strip.text = element_text(size=16),
				axis.title = element_text(size=14), axis.text = element_text(size=12),
				legend.text = element_text(size=12), legend.title = element_text(size=14),
				title = element_text(size=16), plot.title = element_text(hjust=0.5))
#GFI for temporal D07

analysisdata <- read_csv("final_sample_list.csv")

joineddata <- left_join(metadata, analysisdata)

library(stringr)
lengthdata <- joineddata %>%
	filter(Analysis!="Spatial" & 
				 	Priority=="Yes" &
				 	site_id %in% c("D07", "J07")) %>% 
	mutate(Year=(str_sub(semsp_id, end=4)))

lengthdata$species <- as.factor(lengthdata$species)

lengthdata$species <- factor(lengthdata$species, levels(lengthdata$species)[c(2, 1)])

lengthdata$sampling_week <- factor(lengthdata$sampling_week, levels = date_order)

datedata <- lengthdata %>% 
	mutate(date_no_yr=str_sub(date, -5))
#?

lengthdata %>% 
  ggplot(aes(sampling_week, fork_length))+
  geom_boxplot(aes(fill=species))+
  labs(title="Temporal Fish Length", y="Fork Length", x="Sample Week",
  		 fill="Fish Species")+
  facet_grid(Year~site_id, scales = "free_x")+
  theme_bw()+
	scale_fill_discrete(labels=c("Pink", "Chum"))+
	theme(panel.grid=element_blank(), strip.text = element_text(size=16),
				axis.title = element_text(size=14), axis.text = element_text(size=12),
				legend.text = element_text(size=12), legend.title = element_text(size=14),
				title = element_text(size=16), plot.title = element_text(hjust=0.5))
#length for temporal

##### K condition calculations #####

temporalk <- lengthdata %>%
	mutate(k=((100000*weight)/(fork_length^3)))

spat_join <- joineddata %>%
	filter(site_id %in% c("D11", "D09", "J02", "J06", "J08"))

temp_join <- joineddata %>% 
	filter(site_id == "D07" & date=="2016-06-16")

spat_data <- rbind(spat_join, temp_join)

spatialk <- spat_data %>%
	mutate(k=((100000*weight)/(fork_length^3)))

spatialk$site_id <- as.factor(spatialk$site_id)

spatialk$site_id <- factor(spatialk$site_id, levels(spatialk$site_id)[c(4, 6, 5, 3, 2, 1)])

spatialk %>% 
	ggplot(aes(site_id, k))+
	geom_boxplot(aes(fill=species))+
	labs(title="Spatial Condition", y="Fulton's K", x="Sample Site", fill="Fish Species")+
	theme_bw()+
	scale_fill_discrete(labels=c("Pink", "Chum"))+
	theme(panel.grid=element_blank(), strip.text = element_text(size=16),
				axis.title = element_text(size=14), axis.text = element_text(size=12),
				legend.text = element_text(size=12), legend.title = element_text(size=14),
				title = element_text(size=16), plot.title = element_text(hjust=0.5))
#K for spatial

temporalk %>% 
	ggplot(aes(sampling_week, k))+
	geom_boxplot(aes(fill=species))+
	labs(title="Temporal Condition", y="Fulton's K", x="Sample Week", fill="Fish Species")+
	theme_bw()+
	facet_grid(Year~site_id, scales="free")+
	scale_fill_discrete(labels=c("Pink", "Chum"))+
	theme(panel.grid=element_blank(), strip.text = element_text(size=16),
				axis.title = element_text(size=14), axis.text = element_text(size=12),
				legend.text = element_text(size=12), legend.title = element_text(size=14),
				title = element_text(size=16), plot.title = element_text(hjust=0.5))
#K for spatial

##### NMDS????? TBD #####
##### DIVERSITY INDICES????? TBD #####