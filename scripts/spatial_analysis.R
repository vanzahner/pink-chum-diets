# Spatial analysis code for MSc thesis on juvenile pink and chum salmon diets #

##### SET UP #####

rm(list=ls())
#remove other R stuff

library(readr)
#read in files
library(ggplot2)
#graphs
library(tidyverse)
#data wrangling
library(ggdendro)
#dendrograms
library(vegan)
#analysis
library(RColorBrewer)
#graph colors

setwd("/Users/Vanessa/Desktop/msc_project")
#set working directory

spat_data <- read_csv("processed/spatial_pink_chum_diets.csv")
#read in spatial diet data

#load in file with old and new taxa names to be assigned
spat_names<-read.csv("data/spatial_category_change.csv") 

#for loop doesn't like data as factors
spat_data$taxa_detail_calc <- as.character(spat_data$taxa_detail_calc) 
spat_names$old_category <- as.character(spat_names$old_category)
spat_names$new_category <- as.character(spat_names$new_category)

#for loop that will go through all the organism names in the data spreadsheet 
#and for each one it will go to the names spreadsheet and reassign the name accordingly
for (n in spat_names$old_category) {
  spat_data$taxa_detail_calc[which(spat_data$taxa_detail_calc %in% n)] <- spat_names$new_category[which(spat_names$old_category == n)]
}

spat_biomass_data <- spat_data %>%
  group_by(ufn, fish_species, sample_date, sample_site, taxa_detail_calc, semsp_id,
           year, sampling_week, bolus_weight, weight, fork_length, work_area, microscope_hours) %>%
  summarise(Biomass=sum(prey_weight))
#simplify dataset and combine any redundancies (rename QUADRA area later)

unique(spat_biomass_data$taxa_detail_calc)
#166 taxa groups --> Simplified to 118. (what about lrg/sml calanoids?) and n=8 empties

spat_numbers_taxa <- spat_biomass_data %>%
  ungroup() %>%
  count(taxa_detail_calc)

spat_data_wide <- spat_biomass_data %>%
  ungroup() %>% 
  select(ufn, semsp_id, fish_species, sample_site, work_area, bolus_weight, weight, fork_length, taxa_detail_calc, Biomass, microscope_hours) %>% 
  group_by(ufn, fish_species, sample_site) %>% 
  spread(key=taxa_detail_calc, value=Biomass, fill = 0)

sum(spat_data_wide$microscope_hours)
#432 hours at the microscope for spatial alone... average time per stomach of 3.6 hours!

##### Multivariate matrix prep #####

diet_matrix <- spat_data_wide %>%
  ungroup() %>%
  select(Acartia:Ditrichocorycaeus_anglicus_Copepodite, Epilabidocera_amphrites:Triconia_conifera)
#create a dataframe with only taxa categories (delete "Empty" category)

spat_data_wide$ufn <- as.factor(spat_data_wide$ufn)
#change ufn to factors for this dataframe (necessary?)
ufn_names <- spat_data_wide$ufn
#create a vector for ufn info to be reattached after calc

site_names <- spat_data_wide$sample_site
#make vector for sites, same as before for ufns
site_names <- as.factor(site_names)
#make factor to avoid any possible errors

species_names <- spat_data_wide$fish_species
species_names <- as.factor(species_names)
#make vector with fish species labels (as factors) too

region_names <- spat_data_wide$work_area
region_names <- as.factor(region_names)
#make vector with region labels (as factors) too

semsp_names <- spat_data_wide$semsp_id

simple_semsp_names <- vector(length = length(semsp_names))

for(i in 1:length(semsp_names)){
  simple_semsp_names[i] <- substring(semsp_names[i], first=12)
}

simple_semsp_names <- as.factor(simple_semsp_names)
#make vector with semsp id (for cluster dendrogram)

Total <- vector(length = nrow(diet_matrix))
#create an empty vector

Total <- rowSums(diet_matrix)
#fill that vector with calculated row totals (total per stom.)

diet_matrix <- cbind(diet_matrix, Total)
#add that vector as a column to the matrix

diet_proportions <- data.frame()
#create empty dataframe to fill with transformed data!

diet_proportions <- diet_matrix/diet_matrix$Total*100
#divide entire dataframe by row totals (total column should all = 100)
#just realized that decostand function can calc and divide by totals...

diets_w_labels <- cbind(ufn_names, simple_semsp_names, site_names, species_names, region_names, diet_proportions)
#reattach all relevant labels now that total calculation is done

diets_w_labels <- cbind(ufn_names, simple_semsp_names, site_names, species_names, region_names,
                        diet_matrix, semsp_names)
#REDO WITH RAW BIOMASS DATA (DELETE ONE OF THESE OPTIONS LATAER)

diets_filtered <- diets_w_labels %>%
  filter(Total!=0 & !ufn_names %in% c("U2978", "U3501", "U5168", "U5282", "U5283", "U5285"))
#filter those out with that have 0 biomass (see columns for future labels)
#manually chose those with 1 prey group = 100% because fullness is too low
#need to figure out how to strategically go through what is "empty" later.

diets_ufn <- diets_filtered %>%
  select(simple_semsp_names, Acartia:Triconia_conifera)
#drop total=100 column and other label columns to have numerical matrix

site_names_filtered <- diets_filtered$site_names
#vector with site labels corresponding to the 105 fish ids
species_names_filtered <- diets_filtered$species_names
#vector with species labels corresponding to the 105 fish ids
region_names_filtered <- diets_filtered$region_names
#vector with region labels corresponding to the 105 fish ids
simple_semsp_filtered <- diets_filtered$simple_semsp_names
#vector with semsp labels corresponding to the 105 fish ids

#create a matrix with ufns as row names
matrix1<-as.matrix(diets_ufn)
row.names(matrix1) <- matrix1[,1]
spat_diet_matrix <- matrix1[,-1]
class(spat_diet_matrix)<-"numeric"
spat_trans_matrix <- decostand(spat_diet_matrix, "log")
#need to rename in between matrices and dataframes better...

##### NMDS #####

rankindex(region_names_filtered, spat_trans_matrix, indices = c("euc", "man", "gow", "bra", "kul"))
#it says bray is best, then kul, then man, then euc, then gow.

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc<- metaMDS(spat_trans_matrix,distance="bray",labels=region_names_filtered, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
#NO CONVERGENCE --> NEED TO FIX SOMEHOW (Simplify taxa groups even further?????)

##PERMANOVA - provides r2 and p values related to the nmds (are differences between factor levels (e.g. clusters) significant?)
permanova_eco.bc<-adonis(spat_trans_matrix ~ region_names_filtered, permutations = 999, method="bray")
permanova_eco.bc #significant! p = 0.001 so plot it
permanova_eco.eu<-adonis(spat_trans_matrix ~ region_names_filtered, permutations = 999, method="euclidean")
permanova_eco.eu #significant! p = 0.001 so plot it (has lower mean sqs and sum of sqs tho)
#this is old code, will prob do anosim+simper not permanova...

NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=region_names_filtered)
#plot NMDS, only once (picking Bray because all similar), no presence absence

ord.bc<-ordiellipse(eco.nmds.bc,region_names_filtered,display="sites",kind="sd", conf = 0.95, label=T)
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

brewer.pal(n = 5, name = "Set3")
#medium blue "#80B1D3"
brewer.pal(n = 8, name = "Set1")
#red and pink "#E41A1C" "#F781BF"
brewer.pal(n = 2, name = "Paired")
#dark and light blue "#A6CEE3" "#1F78B4"
brewer.pal(n = 4, name = "Dark2")
#hot pink "#E7298A"
brewer.pal(n=11, "RdBu")
#darker red and darker blue "#B2182B" "#2166AC"

a <- ggplot(NMDS.bc, aes(NMDS1.bc, NMDS2.bc))+
  geom_point(stat = "identity", aes(shape=species_names_filtered, fill=site_names_filtered), size=3)+#, color = "black")+
  geom_path(data=df_ell.bc, aes(x=NMDS1, y=NMDS2,colour=group), size=1, linetype=2) +
  scale_shape_manual(values=c(21, 22), name="Species")+
  scale_fill_manual(values=c("#B2182B", "#E41A1C", "#F781BF",
                             "#053061", "#A6CEE3", "#1F78B4"),
                    name="Region", guide="legend") +
  guides(fill= guide_legend(override.aes = list(shape=21)))+
  scale_colour_manual(values=c("#053061", "#B2182B")#,
                      #guide=FALSE
                      ) +
  #scale_y_continuous(limits=c(-1,1),breaks=seq(-1,1,by=.5),name = "NMDS2, Proportion-based dissimilarity")+
  #scale_x_continuous(limits=c(-0.85,0.65),breaks=seq(-0.85,0.65,by=.5),name = "NMDS1, Proportion-based dissimilarity")+
  theme_bw()+
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(angle=90,size=12),
        axis.text.y=element_text(size=12),
        panel.grid.minor=element_blank(),panel.grid.major=element_blank()#,
        #legend.position = "none"
        ) + coord_fixed() +
  annotate("text",x=-2.5,y=1.5,label="(stress = 0.17)",size=4, hjust = -0.1)
#NMDS graph for the different sites!

a

ggsave("figs/spatial_NMDS.png")

##### Cluster #####

rankindex(species_names_filtered, spat_trans_matrix, indices = c("euc", "man", "gow", "bra", "kul"))
#see which is best

Bray_Curtis_Dissimilarity <- vegdist(spat_trans_matrix, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
#make dendrogram data (heirarchical clustering by complete linkages method)

dendr <- dendro_data(bcclust, type = "rectangle")
#put it in ggdendro form

#making labels with site and species for dendro:
simple_semsp_filtered <- as.character(simple_semsp_filtered)
site_sp_names <- vector(length = length(simple_semsp_filtered))
for(i in 1:length(simple_semsp_filtered)){
  site_sp_names[i] <- substring(simple_semsp_filtered[i], first=1, last=6)
}
site_sp_names <- as.factor(site_sp_names)
#make vector with semsp id (for cluster dendrogram)

data_w_site_sp_combo <- cbind(diets_filtered, site_sp_names)

fishsp <- data_w_site_sp_combo %>%
  ungroup() %>%
  select(ufn=simple_semsp_names, Sp=site_sp_names)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "ufn")

lab <- left_join(labs, fishsp, by = "ufn")

brewer.pal(n=12, "Paired")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=0, color=lab$Sp), size=4) +
  coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("#F781BF", "#1F78B4", "white"))+
  scale_colour_manual(values = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00",
                                 "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928", "grey50"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")+
  labs(title="Cluster By Fish ID")
#plot the dendrogram data for the different fish ID's

ggsave("figs/spatial_cluster.png", width=15, height=15)

#next step compare dendrograms of pink and chum separately? (or other combos)
