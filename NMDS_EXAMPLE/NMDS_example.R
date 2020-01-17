# NMDS Code for Vanessa's Temporal Analysis #

#If you already have your matrix ready then skip ahead to the NMDS calculation + graph!

##### Matrix Set Up #####

remove(list = ls())
#clear R environment

library(tidyverse)
library(vegan)
library(RColorBrewer)
#load relevant libraries

#set working directory as needed (download my temporal_diet_data_wide.csv for reference)

temporal_data <- read_csv("NMDS_EXAMPLE/temporal_diet_data_wide.csv")
#load in raw biomass diet data
#wide format: fish ID, species, sites and prey as columns; with each stomach as a row
#I've already filtered out any empty stomachs or outliers that would mess up analysis

diet_data_only <- temporal_data %>%
  select(Acartia:Tortanus_discaudatus)
#create dataframe with only prey categories

#create vectors for components you want to add to NMDS graph and in calculation:
site_ids <- temporal_data$sample_site
#in this case: regions (D07 and J07) are my groups for calculation and ellipses in graph
#NOTE: ellipses will return an error if you try to have a group with < 3 data points
species_ids <- temporal_data$fish_species
#species (pink and chum) will have different shapes in the NMDS graph (optional step)

#create matrix for NMDS calculation:
diet_matrix <- as.matrix(diet_data_only)
#change diet dataframe into a matrix
class(diet_matrix) <- "numeric"
#make sure your numbers are treated as numbers
proportions_matrix <- decostand(diet_matrix, "total")
#calculations proportional biomass for each fish stomach
#total = 1 so it's expressed as a decimal. It is NOT total = 100 and a percentage.
transformed_matrix <- asin(sqrt(proportions_matrix))
#arc sine square root transformation of diet data

##### NMDS Calculation and Graph #####

rankindex(site_ids, transformed_matrix, indices = c("euc", "man", "gow", "bra", "kul"))
#can compare data against dissimilarity metrics (but I still always use bray curtis...)

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc<- metaMDS(transformed_matrix,distance="bray",labels=site_ids, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
#calculates NMDS and gives initial plot
#Calculation should converge before 100 tries, also stress should be below 0.2
#try filtering out outliers or combining some data if can't converge or stress is high

permanova_eco.bc<-adonis(transformed_matrix ~ site_ids, permutations = 999, method="bray")
permanova_eco.bc #if significant, then plot it
#PERMANOVA calculation (this isn't officially part of NMDS, it's a test on it's own.)

NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=site_ids)
#dataframe for plotting NMDS

ord.bc<-ordiellipse(eco.nmds.bc,site_ids,display="sites",kind="sd", conf = 0.95, label=T)
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
#fancy dataframe creation for plotting that I don't fully understand, but trust it! :)

ggplot(NMDS.bc, aes(NMDS1.bc, NMDS2.bc))+
  geom_point(stat = "identity", aes(shape=species_ids, fill=site_ids), size=3)+
  #to change color: fill = other_ids
  geom_path(data=df_ell.bc, aes(x=NMDS1, y=NMDS2,colour=group), size=1, linetype=2) +
  scale_shape_manual(values=c(21, 22), name="Species")+
  scale_fill_manual(values=c("#053061", "#B2182B"), name="Region", guide="legend") +
  guides(fill= guide_legend(override.aes = list(shape=21)))+
  scale_colour_manual(values=c("#053061", "#B2182B"), guide=FALSE) +
  theme_bw()+
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(angle=90,size=12),
        axis.text.y=element_text(size=12),
        panel.grid.minor=element_blank(),panel.grid.major=element_blank())+
  coord_fixed() +
  labs(title="NMDS of Pink and Chum Diet Composition in DI-JS",
       x="NMDS1, Proportion-based dissimilarity",
       y="NMDS2, Proportion-based dissimilarity")+
  annotate("text",x=1.25,y=-1.5,label="(stress = 0.15)",size=4, hjust = -0.1)
#NMDS graph for the different sites! (It should look like my saved graph in folder)

#for simplicity, I'm not including how I color the data points by sampling date
#let me know if I need to explain any of my coding better, I hope it's clear!!