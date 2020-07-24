# Spatial analysis code for MSc thesis on juvenile pink and chum salmon diets #

#note: updated raw data to be survey_date and site_id
#instead of sample_site and sample_date * NEED TO UPDATE CODE

##### Set up (data; libraries) #####

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
library(fishualize)
#salmon color palette!
library(here)
#project oriented workflow

here()
#check msc_project working directory

spat_data_raw <- read_csv(here("processed", "spatial_pink_chum_diets.csv"))
#read in spatial diet data

calanoid_fixing <- filter(spat_data_raw, taxa_detail_calc=="Calanoida")
no_calanoids <- anti_join(spat_data_raw, calanoid_fixing)#, by=c("ufn", "vfid", "semsp_id"))
small_calanoids <- filter(calanoid_fixing, size_class %in% c("<1", "1 to 2"))
large_calanoids <- filter(calanoid_fixing, size_class %in% c("2 to 5", "5 to 10"))
small_calanoids$taxa_detail_calc <- "Calanoids_Small"
large_calanoids$taxa_detail_calc <- "Calanoids_Large"
spat_diet_data <- rbind(no_calanoids, small_calanoids, large_calanoids)

spat_diet_data <- spat_data_raw # no dig. calanoid size differentiation *

site_order <- c("J02", "J08", "J06", "D11", "D09", "D07")
#spat_diet_data$sample_site <- factor(spat_diet_data$sample_site, levels = site_order)
#reorder sites from the default of alphabetical to west to east, like on the map

species_order <- c("Pink", "Chum")
#spat_diet_data$fish_species <- factor(spat_diet_data$fish_species, levels = species_order)
#reorder species from the default of alphabetical to pink then chum, for graph reasons

spat_data_mod <- spat_diet_data
#make copy of data, in case want to access unmodified version

#load in file with old and new taxa names to be assigned
spat_names<-read.csv(here("data","spatial_taxa_category_change.csv"), stringsAsFactors=FALSE) 

#for loop that will reassign all the organism names in the data spreadsheet 
for (n in spat_names$old_category) {
  spat_data_mod$taxa_detail_calc[which(spat_data_mod$taxa_detail_calc %in% n)] <- spat_names$new_category[which(spat_names$old_category == n)]
}

spat_biomass_data <- spat_data_mod %>%
  filter(!taxa_detail_calc%in%c("Detritus", "Parasites", "Digested_food",
                                "Coscinodiscophycidae", "Phaeophyceae", "Objects")
         & prey_weight!=0
         & !ufn %in% c("U2978", #< 0.5 mg digested food = empty
                       "U5283", #< 0.1 mg digested food = empty
                       "U3501", #u3501 is parasites and cope antenna = empty
                       #"U5282", #only harpacticoids <0.1 mg
                       #"U5168", #only oikopleura <0.1 mg. not outlier though...
                       "U5161", #bunch of fish eggs and not much else...
                       "U5285", #single spider 3.5 mg
                       #"U5319", #one gammarid and fly larvae... < 5 mg
                       "U5284" #pteropods and other weird things < 3 mg
                       )) %>%
  group_by(ufn, fish_species, sample_date, sample_site, taxa_detail_calc, semsp_id,
           year, sampling_week, bolus_weight, weight, fork_length, work_area, microscope_hours) %>%
  summarise(Biomass=sum(prey_weight))
#biomass long data filtering out useless categories, empty stomachs (11), outliers (4)

spat_biomass_data_all_fish <- spat_data_mod %>%
  group_by(ufn, fish_species, sample_date, sample_site, taxa_detail_calc, semsp_id,
           year, sampling_week, bolus_weight, weight, fork_length, work_area, microscope_hours) %>%
  summarise(Biomass=sum(prey_weight))
#biomass long data for all 120 fish, including empties and outliers!

unique(spat_biomass_data$taxa_detail_calc)
#161 taxa groups --> Simplified to 86! (separated lrg/sml calanoids)

spat_data_raw_sum <-spat_data_raw %>%
  group_by(ufn, taxa_detail_calc) %>%
  summarise(Biomass=sum(prey_weight))

spat_numbers_raw <- spat_data_raw_sum %>%
  ungroup() %>%
  count(taxa_detail_calc)

spat_numbers_taxa <- spat_biomass_data %>%
  ungroup() %>%
  count(taxa_detail_calc)

spat_data_wide <- spat_biomass_data %>%
  ungroup() %>% 
  select(ufn, semsp_id, fish_species, sample_date, sample_site, work_area, bolus_weight, weight, fork_length, taxa_detail_calc, Biomass, microscope_hours) %>% 
  group_by(ufn, fish_species, sample_site) %>% 
  spread(key=taxa_detail_calc, value=Biomass, fill = 0)
#filtered wide data set that gets rid of empties and outliers for NMDS and whatnot

spat_data_wide_all_fish <- spat_biomass_data_all_fish %>%
  ungroup() %>% 
  select(ufn, semsp_id, fish_species, sample_date, sample_site, work_area, bolus_weight, weight, fork_length, taxa_detail_calc, Biomass, microscope_hours) %>% 
  group_by(ufn, fish_species, sample_site) %>% 
  spread(key=taxa_detail_calc, value=Biomass, fill = 0)
#unfiltered wide data set with all 120 fish, empties and all for freq. occur/gfi data

sum(spat_data_wide$microscope_hours)
#432 hours at the microscope for spatial alone... average time per stomach of 3.6 hours!

simple_spat_data <- spat_data_wide %>%
  select(ufn, fish_species, sample_site, work_area, Acartia:Tortanus_discaudatus)

##### Set up (matrix prep) #####

diet_matrix <- spat_data_wide %>%
  ungroup() %>%
  select(Acartia:Tortanus_discaudatus)
#create a dataframe with only taxa categories

#creating vectors - how necessary is this step?? move up to set up? delete? change to df?
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

spat_data_wide$work_area[which(spat_data_wide$work_area=="QUADRA")] <- "Discovery Islands"
spat_data_wide$work_area[which(spat_data_wide$work_area=="JOHNSTONE STRAIT")] <- "Johnstone Strait"
region_levels <- c("Johnstone Strait", "Discovery Islands")
region_names <- spat_data_wide$work_area
#region_names <- as.factor(region_names)
spat_data_wide$work_area <- factor(spat_data_wide$work_area, levels = region_levels)

#make vector with region labels (as factors) too

semsp_names <- spat_data_wide$semsp_id

simple_semsp_names <- vector(length = length(semsp_names))

for(i in 1:length(semsp_names)){
  simple_semsp_names[i] <- substring(semsp_names[i], first=12)
}
simple_semsp_names <- as.factor(simple_semsp_names)
#make vector with semsp id (for cluster dendrogram)

diet_proportions <- decostand(diet_matrix, method="total")

diets_w_labels <- cbind(ufn_names, simple_semsp_names, site_names, species_names, region_names, diet_proportions)
#reattach all relevant labels now that total calculation is done

#diets_w_labels <- cbind(ufn_names, simple_semsp_names, site_names, species_names, region_names,
#                        diet_matrix, semsp_names)
#REDO WITH RAW BIOMASS DATA (DELETE ONE OF THESE OPTIONS LATAER)

diets_ufn <- diets_w_labels %>%
  select(simple_semsp_names, Acartia:Tortanus_discaudatus)
#drop total=100 column and other label columns to have numerical matrix

#create a matrix with ufns as row names
matrix1<-as.matrix(diets_ufn)
row.names(matrix1) <- matrix1[,1]
spat_diet_matrix <- matrix1[,-1]
class(spat_diet_matrix)<-"numeric"
spat_trans_matrix <- asin(sqrt(spat_diet_matrix))
#need to rename in between matrices and dataframes better...

##### NMDS (filtered; full taxa data) #####

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc<- metaMDS(spat_trans_matrix,distance="bray",labels=region_names, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)

NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=region_names)
#plot NMDS, only once (picking Bray because all similar), no presence absence

ord.bc<-ordiellipse(eco.nmds.bc,region_names,display="sites",kind="sd", conf = 0.95, label=T)
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
  geom_point(stat = "identity", aes(shape=species_names, fill=site_names), size=3)+#, color = "black")+
  geom_path(data=df_ell.bc, aes(x=NMDS1, y=NMDS2,colour=group), size=1, linetype=2) +
  scale_shape_manual(values=c(21, 24), name="Species")+
  scale_fill_manual(values=c("#053061", "#1F78B4", "#A6CEE3", 
                             "#F781BF", "#E41A1C", "#B2182B"
                             ),
                    
                    name="Site", guide="legend") +
  guides(fill= guide_legend(override.aes = list(shape=21)))+#, order = 2),
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

ggsave(here("figs","spatial","spatial_NMDS.png"))

##### Cluster (filtered; full taxa data) #####

Bray_Curtis_Dissimilarity <- vegdist(spat_trans_matrix, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity, method = "average")
#make dendrogram data (heirarchical clustering by average linkages method)

dendr <- dendro_data(bcclust, type = "rectangle")
#put it in ggdendro form

bcd <- as.dendrogram(bcclust)
#put it in dendextend form...

bcd <- bcclust %>% 
  as.dendrogram() %>% 
  set("leaves_col")

#making labels with site and species for dendro:
simple_semsp_names <- as.character(simple_semsp_names)
site_sp_names <- vector(length = length(simple_semsp_names))
for(i in 1:length(simple_semsp_names)){
  site_sp_names[i] <- substring(simple_semsp_names[i], first=1, last=6)
}
site_sp_names <- as.factor(site_sp_names)
#make vector with semsp id (for cluster dendrogram)

data_w_site_sp_combo <- cbind(diets_w_labels, site_sp_names)

fishsp <- data_w_site_sp_combo %>%
  ungroup() %>%
  select(ufn=simple_semsp_names, Sp=site_sp_names)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "ufn")

lab <- left_join(labs, fishsp, by = "ufn")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend#, color="NA"
                                          ))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=0, color=lab$Sp), size=5) +
  coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  scale_colour_manual(values = c(#"#053061", "#1F78B4", "#A6CEE3", 
                                 #"#F781BF", "#E41A1C", "#B2182B",
                                 
                                 "#B2182B","#B2182B","#E41A1C","#E41A1C",
                                 "#F781BF","#F781BF","#053061","#053061",
      "lightseagreen", "lightseagreen",                          # "#A6CEE3","#A6CEE3",
                                 "#1F78B4","#1F78B4"#, "grey50"
    #"#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00",
    #"#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928", "grey50"
    ),
                      name="Site and Species")+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.text = element_text(size=24),
        axis.title = element_text(size=26),
        legend.text = element_text(size=24),
        legend.title = element_text(size=26),
        #panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "right")+
  labs(#title="Cluster By Fish ID"
       y="Dissimilarity")
#plot the dendrogram data for the different fish ID's

library(dendextend)

ggsave(here("figs", "spatial", "spatial_cluster.png"), width=15, height=20)
#cluster groups (top to bottom) DI CU; DI PI; J02 CU, J02 PI, J08 PI, J08 CU, J06 CU...
#outliers scattered amongst other clusters: D11 and J06 (lowest fullness, most empty!)

plot(bcd, xlab="Dissimilarity", horiz = TRUE)

#simproftest <- simprof(spat_trans_matrix, method.cluster = "average", method.distance = "braycurtis", num.expected = 100, num.simulated = 99)
#simprof.plot(simproftest)
#takes a long time to run this code...

##### Percent Overlap ##### 

summed_data <- spat_data_mod %>%
  filter(!taxa_detail_calc%in%c("Detritus", "Parasites", "Digested_food",
                                "Coscinodiscophycidae", "Phaeophyceae", "Objects")) %>%
  select(fish_species, sample_site, taxa_detail_calc, prey_weight) %>%
  group_by(fish_species, sample_site, taxa_detail_calc) %>%
  summarise(totalw=sum(prey_weight)) %>%
  spread(key=taxa_detail_calc, value=totalw, fill=0) 

sites <- summed_data$sample_site
salmon <- summed_data$fish_species

summed_matrix <- summed_data %>%
  ungroup() %>% 
  select(Acartia:Tortanus_discaudatus) %>% 
  decostand(method="total")

proportional_sums <- cbind(sites, salmon, summed_matrix)

D07sim <- proportional_sums %>%
  filter(sites=="D07") %>% 
  select(-c(sites, salmon)) %>%
  summarise_all(min) %>%
  rowSums() # 24.9 %
# 25 %

D09sim <- proportional_sums %>%
  filter(sites=="D09") %>% 
  select(-c(sites, salmon)) %>%
  summarise_all(min) %>%
  rowSums() # 33.0 %
# 33 %
# ?

D11sim <- proportional_sums %>%
  filter(sites=="D11") %>% 
  select(-c(sites, salmon)) %>%
  summarise_all(min) %>%
  rowSums() # 21.7 %
# 22 %

J02sim <- proportional_sums %>%
  filter(sites=="J02") %>% 
  select(-c(sites, salmon)) %>%
  #pink is top row, chum is bottom (doesn't actually matter)
  summarise_all(min) %>%
  rowSums() # 59.8 % almost significant... Hmm.
# 60 %

J06sim <- proportional_sums %>%
  filter(sites=="J06") %>% 
  select(-c(sites, salmon)) %>%
  summarise_all(min) %>%
  rowSums() # 4.8 %
# 5 %
# ?

J08sim <- proportional_sums %>%
  filter(sites=="J08") %>% 
  select(-c(sites, salmon)) %>%
  summarise_all(min) %>%
  rowSums() # 14.1 %
# 14 %

per_overlap <- data.frame(sample_site=c("J02", "J08", "J06", "D11", "D09", "D07"),
                          overlap=c(J02sim, J08sim, J06sim, D11sim, D09sim, D07sim))

per_overlap$sample_site <- factor(per_overlap$sample_site, levels = site_order)

duplicateddata <- data.frame(sample_site=rep(c("J02", "J08", "J06", "D11", "D09", "D07"), 20),
                             overlap=rep(c(J02sim, J08sim, J06sim, D11sim, D09sim, D07sim), 20))

##### GFI (unfiltered; no taxa data) #####

spatial_gfi_data <- spat_data_wide_all_fish %>%
  mutate(bolus_weight_g = bolus_weight/1000) %>%
  mutate(calc_gfi=bolus_weight_g/weight*100)
#stomach bolus weight (grams) / fish body weight (grams), expressed as a percentage

fishualize(option="Oncorhynchus_keta", n=5)
fishualize(option="Oncorhynchus_gorbuscha", n=5)

fishcolors %>%
  View()

spatial_gfi_overlap <- left_join(spatial_gfi_data, per_overlap, by="sample_site")

spatial_gfi_overlap %>% 
  #filter(sample_site %in% c("J06", "D11", "D09", "D07")) %>% 
  ggplot(aes(sample_site, calc_gfi))+
  #stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(aes(fill=fish_species))+
  labs(#title="Spatial Gut Fullness Index",
       y="Gut Fullness Index", fill="Species",
       x="Site")+
  theme_bw()+
  scale_fill_manual(values=c("#d294af", "#516959"))+
  geom_line(aes(y=overlap*10, x=sample_site, group=NA), color="darkred")+
  scale_y_continuous(sec.axis = sec_axis(~.*10, name="Diet Overlap (%)"))+
  theme(panel.grid=element_blank(), strip.text = element_text(size=16), axis.ticks.x = element_blank(),
        axis.title = element_text(size=14), axis.text = element_text(size=12, color="black"),
        legend.text = element_text(size=12), legend.title = element_text(size=14),
        title = element_text(size=16), plot.title = element_text(hjust=0.5),
        axis.title.y.right = element_text(color = "darkred"), axis.text.y.right = element_text(color="darkred"),
        legend.position = c(0.85, 0.85), legend.background = element_rect(color = "grey50"))#+
#  annotate("text",x=5,y=10,label="Empty n = 12", size=4, hjust = -0.1)
#GFI for spatial (1 weight=NA, which is why warning message pops up after printing)

ggsave(here("figs","spatial","spatial_GFI.png"))
#save figure into folder

gfi_table <- spatial_gfi_data %>%
  filter(is.na(calc_gfi)==FALSE) %>%
  group_by(fish_species, sample_site) %>% 
  summarise(mean=mean(calc_gfi), median=median(calc_gfi), sd=sd(calc_gfi))

write_csv(gfi_table, here("processed","spatial_gfi_means.csv"))

gfi_summary <- spatial_gfi_data %>%
  filter(is.na(calc_gfi)==FALSE) %>%
  filter(calc_gfi!=0) %>% 
  filter(sample_site != "J02") %>% 
  group_by(fish_species, work_area
           ) %>%
  summarise(mean=mean(calc_gfi), median=median(calc_gfi), sd=sd(calc_gfi))

zoop_data_ww %>%
  filter(sampleID %in% c("JSPK1122", "JSPK1123", "QPK734", "QPK751"
                         #, "QPK747" #biomass super high from diatom bloom - erronous
  )) %>%
  ggplot(aes(site, biomass))+
  geom_boxplot(aes(color=sieve))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  labs(title="Zoop Biomass (Spatial)")

zoop_names <- read_csv(here("data","zoop_names.csv"))

#for loop doesn't like data as factors
zoop_data$labID <- as.character(zoop_data$labID) 
zoop_names$old_category <- as.character(zoop_names$old_category)
zoop_names$new_category <- as.character(zoop_names$new_category)
#for loop that will go through all the organism names in the data spreadsheet 
#and for each one it will go to the names spreadsheet and reassign the name accordingly
for (n in zoop_names$old_category) {
  zoop_data$labID[which(zoop_data$labID %in% n)] <- zoop_names$new_category[which(zoop_names$old_category == n)]
}

taxa_levels <- c("Cyclopoids", "Calanoids", "Decapods", "Euphausiids",# "Insects",
                 "Harpacticoids", "Gelatinous", "Larvaceans", "Chaetognath", "Other")

color_levels <- c("#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00",# "#B2DF8A",
                  "#33A02C", "#A6CEE3", "#1F78B4", "#CAB2D6", "#6A3D9A")
#red, pink, orange, Lorange, green, Lgreen, blue, Lblue, purple, Lpurple

zoop_data$labID <- factor(zoop_data$labID, levels = taxa_levels)

zoop_data %>%
  filter(sampleID %in% c("JSPK1122", "JSPK1123", "QPK734", "QPK751", "JSPK1118",
                         "QPK747")) %>%
  ggplot(aes(site, abundance))+
  geom_bar(aes(fill=labID), stat="identity", position="fill")+
  scale_fill_manual(values=color_levels)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  labs(title="Zoop Composition (Spatial)")

##### Niche Breadth? (filtered; full taxa data) #####

spat_data_wide_info <- spat_data_wide %>%
  ungroup() %>% 
  select(semsp_id, ufn, sample_site, fish_species)

spat_data_pa <- spat_data_wide %>%
  ungroup() %>% 
  select(Acartia:Tortanus_discaudatus
         #, -Empty
         ) %>% 
  decostand(method = "pa")

totals <- vector(length = nrow(spat_data_pa))
#create an empty vector
totals <- rowSums(spat_data_pa)
#fill that vector with calculated row totals (total per stom.)
totals <- as.data.frame(totals)

spat_data_taxa_sum <- cbind(spat_data_wide_info, totals)

count(spat_data_taxa_sum)

spat_data_taxa_sum %>%
  group_by(sample_site, fish_species) %>%
  summarise(mean(totals))
#calculations of number of prey taxa for each individual fish, then summarized by site

spat_data_taxa_sum %>% 
  ggplot(aes(sample_site, totals))+
  geom_boxplot(aes(fill=fish_species))+
  labs(title="Spatial Niche Breadth", y="Number of taxanomic groups", fill="Species",
       x=NULL)+
  theme_bw()+
  theme(panel.grid=element_blank(), strip.text = element_text(size=16),
        axis.title = element_text(size=14), axis.text.y = element_text(size=12),
        legend.text = element_text(size=12), legend.title = element_text(size=14),
        title = element_text(size=16), plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(size=12))
#boxplot for simple version of niche breadth (just number of taxa in each fish stomach)

ggsave(here("figs", "spatial", "spatial_niche_breadth.png"))

summary_spat_data <- spat_biomass_data %>%
  ungroup() %>% 
  group_by(sample_site, fish_species, taxa_detail_calc) %>% 
  summarise(summary_biomass=sum(Biomass)) %>%
  ungroup() %>% 
  spread(key = taxa_detail_calc, value = summary_biomass, fill = 0)

summary_spat_data_wide_info <- summary_spat_data %>%
  select(sample_site, fish_species)

summary_spat_data_pa <- summary_spat_data %>%
  ungroup() %>% 
  select(Acartia:Tortanus_discaudatus#, -Empty
         ) %>% 
  decostand(method = "pa")

sum_totals <- vector(length = nrow(summary_spat_data_pa))
#create an empty vector
sum_totals <- rowSums(summary_spat_data_pa)
#fill that vector with calculated row totals (total per stom.)
sum_totals <- as.data.frame(sum_totals)

spat_data_taxa_summary <- cbind(summary_spat_data_wide_info, sum_totals)

count(spat_data_taxa_summary)

spat_data_taxa_summary %>%
  group_by(sample_site, fish_species) %>%
  summarise(mean(sum_totals))
#same calculations as the one above but totals taxa for each species and site combo

spat_diet_matrix <- as.data.frame(spat_diet_matrix)
rownames(spat_diet_matrix) <- NULL
site_names <- as.character(site_names)
species_names <- as.character(species_names)
site_sp_names <- as.character(site_sp_names)

spat_matrix_df <- cbind(site_names, species_names, site_sp_names, spat_diet_matrix)

spat_matrix_long <- pivot_longer(spat_matrix_df, cols=Acartia:Tortanus_discaudatus, names_to = "taxa")
#calculation includes taxa biomass = 0 values so the mean is calculated correctly! :)

spat_df_sum <- spat_matrix_long %>%
  group_by(taxa, site_names, species_names, site_sp_names) %>%
  summarise(ave_rel_biomass=mean(value))

spat_sum_wide <- spread(spat_df_sum, taxa, ave_rel_biomass)
#note: this might be the best way to do a bar graph of diet comp (after grouping taxa)

spat_sum_matrix <- spat_sum_wide %>%
  ungroup() %>%
  select(Acartia:Tortanus_discaudatus)

spat_matrix_site_sp <- as.character(spat_sum_wide$site_sp_names)
spat_matrix_site <- as.character(spat_sum_wide$site_names)
spat_matrix_sp <- as.character(spat_sum_wide$species_names)
spat_matrix_info <- cbind(spat_matrix_sp, spat_matrix_site, spat_matrix_site_sp)

spat_matrix_sqr <- spat_sum_matrix^2
spat_matrix_row_sum <- rowSums(spat_matrix_sqr)
spat_matrix_inverse <- 1/spat_matrix_row_sum
#Levins NB is ave % utilization by sp and site/whatever, calc = 1/(preyi^2+preyj^2+...)
#combine these into one line of code later? Idk, better to have more code than mistakes

spat_nb_calc <- cbind(spat_matrix_info, spat_matrix_inverse)
spat_nb_calc <- as.data.frame(spat_nb_calc) %>%
  rename(site=spat_matrix_site, species=spat_matrix_sp, site_sp=spat_matrix_site_sp, nb=spat_matrix_inverse)
spat_nb_calc$site <- factor(spat_nb_calc$site, levels = site_order)
spat_nb_calc$species <- factor(spat_nb_calc$species, levels = species_order)
spat_nb_calc$nb <- as.character(spat_nb_calc$nb)
spat_nb_calc$nb <- as.numeric(spat_nb_calc$nb)
#the niche breadth calculation was a factor for some reason... takes two steps to fix.

spat_nb_standard <- (spat_matrix_inverse-1)/84
#standardized Levins NB = (NB-1)/(N-1) where N is the number of categories (prey groups)
spat_nb <- cbind(spat_matrix_info, spat_nb_standard)
spat_nb <- as.data.frame(spat_nb) %>%
  rename(site=spat_matrix_site, species=spat_matrix_sp, site_sp=spat_matrix_site_sp, nb=spat_nb_standard)
spat_nb$site <- factor(spat_nb$site, levels = site_order)
spat_nb$species <- factor(spat_nb$species, levels = species_order)
spat_nb$nb <- as.character(spat_nb$nb)
spat_nb$nb <- as.numeric(spat_nb$nb)
#trends look the exact same for standarized and non-standarized, which is good news.

spat_nb %>% 
  ggplot(aes(site, nb))+
  geom_line(aes(group=species, color=species))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  labs(x="Site", y="Levin's NB (Standardized)", title="Spatial Niche Breadth", color="Species")

#try including empty stomachs, see if that changes it. Try less taxa categories too?

#come back to this later! ***** since we have a dataset for empties and for less taxa.

#BUT... the less taxa is the code chunk BELOW this one, so reorder/be mindful of it.

ggsave(here("figs", "spatial", "spatial_NB_calc.png"))

##### Diet Composition Bar Graphs (unfiltered; broad taxa data) #####

#spat_names$old_category = lab ID
#spat_names$new_category = stats ID
#spat_names$prey_category = matched to zoops
#spat_names$prey_group = broad groups (no other)
#spat_names$prey_group_less = broad groups (w/other)
#need to update these names later

#spat_data_copy <- spat_diet_data
spat_data_copy <- spat_data_raw
spat_data_copy$sample_site <- factor(spat_data_copy$sample_site, levels = site_order)
spat_data_copy$fish_species <- factor(spat_data_copy$fish_species, levels = species_order)

#for loop that will reassign all the organism names in the data spreadsheet 
for (n in spat_names$old_category) {
  spat_data_copy$taxa_detail_calc[which(spat_data_copy$taxa_detail_calc %in% n)] <- spat_names$prey_group_less[which(spat_names$old_category == n)]
  }

group_biomass <- spat_data_copy %>%
  group_by(ufn, fish_species, sample_date, sample_site, taxa_detail_calc, semsp_id) %>%
  summarise(prey_weight_sum=sum(prey_weight))
#summarize biomass for each fish

group_bio_wide <- group_biomass %>%
  ungroup() %>%
  select(ufn, fish_species, sample_site, taxa_detail_calc, prey_weight_sum) %>% 
  group_by(ufn, fish_species, sample_site) %>% 
  spread(key=taxa_detail_calc, value = prey_weight_sum, fill=0)
#wide data set (might not need it, but it's a good double check that n=120!)  

group_bio_mat <- group_bio_wide %>%
  ungroup() %>% 
  select(-c(ufn, fish_species, sample_site, Digested)) %>%
  decostand(method="total") %>%
  mutate(site=group_bio_wide$sample_site, fish=group_bio_wide$fish_species) %>%
  gather(key="taxa", value="rel_bio", Calanoids:Other) %>% 
  group_by(site, fish, taxa) %>%
  summarise(rel_bio=mean(rel_bio))

group_bio_mat %>%
  group_by(site, fish) %>%
  summarise(sum_bio=sum(rel_bio))

#taxa_levels <- c("Calanoids", "Cyclopoids", "Euphausiids", "Decapods", "Harpacticoids",
#                 "Insects", "Larvaceans", "Gelatinous", "Chaetognath", "Other")

#color_levels <- c("#E31A1C", "#FB9A99", "#FF7F00", "#FDBF6F", "#33A02C",
#                  "#B2DF8A", "#1F78B4", "#A6CEE3", "#6A3D9A", "#CAB2D6")
#red, pink, orange, Lorange, green, Lgreen, blue, Lblue, purple, Lpurple

taxa_levels <- c("Cyclopoids", "Calanoids", "Decapods", "Euphausiids", "Insects",
                 "Harpacticoids", "Gelatinous", "Larvaceans", "Chaetognath", "Other")

color_levels <- c("#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#B2DF8A",
                  "#33A02C", "#A6CEE3", "#1F78B4", "#CAB2D6", "#6A3D9A")
#red, pink, orange, Lorange, green, Lgreen, blue, Lblue, purple, Lpurple

group_bio_mat$taxa <- factor(group_bio_mat$taxa, levels = taxa_levels)

group_bio_mat %>%
  ggplot(aes(site, rel_bio))+
  geom_bar(aes(fill=taxa), stat="identity", position="fill"#, labels=scales::percent()
           )+
  scale_fill_manual(values = color_levels, name="Prey Groups")+
  facet_wrap(~fish, dir="v")+
  scale_y_continuous(labels= scales::percent)+
  theme_bw()+
  theme(panel.grid=element_blank(), strip.text = element_text(size=16),
        axis.ticks.x = element_blank(), axis.text.x = element_text(color="black"),
        axis.text.y=element_text(color="black"),
        axis.title = element_text(size=14), axis.text = element_text(size=12),
        legend.text = element_text(size=12), legend.title = element_text(size=14),
        title = element_text(size=16), plot.title = element_text(hjust=0.5))+
  labs(#title="Spatial Diet Composition", 
       x="Site", y="Relative Biomass")
#delete useless categories later

ggsave(here("figs","spatial","taxa_comp_spatial.png"))

##### Frequency of occurrence (unfiltered; full taxa data)  ######

pa_diet_matrix <- spat_data_wide_all_fish %>%
  ungroup %>% 
  select(Acartia:Tortanus_discaudatus) %>%
  decostand(method="pa") 

pa_diet_data <- cbind(spat_data_wide_all_fish$sample_site, spat_data_wide_all_fish$fish_species, pa_diet_matrix)%>%
  rename(site_id=`spat_data_wide_all_fish$sample_site`, fish_sp=`spat_data_wide_all_fish$fish_species`)

freq_occur_data <- pa_diet_data %>% 
  group_by(site_id, fish_sp) %>%
  summarise_all(list(sum)) #how to divide by 10??
#freq occur for all taxa, sites and species (spatial)

pa_diet_data <- cbind(spat_data_wide_all_fish$work_area, spat_data_wide_all_fish$fish_species, pa_diet_matrix)%>%
  rename(region_id=`spat_data_wide_all_fish$work_area`, fish_sp=`spat_data_wide_all_fish$fish_species`)

freq_occur_data <- pa_diet_data %>% 
  group_by(region_id, fish_sp) %>%
  summarise_all(list(sum))

freq_matrix <- freq_occur_data %>%
  ungroup() %>% 
  select(-region_id, -fish_sp)

View(freq_matrix/30*100)
#freq occur for all taxa, region and species (spatial) - JS PI, JS CU, DI PI, DI CU

pa_diet_data <- cbind(spat_data_wide_all_fish$fish_species, pa_diet_matrix)%>%
  rename(fish_sp=`spat_data_wide_all_fish$fish_species`)

freq_occur_data <- pa_diet_data %>% 
  group_by(fish_sp) %>%
  summarise_all(list(sum))

freq_matrix <- freq_occur_data %>%
  ungroup() %>% 
  select(-fish_sp)

View(freq_matrix/60*100)
#freq occur for all taxa, species only (spatial) - first row is pink then chum

pa_diet_data <- cbind(spat_data_wide_all_fish$sample_site, pa_diet_matrix)%>%
  rename(site_id=`spat_data_wide_all_fish$sample_site`)

freq_occur_data <- pa_diet_data %>% 
  group_by(site_id) %>%
  summarise_all(list(sum))

freq_matrix <- freq_occur_data %>%
  ungroup() %>% 
  select(-site_id)

calc_freq_matrix <- freq_matrix/20*100 #or could just do *5 but this is more clear: n=20, then as a %
calc_freq_data <- cbind(sites=site_order, calc_freq_matrix)

freq_data_long <- gather(calc_freq_data, "Taxa", "Frequency", Acartia:Tortanus_discaudatus)

freq_data_long %>%
  filter(Taxa %in% c("Calanus_marshallae", "Calanus_pacificus", "Euphausiidae",
                     "Euphausiidae_Furcilia", "Calanoida", "Themisto_pacifica",
                     "Eukrohnia_hamata", "Euphausia_pacifica", "Oikopleura",
                     "Pseudocalanus")) %>%
  View()
#those with >75% occurrence by site, picked 'em out manually
#freq occur for all taxa, site only (spatial) - J02, J08, J06, D11, D09, D07 (W to E)

pa_diet_data <- cbind(spat_data_wide_all_fish$work_area, pa_diet_matrix)%>%
  rename(region_id=`spat_data_wide_all_fish$work_area`)

freq_occur_data <- pa_diet_data %>% 
  group_by(region_id) %>%
  summarise_all(list(sum))

freq_matrix <- freq_occur_data %>%
  ungroup() %>% 
  select(-region_id)

calc_freq_matrix <- freq_matrix/60*100

calc_freq_data <- cbind(regions=c("JS", "DI"), calc_freq_matrix)

freq_data_long <- gather(calc_freq_data, "Taxa", "Frequency", Acartia:Tortanus_discaudatus)

freq_data_long %>%
  filter(Taxa %in% c("Oikopleura", "Euphausiidae_Furcilia", "Calanoida",
                     "Calanus_marshallae", "Calanus_pacificus", "Pseudocalanus")) %>%
  View()
#those with >50% occurrence by region, picked 'em out manually
#freq occur for all taxa, region only (spatial) - JS then DI ("Quadra", alphabetical)

##### PERMANOVA/SIMPER (filtered; full taxa data) #####

#need to deal with empties somehowwwww. try deleting empties first (unbalanced now):

simper_reg <- simper(spat_trans_matrix, region_names)
summary(simper_reg)
#important prey for dissimilarity between regions
#oikopleura, ctenophora, calanus marshallae, cnidaria, calanoid_L, calanus pacific, etc.

simper_sp <- simper(spat_trans_matrix, species_names)
summary(simper_sp)
#important prey for dissimilarity between species
#oikopleura, ctenophora, calanus marshallae, calanoid_L, cnidaria, calanus pacificus, etc

simper_site <- simper(spat_trans_matrix, site_names)
#summary(simper_site)
#too much info? also: can do both species and region somehow?? (DI-CU etc. or no good?)

permanova_diet <- adonis2(spat_trans_matrix ~ site_names*species_names,
                          strata=region_names)
permanova_diet
#species explains 10% of variation, site explains 42%! and sp * site interaction = 13%.
#DIFFERENT DEGREES OF FREEDOM... DOES THAT AFFECT ANALYSIS SOMEHOW??? NESTEDNESS?????

anosim_diet <- anosim(x=spat_trans_matrix, grouping=c(species_names
                                                      #, 
                                                      #site_names
                                                      ), strata = 
                        site_names
                        #region_names
                      , distance = "bray")

summary(anosim_diet)

plot(anosim_diet)

##### Prey Selectivity (filtered; broad taxa data) #####

zoop_data <- read_csv(here("data","zoop_comp_data_combined.csv"))
#read in zoop data
zoop_names <- read_csv(here("data","zoop_names.csv"))
#read in zoop name data

#for loop doesn't like data as factors
zoop_data$labID <- as.character(zoop_data$labID) 
zoop_names$old_category <- as.character(zoop_names$old_category)
zoop_names$zoop_category_general <- as.character(zoop_names$zoop_category_general)
#for loop that will go through all the organism names in the data spreadsheet 
#and for each one it will go to the names spreadsheet and reassign the name accordingly
for (n in zoop_names$old_category) {
  zoop_data$labID[which(zoop_data$labID %in% n)] <- zoop_names$zoop_category_general[which(zoop_names$old_category == n)]
}

zoop_data_merged <- zoop_data %>%
  select(sampleID, site, labID, totcount) %>% 
  group_by(sampleID, site, labID) %>%
  summarise(totalcount= sum(totcount))

zoop_data_available <- zoop_data_merged %>%
  spread(key=labID, value=totalcount, fill=0) %>%
  ungroup() %>% 
  filter(sampleID %in% c("JSPK1122", "JSPK1123", "QPK734", "QPK751", "JSPK1118", "QPK747")) %>%
  mutate("Cumacean"=0, "Isopods"=0, "Ostracods"=0) %>% 
  select(site, Amphipods:Cladocerans, Cumacean,
         Cyclopoids:Insects, Isopods,
         Larvaceans, Ostracods,
         Polychaetes)

colnames(zoop_data_available)

#load in file with old and new taxa names to be assigned (broader categories)
prey_spat_names<-read.csv(here("data","spatial_taxa_category_change.csv"))

prey_spat_data <- read_csv(here("processed","spatial_pink_chum_diets.csv"))
#RELOAD in spatial diet data (to reset original taxa categories)

prey_spat_data$sample_site <- factor(prey_spat_data$sample_site, levels = site_order)

#for loop doesn't like data as factors
prey_spat_data$taxa_detail_calc <- as.character(prey_spat_data$taxa_detail_calc) 
prey_spat_names$old_category <- as.character(prey_spat_names$old_category)
prey_spat_names$prey_group <- as.character(prey_spat_names$prey_group)
#group together any taxa that occur in less than 3 stomachs

#for loop that will go through all the organism names in the data spreadsheet 
#and for each one it will go to the names spreadsheet and reassign the name accordingly
for (n in prey_spat_names$old_category) {
  prey_spat_data$taxa_detail_calc[which(prey_spat_data$taxa_detail_calc %in% n)] <- prey_spat_names$prey_group[which(prey_spat_names$old_category == n)]
}

prey_spat_data$sample_site <- factor(prey_spat_data$sample_site, levels = site_order)
#reorder sites from the default of alphabetical to west to east, like on the map

calanoid_fixing <- filter(prey_spat_data, taxa_detail_calc=="Calanoids")
no_calanoids <- anti_join(prey_spat_data, calanoid_fixing)#, by=c("ufn", "vfid", "semsp_id"))
small_calanoids <- filter(calanoid_fixing, size_class %in% c("<1", "1 to 2"))
large_calanoids <- filter(calanoid_fixing, size_class %in% c("2 to 5", "5 to 10"))
small_calanoids$taxa_detail_calc <- "Calanoids_Small"
large_calanoids$taxa_detail_calc <- "Calanoids_Large"
prey_data <- rbind(no_calanoids, small_calanoids, large_calanoids)

prey_data_merged <- prey_data %>%
  filter(bolus_weight!=0) %>% 
  select(ufn, site=sample_site, fish_species, taxa_detail_calc, prey_abund) %>%
  group_by(ufn, site, fish_species, taxa_detail_calc) %>%
  summarise(abd=sum(prey_abund)) %>% 
  spread(key = taxa_detail_calc, value=abd, fill=0) %>%
  mutate("Bryozoa"=0) %>%
  ungroup() %>% 
  select(fish_species, site, Amphipods:Bivalves, Bryozoa,
         Calanoids_Large:Decapods, Echinoderms:Ostracods, Polychaetes) %>%
  arrange(site)

colnames(prey_data_merged)

prey_data_merged$site <- as.character(prey_data_merged$site)

ivlevdata <- list()

ivlevdata$diet <- as.data.frame(prey_data_merged)

ivlevdata$avail <- as.data.frame(zoop_data_available)

myindices <- Electivity(Diet=ivlevdata$diet, Available = ivlevdata$avail, Indices = c("Ivlev"))

ivlevindex <- myindices$Ivlev

ivlevindex[is.na(ivlevindex)] <- 0

addingregions <- data.frame(Available=c("J02", "J08", "J06", "D11", "D09", "D07"),
                            Region=c("Johnstone Strait", "Johnstone Strait", "Johnstone Strait",
                                     "Discovery Islands", "Discovery Islands", "Discovery Islands"))

ivlevregions <- left_join(ivlevindex, addingregions, by="Available")

aveivlev <- ivlevregions %>%
  gather(key="Taxa", value="Ivlev", Amphipods:Polychaetes) %>% 
  group_by(Region, Record, Available, 
           Taxa) %>%
  summarise(preysel=mean(Ivlev))
#ivlev's index (-1 to 1) = (prey-zoop)/(prey+zoop) using proportional data

aveivlev$Record <- factor(aveivlev$Record, levels = species_order)

prey_order <- c("Cyclopoids", "Calanoids_Small", "Calanoids_Large",
                "Decapods", "Euphausiids", "Insects", "Harpacticoids",
                "Gelatinous", "Larvaceans", "Chaetognaths")

aveivlev$Taxa <- factor(aveivlev$Taxa, levels = prey_order)

aveivlev$Available <- factor(aveivlev$Available, levels = site_order)

aveivlev %>%
  filter(Available %in% c("D07", "D09", "D11")) %>% 
  filter(Taxa %in% c("Larvaceans", "Insects", "Harpacticoids", "Gelatinous",
                     "Euphausiids", "Decapods", "Cyclopoids", "Chaetognath",
                     "Calanoids_Small", "Calanoids_Large")) %>%  
  ggplot(aes(Taxa, preysel))+
  ggtitle("Prey Selectivity - Discovery Islands")+
  geom_bar(aes(fill=Record), stat="identity", position = "dodge")+
  facet_wrap(~Available, dir="v")+
  coord_flip()+
  theme_bw()

ggsave("figs","spatial", "DI_spatial_prey_sel.png")

aveivlev %>%
  filter(Available %in% c("J02", "J08", "J06")) %>% 
  filter(Taxa %in% c("Larvaceans", "Insects", "Harpacticoids", "Gelatinous",
                     "Euphausiids", "Decapods", "Cyclopoids", "Chaetognath",
                     "Calanoids_Small", "Calanoids_Large")) %>% 
  ggplot(aes(Taxa, preysel))+
  ggtitle("Prey Selectivity - Johnstone Strait")+
  geom_bar(aes(fill=Record), stat="identity", position = "dodge")+
  facet_wrap(~Available, dir= "v")+
  coord_flip()+
  theme_bw()

ggsave("figs","spatial", "JS_spatial_prey_sel.png")
  
#reorder sites and taxa (see notebook for details)

#BY SIZE CLASS:

size_order <- c("<1", "1 to 2", "2 to 5", "5 to 10", ">10")

zoop_size_data <- select(zoop_data, sampleID, site, size_class=sizeclass, totcount) %>%
  filter(sampleID %in% c("JSPK1122", "JSPK1123", "QPK734", "QPK751", "JSPK1118", "QPK747")) %>%
  group_by(site, size_class) %>% 
  summarize(abundance=sum(totcount)) %>%
  spread(key=size_class, value=abundance, fill=0)

prey_size_data <- select(prey_spat_data, ufn, fish_species, site=sample_site, size_class, prey_abund) %>%
  group_by(fish_species, site, size_class) %>%
  summarise(abundance=sum(prey_abund))%>%
  spread(key=size_class, value=abundance, fill=0)

prey_size_data$site <- as.character(prey_size_data$site)

ivlevsize <- list()

ivlevsize$diet <- as.data.frame(prey_size_data)

ivlevsize$avail <- as.data.frame(zoop_size_data)

sizeindices <- Electivity(Diet=ivlevsize$diet, Available = ivlevsize$avail, Indices = c("Ivlev"))

sizeindex <- sizeindices$Ivlev

sizeindex[is.na(sizeindex)] <- 0

sizeregions <- left_join(sizeindex, addingregions, by="Available")

avesize <- sizeregions %>%
  gather(key="Size", value="Index", `<1`:`5 to 10`) %>% 
  group_by(Region, Record,# Available, 
           Size) %>%
  summarise(preysel=mean(Index))
#ivlev's index (-1 to 1) = (prey-zoop)/(prey+zoop) using proportional data

avesize$Record <- factor(avesize$Record, levels = species_order)
avesize$Size <- factor(avesize$Size, levels=size_order)

avesize %>%
  #filter(Available=="D07") %>%
  filter(Region=="Johnstone Strait") %>% 
  ggplot(aes(Size, preysel))+
  geom_bar(aes(fill=Record), stat="identity", position = "dodge")+
  coord_flip()+
  theme_bw()

##### Feeding habits #####




##### Diversity Indices (filtered; full taxa data?) #####

#compare pink and chum diet diversity (and zoop diversity as well?)

#need to decide on groups - get rid of anything too coarsely defined?

##### BIO-ENV (filtered? full taxa data?) #####

#even if the NMDS graph is too tough to figure out at first, punch in the calculation!

#diet matrix (dissimilarity?) and env: temp, sal, secchi, fl, gfi, adipose, other shit?