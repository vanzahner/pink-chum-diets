# Spatial analysis code for MSc thesis on juvenile pink and chum salmon diets #

##### SET UP #####

rm(list=ls())
#remove other R stuff

library(tidyverse)
#data wrangling/graphs/read in data
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
spat_names<-read.csv("data/spatial_taxa_category_change.csv") 

#for loop doesn't like data as factors
spat_data$taxa_detail_calc <- as.character(spat_data$taxa_detail_calc) 
spat_names$old_category <- as.character(spat_names$old_category)
spat_names$new_category <- as.character(spat_names$new_category)
#group together any taxa that occur in less than 3 stomachs

#for loop that will go through all the organism names in the data spreadsheet 
#and for each one it will go to the names spreadsheet and reassign the name accordingly
for (n in spat_names$old_category) {
  spat_data$taxa_detail_calc[which(spat_data$taxa_detail_calc %in% n)] <- spat_names$new_category[which(spat_names$old_category == n)]
}

spat_biomass_data <- spat_data %>%
  filter(!taxa_detail_calc%in%c("Detritus", "Parasites", "Digested_food",
                                "Coscinodiscophycidae", "Phaeophyceae")) %>% 
  filter(prey_weight!=0 & ufn!="U3501") %>% #u3501 is parasites and cope antenna empty
  #gets rid of empties, delete line when want empties again... bad data mgmt, I know!
  filter(!ufn %in% c(#"U5168", #only oikopleura <0.1 mg
                     "U5282", #only harpacticoids <0.1 mg
                     "U5161", #bunch of fish eggs and not much else...
                     #"U5162", #? one with <0.5 mg prey
                     "U5285", #single spider 3.5 mg
                     "U5319" #one gammarid and fly larvae... < 5 mg
                     )) %>% 
  #gets rid of outliers for NMDS, so delete later when want to bring em back.
  group_by(ufn, fish_species, sample_date, sample_site, taxa_detail_calc, semsp_id,
           year, sampling_week, bolus_weight, weight, fork_length, work_area, microscope_hours) %>%
  summarise(Biomass=sum(prey_weight))
#simplify dataset and combine any redundancies (rename QUADRA area later)

# NEED TO FIX THESE DELETIONS OF OUTLIERS FOR NMDS ^^^^^ ***** #

unique(spat_biomass_data$taxa_detail_calc)
#161 taxa groups --> Simplified to 85! (what about lrg/sml calanoids?) and n=8 empties

spat_numbers_taxa <- spat_biomass_data %>%
  ungroup() %>%
  count(taxa_detail_calc)

spat_data_wide <- spat_biomass_data %>%
  ungroup() %>% 
  select(ufn, semsp_id, fish_species, sample_date, sample_site, work_area, bolus_weight, weight, fork_length, taxa_detail_calc, Biomass, microscope_hours) %>% 
  group_by(ufn, fish_species, sample_site) %>% 
  spread(key=taxa_detail_calc, value=Biomass, fill = 0)

sum(spat_data_wide$microscope_hours)
#432 hours at the microscope for spatial alone... average time per stomach of 3.6 hours!

simple_spat_data <- spat_data_wide %>%
  select(ufn, fish_species, sample_site, work_area, Acartia:Tortanus_discaudatus#, -Empty
         )

write_csv(simple_spat_data, "processed/spatial_diet_data_wide.csv")

##### Multivariate matrix prep #####

diet_matrix <- spat_data_wide %>%
  ungroup() %>%
  select(Acartia:Tortanus_discaudatus, -Empty)
#create a dataframe with only taxa categories (delete "Empty" category)

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

#do calculations to create diet data matrix:
Total <- vector(length = nrow(diet_matrix))
#create an empty vector

Total <- rowSums(diet_matrix)
#fill that vector with calculated row totals (total per stom.)

diet_matrix <- cbind(diet_matrix, Total)
#add that vector as a column to the matrix

diet_proportions <- data.frame()
#create empty dataframe to fill with transformed data!

diet_proportions <- diet_matrix/diet_matrix$Total#*100
#divide entire dataframe by row totals (total column should all = 100)
#just realized that decostand function can calc and divide by totals...

diets_w_labels <- cbind(ufn_names, simple_semsp_names, site_names, species_names, region_names, diet_proportions)
#reattach all relevant labels now that total calculation is done

#diets_w_labels <- cbind(ufn_names, simple_semsp_names, site_names, species_names, region_names,
#                        diet_matrix, semsp_names)
#REDO WITH RAW BIOMASS DATA (DELETE ONE OF THESE OPTIONS LATAER)

diets_filtered <- diets_w_labels %>%
  filter(Total!=0 & !ufn_names %in% c("U2978", #< 0.5 mg digested food = empty
                                      "U3501", #2 parasites, 1 cope antenna, dig food = empty
                                      #"U5168", #2 oikopleura heads (not empty? but < 0.1mg)
                                      #"U5282", #3 harpacticoids (not empty? but < 0.1 mg)
                                      "U5283"#, #< 0.1 mg digested food = empty
                                      #"U5285" #1 spider... outlier but not empty. 3.5 mg.
                                      ))
#filter those out with that have 0 biomass (see columns for future labels)
#manually chose those with 1 prey group = 100% because fullness is too low
#need to figure out how to strategically go through what is "empty" later.

diets_ufn <- diets_filtered %>%
  select(simple_semsp_names, Acartia:Tortanus_discaudatus)
#drop total=100 column and other label columns to have numerical matrix

site_names_filtered <- diets_filtered$site_names
#vector with site labels corresponding to the 105 fish ids
species_names_filtered <- diets_filtered$species_names
#vector with species labels corresponding to the 105 fish ids
region_names_filtered <- diets_filtered$region_names
#vector with region labels corresponding to the 105 fish ids
simple_semsp_filtered <- diets_filtered$simple_semsp_names
#vector with semsp labels corresponding to the 105 fish ids

#fix this step, it shouldn't be duplicated in two sections and datasets...

#create a matrix with ufns as row names
matrix1<-as.matrix(diets_ufn)
row.names(matrix1) <- matrix1[,1]
spat_diet_matrix <- matrix1[,-1]
class(spat_diet_matrix)<-"numeric"
spat_trans_matrix <- asin(sqrt(spat_diet_matrix))
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

#NOTICE DIFF BY SITE AND SPECIES AND REGION AND GFI????? INVESTIGATE FURTHER!

ggsave("figs/spatial_cluster.png", width=15, height=20)

#next step compare dendrograms of pink and chum separately? (or other combos)

##### GFI #####

spatial_gfi_data <- spat_data_wide %>%
  mutate(bolus_weight_g = bolus_weight/1000) %>%
  mutate(calc_gfi=bolus_weight_g/weight*100)
#stomach bolus weight (grams) / fish body weight (grams), expressed as a percentage

spatial_gfi_data %>% 
  #filter(sample_site %in% c("J06", "D11", "D09", "D07")) %>% 
  ggplot(aes(sample_site, calc_gfi))+
  geom_boxplot(aes(fill=fish_species))+
  labs(title="Spatial Gut Fullness Index", y="GFI (% Body Weight)", fill="Species",
       x="Sample Site")+
  theme_bw()+
  theme(panel.grid=element_blank(), strip.text = element_text(size=16),
        axis.title = element_text(size=14), axis.text = element_text(size=12),
        legend.text = element_text(size=12), legend.title = element_text(size=14),
        title = element_text(size=16), plot.title = element_text(hjust=0.5))+
  annotate("text",x=5,y=7,label="Empty n = 12",size=4, hjust = -0.1)
#GFI for spatial (1 weight=NA, which is why warning message pops up after printing)

ggsave("figs/spatial_GFI.png")
#save figure into folder
##### Niche Breadth #####

spat_data_wide_info <- spat_data_wide %>%
  ungroup() %>% 
  select(semsp_id, ufn, sample_site, fish_species)

spat_data_pa <- spat_data_wide %>%
  ungroup() %>% 
  select(Acartia:Tortanus_discaudatus, -Empty) %>% 
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

ggsave("figs/spatial_niche_breadth.png")

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
  select(Acartia:Tortanus_discaudatus, -Empty) %>% 
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
site_names_filtered <- as.character(site_names_filtered)
species_names_filtered <- as.character(species_names_filtered)
site_sp_names <- as.character(site_sp_names)

spat_matrix_df <- cbind(site_names_filtered, species_names_filtered, site_sp_names, spat_diet_matrix)

spat_matrix_long <- pivot_longer(spat_matrix_df, cols=Acartia:Tortanus_discaudatus, names_to = "taxa")
#calculation includes taxa biomass = 0 values so the mean is calculated correctly! :)

spat_df_sum <- spat_matrix_long %>%
  group_by(taxa, site_names_filtered, species_names_filtered, site_sp_names) %>%
  summarise(ave_rel_biomass=mean(value))

spat_sum_wide <- spread(spat_df_sum, taxa, ave_rel_biomass)
#note: this might be the best way to do a bar graph of diet comp (after grouping taxa)

spat_sum_matrix <- spat_sum_wide %>%
  ungroup() %>%
  select(Acartia:Tortanus_discaudatus)

spat_matrix_site_sp <- as.character(spat_sum_wide$site_sp_names)
spat_matrix_site <- as.character(spat_sum_wide$site_names_filtered)
spat_matrix_sp <- as.character(spat_sum_wide$species_names_filtered)
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

ggsave("figs/spatial_NB_calc.png")

##### Diet Composition Bar Graphs #####

#load in file with old and new taxa names to be assigned
spat_names<-read.csv("data/spatial_taxa_category_change.csv") 

#for loop doesn't like data as factors
spat_data$taxa_detail_calc <- as.character(spat_data$taxa_detail_calc) 
spat_names$old_category <- as.character(spat_names$old_category)
spat_names$new_category <- as.character(spat_names$new_category)
#group together any taxa that occur in less than 3 stomachs

#for loop that will go through all the organism names in the data spreadsheet 
#and for each one it will go to the names spreadsheet and reassign the name accordingly
for (n in spat_names$old_category) {
  spat_data$taxa_detail_calc[which(spat_data$taxa_detail_calc %in% n)] <- spat_names$new_category[which(spat_names$old_category == n)]
}

