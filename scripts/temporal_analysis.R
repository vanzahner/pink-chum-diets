# Temporal analysis code for MSc thesis on juvenile pink and chum salmon diets #

##### SET UP #####

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
#check working directory (should be msc_project)

temp_data_raw <- read_csv(here("processed", "temporal_pink_chum_diets.csv"))
#read in temporal diet data

temp_data <- temp_data_raw
#make a copy for modifying the taxanomic groups

#load in file with old and new taxa names to be assigned
temp_names<-read.csv(here("data", "temporal_taxa_category_change.csv"))

#for loop doesn't like data as factors
temp_data$taxa_detail_calc <- as.character(temp_data$taxa_detail_calc) 
temp_names$old_category <- as.character(temp_names$old_category)
temp_names$new_category <- as.character(temp_names$new_category)
#group together any taxa that occur in less than 3 stomachs

#for loop that will go through all the organism names in the data spreadsheet 
#and for each one it will go to the names spreadsheet and reassign the name accordingly
for (n in temp_names$old_category) {
  temp_data$taxa_detail_calc[which(temp_data$taxa_detail_calc %in% n)] <- temp_names$new_category[which(temp_names$old_category == n)]
}

site_order <- c("D07", "J07")
temp_data$sample_site <- factor(temp_data$sample_site, levels = site_order)
#reorder sites from the default of alphabetical to west to east, like on the map

species_order <- c("Pink", "Chum")
temp_data$fish_species <- factor(temp_data$fish_species, levels = species_order)
#reorder species from the default of alphabetical to pink then chum, for graph reasons

temp_biomass_data <- temp_data %>%
  filter(!taxa_detail_calc%in%c("Detritus", "Parasites", "Digested_food",
                                "Coscinodiscophycidae", "Phaeophyceae")) %>% 
  filter(prey_weight!=0) %>% 
  #delete empty stomachs?
  #filter(!ufn %in% c("U2627", "U5143", #full of barnacles...
  #                   "U5319", #gammarid
  #                   "U5404" #single prey type <0.1 mg
  #                   )) %>% 
#getting rid of outliers for nmds... change later.
  group_by(ufn, fish_species, sample_date, sample_site, taxa_detail_calc, semsp_id,
           year, sampling_week, bolus_weight, weight, fork_length, microscope_hours) %>%
  summarise(Biomass=sum(prey_weight))
#simplify dataset and combine any redundancies

unique(temp_biomass_data$taxa_detail_calc)
#176 taxa groups (way too many?) --> Simplified to 100. n=7 empties (like 100% empty.)

temp_data_raw_sum <-temp_data_raw %>%
  group_by(ufn, taxa_detail_calc) %>%
  summarise(Biomass=sum(prey_weight))

taxa_numbers_raw <- temp_data_raw_sum %>%
  ungroup() %>%
  count(taxa_detail_calc)

temp_numbers_taxa <- temp_biomass_data %>%
  ungroup() %>%
  count(taxa_detail_calc)
#grouped together anything <3 occurances and grouped together all insects/arachnids.

temp_data_wide <- temp_biomass_data %>%
  ungroup() %>% 
  select(ufn, fish_species, sample_site, bolus_weight, weight, fork_length, taxa_detail_calc, Biomass,
         microscope_hours, sample_date, year, sampling_week, semsp_id) %>% 
  group_by(ufn, fish_species, sample_site, sample_date) %>% 
  spread(key=taxa_detail_calc, value=Biomass, fill = 0)

sum(temp_data_wide$microscope_hours)
#634 hours at the microscope for temporal alone... average time per stomach of 3.0 hours!

simple_temp_data <- temp_data_wide %>%
  ungroup() %>% 
  select(ufn, fish_species, sample_site, Acartia:Tortanus_discaudatus#, -Empty
  )

write_csv(simple_temp_data, here("NMDS_EXAMPLE", "temporal_diet_data_wide.csv"))

##### Multivariate matrix prep #####

diet_matrix <- temp_data_wide %>%
  ungroup() %>%
  select(Acartia:Tortanus_discaudatus
         #, -Empty
         )
#create a dataframe with only taxa categories (delete "Empty" category)

#creating vectors - how necessary is this step?? move up to set up? delete? change to df?
temp_data_wide$ufn <- as.factor(temp_data_wide$ufn)
#change ufn to factors for this dataframe (necessary?)
ufn_names <- temp_data_wide$ufn
#create a vector for ufn info to be reattached after calc

site_names <- temp_data_wide$sample_site
#make vector for sites, same as before for ufns
site_names <- as.factor(site_names)
#make factor to avoid any possible errors

species_names <- temp_data_wide$fish_species
species_names <- as.factor(species_names)
#make vector with fish species labels (as factors) too

date_names <- temp_data_wide$sample_date
date_names <- as.factor(date_names)
#make vector with date labels (as factors) too

year_names <- temp_data_wide$year
year_names <- as.factor(year_names)
#make vector with date labels (as factors) too

week_names <- temp_data_wide$sampling_week
week_names <- as.factor(week_names)
#make vector with date labels (as factors) too

semsp_names <- temp_data_wide$semsp_id
simple_semsp_names <- vector(length = length(semsp_names))
for(i in 1:length(semsp_names)){
  simple_semsp_names[i] <- substring(semsp_names[i], first=12)
}
simple_semsp_names <- as.factor(simple_semsp_names)
#make vector with semsp id (for cluster dendrogram)

site_sp_names <- vector(length = length(semsp_names))
for(i in 1:length(semsp_names)){
  site_sp_names[i] <- substring(semsp_names[i], first=12, last=17)
}
site_sp_names <- as.factor(site_sp_names)
#make vector with site and sp id (for cluster dendrogram/nmds)

date_categories <- read_csv("data/temporal_date_id_categories.csv")
date_site_names <- vector(length = length(semsp_names))
date_id_names <- vector(length = length(semsp_names))
for(i in 1:length(semsp_names)){
  date_site_names[i] <- substring(semsp_names[i], first=1, last=14)
}
date_site_names <- as.data.frame(date_site_names)
date_site_names$date_site_names <- as.character(date_site_names$date_site_names)
for(n in date_site_names$date_site_names){
  date_site_names$date_site_names[which(date_site_names$date_site_names %in% n)] <- date_categories$date_id_names[which(date_categories$date_site_names == n)]
}
date_site_names$date_site_names <- as.factor(date_site_names$date_site_names)
#make vector with date id (for cluster dendrogram)

date_site_names$date_site_names <- as.character(date_site_names$date_site_names)
simple_date_site_names <- date_site_names$date_site_names
simple_date_site_names <- vector(length = length(date_site_names$date_site_names))
for(i in 1:length(date_site_names$date_site_names)){
  simple_date_site_names[i] <- gsub('.{3}$', '', date_site_names$date_site_names[i])
}
simple_date_site_names <- as.factor(simple_date_site_names)
#make vector with ids for site and date, but no year (for graphs)

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

diets_w_labels <- cbind(ufn_names, simple_semsp_names, site_names, species_names, date_names, site_sp_names,
                        year_names, week_names, diet_proportions, semsp_names, date_site_names, simple_date_site_names)
#reattach all relevant labels now that total calculation is done

#diets_w_labels <- cbind(ufn_names, simple_semsp_names, site_names, species_names, date_names,
#                        year_names, week_names, diet_matrix, semsp_names)
#REDO WITH RAW BIOMASS DATA (DELETE ONE OF THESE OPTIONS LATAER)

diets_filtered <- diets_w_labels %>%
  filter(Total!=0 & !ufn_names %in% c("U2627", "U5143", #barnacles.
                                      #"U5435", #mix up with acartia and jellies, fixed now!
                                      "U5346", #nothing unusual, WHY IS IT LISTED HERE?
                                      "U5319", #gammarid
                                      "U5400", #empty, it's already filtered out
                                      "U5404" #three lil'calanoids <0.1mg total
    #"U2978", "U3501", "U5168", "U5282", "U5283", "U5285"
    ))
#*double check these later
#filter those out with that have 0 biomass (see columns for future labels)
#manually chose those with 1 prey group = 100% because fullness is too low
#need to figure out how to strategically go through what is "empty" later.

diets_ufn <- diets_filtered %>%
  select(semsp_names, Acartia:Tortanus_discaudatus)
#drop total=100 column and other label columns to have numerical matrix

site_names_filtered <- diets_filtered$site_names
#vector with site labels corresponding to the 198 fish ids
species_names_filtered <- diets_filtered$species_names
#vector with species labels corresponding to the 198 fish ids
date_names_filtered <- diets_filtered$date_names
#vector with date labels corresponding to the 198 fish ids
simple_semsp_filtered <- diets_filtered$simple_semsp_names
#vector with semsp labels corresponding to the 198 fish ids
year_names_filtered <- diets_filtered$year_names
#vector with date labels corresponding to the 198 fish ids
week_names_filtered <- diets_filtered$week_names
#vector with date labels corresponding to the 198 fish ids
date_sites_filtered <- diets_filtered$date_site_names
#vector for date and site id corresponding to 198 fish ids
site_sp_names_filtered <- diets_filtered$site_sp_names
#vector for species and site id corresponding to 198 fish ids
simple_date_site_names_filtered <- diets_filtered$simple_date_site_names
#vector for date and site id (no year) corresponding to 198 fish ids
#NEED TO STREAMLINE THIS MESS OF CODE... Do it after filter???

#create a matrix with ufns as row names
matrix1<-as.matrix(diets_ufn)
row.names(matrix1) <- matrix1[,1]
temp_diet_matrix <- matrix1[,-1]
class(temp_diet_matrix)<-"numeric"
temp_trans_matrix <- asin(sqrt(temp_diet_matrix))
#need to rename in between matrices and dataframes better...

##### NMDS #####

rankindex(site_names_filtered, temp_trans_matrix, indices = c("euc", "man", "gow", "bra", "kul"))
#see which is best

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc<- metaMDS(temp_trans_matrix,distance="bray",labels=site_names_filtered, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
#converge ~20-60 varies, stress ~0.15!

stressplot(eco.nmds.bc)
#see how data scatters around line to see "fit"

##PERMANOVA - provides r2 and p values related to the nmds (are differences between factor levels (e.g. clusters) significant?)
permanova_eco.bc<-adonis(temp_trans_matrix ~ site_names_filtered, permutations = 999, method="bray")
permanova_eco.bc #significant! p = 0.001 so plot it
permanova_eco.eu<-adonis(temp_trans_matrix ~ site_names_filtered, permutations = 999, method="euclidean")
permanova_eco.eu #significant! p = 0.001 so plot it (has lower mean sqs and sum of sqs tho)
#this is old code, will prob do anosim+simper not permanova...

NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=site_names_filtered)
#plot NMDS, only once (picking Bray because all similar), no presence absence

ord.bc<-ordiellipse(eco.nmds.bc,site_names_filtered,display="sites",kind="sd", conf = 0.95, label=T)
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
brewer.pal(n = 12, name = "Paired")
#dark and light blue "#A6CEE3" "#1F78B4"
brewer.pal(n = 4, name = "Dark2")
#hot pink "#E7298A"
brewer.pal(n=11, "RdBu")
#darker red and darker blue "#B2182B" "#2166AC"

a <- ggplot(NMDS.bc, aes(NMDS1.bc, NMDS2.bc))+
  geom_point(stat = "identity", aes(shape=species_names_filtered, fill=date_sites_filtered), size=3)+#, color = "black")+
  geom_path(data=df_ell.bc, aes(x=NMDS1, y=NMDS2,colour=group), size=1, linetype=2) +
  scale_shape_manual(values=c(21, 22), name="Species")+
  scale_fill_manual(values=c("#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#FFFF99", "#B15928", "#CAB2D6", "#6A3D9A",
                             "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "grey50"),
                    name="Date", guide="legend") + #for site and date groupings
  #scale_fill_manual(values=c("#FB9A99", "#E31A1C", "#A6CEE3", "#1F78B4"),
  #                    name="Site and Sp.", guide="legend") + #for site and sp groups
  guides(fill= guide_legend(override.aes = list(shape=21)))+
  scale_colour_manual(values=c("#B2182B", "#053061"),
                      name="Site and Sp.", guide="legend") + #for site and sp groups
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
  annotate("text",x=-2.5,y=1.5,label="(stress = 0.15)",size=4, hjust = -0.1)
#NMDS graph for the different sites!

a

ggsave(here("figs", "temporal", "temporal_NMDS_detailed.png"))

#update colors to be blue=JS, red=DI and pink=light colors and chum=dark
#update shapes to be early-mid-late = circle/diamond/triangle (order tbd)

##### Cluster #####

rankindex(date_sites_filtered, temp_trans_matrix, indices = c("euc", "man", "gow", "bra", "kul"))
#see which is best

Bray_Curtis_Dissimilarity <- vegdist(temp_trans_matrix, method = "bray")
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

data_w_site_sp_combo <- cbind(diets_filtered, date_sites_filtered)

fishsp <- data_w_site_sp_combo %>%
  ungroup() %>%
  select(ufn=semsp_names, Sp=site_sp_names)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "ufn")

lab <- left_join(labs, fishsp, by = "ufn") %>%
  unique()

brewer.pal(n=12, "Paired")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1.05, angle=90, color=lab$Sp), size=2.5) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("#A6CEE3", lightblue "#1F78B4", blue "#B2DF8A", lightgreen "#33A02C", green "#FB9A99", pink
                                  #"#E31A1C", red "#FDBF6F", lightorange "#FF7F00", orange
  #                               "#CAB2D6", lightpurple "#6A3D9A", purple "#FFFF99", yellow "#B15928", brown "grey50"))+
  #scale_colour_manual(values = c("#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#FFFF99", "#B15928", "#CAB2D6", "#6A3D9A",
  #                               "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "grey50"))+
  scale_colour_manual(values = c("#FB9A99", "#E31A1C",
                                 "#A6CEE3", "#1F78B4", "grey50"))+
  scale_y_continuous(limits = c(-0.2, 1))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")+
  labs(title="Cluster By Fish ID")
#plot the dendrogram data for the different fish ID's

ggsave(here("figs", "temporal", "temporal_cluster_site_and_species.png"), width=20, height=10)

##### GFI #####

temporal_gfi_data <- temp_data_wide %>%
  mutate(bolus_weight_g = bolus_weight/1000) %>%
  mutate(calc_gfi=bolus_weight_g/weight*100) %>% 
  ungroup()
#stomach bolus weight (grams) / fish body weight (grams), expressed as a percentage

date_site_names$date_site_names <- as.character(date_site_names$date_site_names)
simple_date_site_names <- date_site_names$date_site_names
simple_date_site_names <- vector(length = length(date_site_names$date_site_names))
for(i in 1:length(date_site_names$date_site_names)){
  simple_date_site_names[i] <- gsub('.{3}$', '', date_site_names$date_site_names[i])
}
simple_date_site_names <- as.factor(simple_date_site_names)
#make vector with ids for site and date, but no year (for gfi boxplot)

temporal_gfi_dates <- cbind(temporal_gfi_data, simple_date_site_names)
#combine wide dataset with site_date ids for plotting

dat_text <- data.frame(
  label = c("Empty n = 1", "Empty n = 3", "Empty n = 0", "Empty n = 5"),
  sample_site=c("D07", "D07", "J07", "J07"),
  year=c("2015", "2016", "2015", "2016"),
  x=c(2.5, 2.5, 2.5, 2.5),
  y=c(3.75, 3.75, 3.75, 3.75)
)

temporal_gfi_dates %>% 
  ggplot(aes(simple_date_site_names, calc_gfi))+
  geom_boxplot(aes(fill=fish_species))+
  labs(title="Temporal Gut Fullness Index", y="GFI (% Body Weight)", fill="Species",
       x=NULL)+
  theme_bw()+
  scale_fill_manual(values=c("#d294af", "#516959"))+
  theme(panel.grid=element_blank(), strip.text = element_text(size=16),
        axis.title = element_text(size=14), axis.text.y = element_text(size=12),
        legend.text = element_text(size=12), legend.title = element_text(size=14),
        title = element_text(size=16), plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(size=12))+
  facet_grid(year~sample_site, scales = "free_x")+
  scale_x_discrete(labels=c("DI_Early"="May", "DI_June_Early"="Early June", "DI_June_Mid"="Mid-June",
                            "JS_June_Early"="Early June", "JS_June_Mid"="Mid-June", "JS_Late"="July"))+
  #can change these labels later to be more exact and relevant.
  geom_text(data = dat_text,
    mapping = aes(x = x, y = y, label = label),
    hjust   = 0,
    vjust   = 0
  )
#GFI for temporal (1 weight=NA, which is why warning message pops up after printing)

ggsave(here("figs", "temporal", "temporal_GFI.png"))
#save figure into folder

##### Niche Breadth #####

temp_data_wide_info <- temporal_gfi_dates %>%
  ungroup() %>% 
  select(semsp_id, ufn, sample_site, fish_species, sample_date, year, simple_date_site_names)

temp_data_pa <- temporal_gfi_dates %>%
  ungroup() %>% 
  select(Acartia:Tortanus_discaudatus#, -Empty
         ) %>% 
  decostand(method = "pa")

totals <- vector(length = nrow(temp_data_pa))
#create an empty vector
totals <- rowSums(temp_data_pa)
#fill that vector with calculated row totals (total per stom.)
totals <- as.data.frame(totals)

temp_data_taxa_sum <- cbind(temp_data_wide_info, totals)

count(temp_data_taxa_sum)

temp_data_taxa_sum %>%
  group_by(sample_site, fish_species, sample_date, year, simple_date_site_names) %>%
  summarise(mean(totals))

temp_data_taxa_sum %>% 
  ggplot(aes(simple_date_site_names, totals))+
  geom_boxplot(aes(fill=fish_species))+
  labs(title="Temporal Niche Breadth", y="Number of taxanomic groups", fill="Species",
       x=NULL)+
  theme_bw()+
  theme(panel.grid=element_blank(), strip.text = element_text(size=16),
        axis.title = element_text(size=14), axis.text.y = element_text(size=12),
        legend.text = element_text(size=12), legend.title = element_text(size=14),
        title = element_text(size=16), plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(size=12))+
  facet_grid(year~sample_site, scales = "free_x")+
  scale_x_discrete(labels=c("DI_Early"="May", "DI_June_Early"="Early June", "DI_June_Mid"="Mid-June",
                            "JS_June_Early"="Early June", "JS_June_Mid"="Mid-June", "JS_Late"="July"))
#boxplot for simple version of niche breadth (just number of taxa in each fish stomach)

ggsave(here("figs","temporal","temporal_niche_breadth.png"))

summary_temp_data <- temp_biomass_data %>%
  ungroup() %>% 
  group_by(sample_site, sample_date, fish_species, taxa_detail_calc) %>% 
  summarise(summary_biomass=sum(Biomass)) %>%
  ungroup() %>% 
  spread(key = taxa_detail_calc, value = summary_biomass, fill = 0)

summary_temp_data_wide_info <- summary_temp_data %>%
  select(sample_site, sample_date, fish_species)

summary_temp_data_pa <- summary_temp_data %>%
  ungroup() %>% 
  select(Acartia:Tortanus_discaudatus#, -Empty
         ) %>% 
  decostand(method = "pa")

sum_totals <- vector(length = nrow(summary_temp_data_pa))
#create an empty vector
sum_totals <- rowSums(summary_temp_data_pa)
#fill that vector with calculated row totals (total per stom.)
sum_totals <- as.data.frame(sum_totals)

temp_data_taxa_summary <- cbind(summary_temp_data_wide_info, sum_totals)

count(temp_data_taxa_summary)

temp_data_taxa_summary %>%
  group_by(sample_site, sample_date, fish_species) %>%
  summarise(mean(sum_totals))
#same calculations as the one above but totals taxa for each species-site-date combo

temp_diet_matrix <- as.data.frame(temp_diet_matrix)
rownames(temp_diet_matrix) <- NULL
site_names_filtered <- as.character(site_names_filtered)
species_names_filtered <- as.character(species_names_filtered)
#date_names_filtered <- as.character(date_names_filtered)
simple_date_site_names_filtered <- as.character(simple_date_site_names_filtered)
year_names_filtered <- as.character(year_names_filtered)

temp_matrix_df <- cbind(site_names_filtered, species_names_filtered, year_names_filtered, simple_date_site_names_filtered, temp_diet_matrix)

#temp_df_dates <- left_join(, temporal_gfi_dates, by="semsp_id")

temp_matrix_long <- pivot_longer(temp_matrix_df, cols=Acartia:Tortanus_discaudatus, names_to = "taxa")
#calculation includes taxa biomass = 0 values so the mean is calculated correctly! :)

temp_df_sum <- temp_matrix_long %>%
  group_by(taxa, site_names_filtered, species_names_filtered, year_names_filtered, simple_date_site_names_filtered) %>%
  summarise(ave_rel_biomass=mean(value))

temp_sum_wide <- spread(temp_df_sum, taxa, ave_rel_biomass)
#note: this might be the best way to do a bar graph of diet comp (after grouping taxa)

temp_sum_matrix <- temp_sum_wide %>%
  ungroup() %>%
  select(Acartia:Tortanus_discaudatus)

#temp_matrix_date <- as.character(temp_sum_wide$)
#change date to be site_date name IDs for ease of plotting... ***
temp_matrix_site <- as.character(temp_sum_wide$site_names_filtered)
temp_matrix_sp <- as.character(temp_sum_wide$species_names_filtered)
temp_matrix_year <- as.character(temp_sum_wide$year_names_filtered)
temp_matrix_date <- as.character(temp_sum_wide$simple_date_site_names_filtered)
temp_matrix_info <- cbind(temp_matrix_sp, temp_matrix_site, temp_matrix_date, temp_matrix_year)
#need to add year... ***
temp_matrix_sqr <- temp_sum_matrix^2
temp_matrix_row_sum <- rowSums(temp_matrix_sqr)
temp_matrix_inverse <- 1/temp_matrix_row_sum
#Levins NB is ave % utilization by sp and site/whatever, calc = 1/(preyi^2+preyj^2+...)
#combine these into one line of code later? Idk, better to have more code than mistakes

temp_nb_calc <- cbind(temp_matrix_info, temp_matrix_inverse)
temp_nb_calc <- as.data.frame(temp_nb_calc) %>%
  rename(site=temp_matrix_site, species=temp_matrix_sp, year=temp_matrix_year, date=temp_matrix_date, nb=temp_matrix_inverse)
#temp_nb_calc$site <- factor(temp_nb_calc$site, levels = site_order)
temp_nb_calc$species <- factor(temp_nb_calc$species, levels = species_order)
temp_nb_calc$nb <- as.character(temp_nb_calc$nb)
temp_nb_calc$nb <- as.numeric(temp_nb_calc$nb)
#the niche breadth calculation was a factor for some reason... takes two steps to fix.

temp_nb_standard <- (temp_matrix_inverse-1)/84
#standardized Levins NB = (NB-1)/(N-1) where N is the number of categories (prey groups)
temp_nb <- cbind(temp_matrix_info, temp_nb_standard)
temp_nb <- as.data.frame(temp_nb) %>%
  rename(site=temp_matrix_site, species=temp_matrix_sp, year=temp_matrix_year, date=temp_matrix_date, nb=temp_nb_standard)
temp_nb$species <- factor(temp_nb$species, levels = species_order)
temp_nb$nb <- as.character(temp_nb$nb)
temp_nb$nb <- as.numeric(temp_nb$nb)
#trends look the exact same for standarized and non-standarized, which is good news.

temp_nb %>% 
  ggplot(aes(date, nb))+
  geom_line(aes(group=species, color=species))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  labs(x="Date", y="Levin's NB (Standardized)", title="Temporal Niche Breadth", color="Species")+
  facet_grid(year~site, scales = "free_x")+
  scale_x_discrete(labels=c("DI_Early"="May", "DI_June_Early"="Early June", "DI_June_Mid"="Mid-June",
                            "JS_June_Early"="Early June", "JS_June_Mid"="Mid-June", "JS_Late"="July"))
ggsave("figs","temporal","temoral_NB_calc.png")
#try including empty stomachs, see if that changes it. Try less taxa categories too?

##### Diet Composition Bar Graphs #####

#load in file with old and new taxa names to be assigned
temp_names_broad <-read.csv(here("data", "taxa_broad_groups_temporal.csv"))

temp_date_ids <- read_csv(here("data", "temporal_date_id_categories_extensive.csv"))
#read in data to get relevant date ids for plotting (streamline above code too.)

temp_data_merged <- left_join(temp_data_raw, temp_date_ids)

temp_data_merged$sample_site <- factor(temp_data_merged$sample_site, levels = site_order)

#for loop doesn't like data as factors
temp_data_merged$taxa_detail_calc <- as.character(temp_data_merged$taxa_detail_calc) 
temp_names_broad$old_category <- as.character(temp_names_broad$old_category)
temp_names_broad$new_category <- as.character(temp_names_broad$new_category)
#group together any taxa that occur in less than 3 stomachs

#for loop that will go through all the organism names in the data spreadsheet 
#and for each one it will go to the names spreadsheet and reassign the name accordingly
for (n in temp_names_broad$old_category) {
  temp_data_merged$taxa_detail_calc[which(temp_data_merged$taxa_detail_calc %in% n)] <- temp_names_broad$new_category[which(temp_names_broad$old_category == n)]
}

#group_biomass <- temp_data_fixed %>%
group_biomass <- temp_data_merged %>%
  group_by(ufn, fish_species, sample_date, sample_site, taxa_detail_calc, semsp_id, date_id_names) %>%
  summarise(prey_weight_sum=sum(prey_weight))
#summarize biomass for each fish

group_bio_wide <- group_biomass %>%
  ungroup() %>%
  select(ufn, fish_species, sample_site, taxa_detail_calc, prey_weight_sum, date_id_names) %>% 
  group_by(ufn, fish_species, sample_site, date_id_names) %>% 
  spread(key=taxa_detail_calc, value = prey_weight_sum, fill=0)
#wide data set (summarized prey group data)

group_bio_mat <- group_bio_wide %>%
  ungroup() %>% 
  select(-c(ufn, fish_species, sample_site, date_id_names, Digested)) %>%
  decostand(method="total") %>%
  mutate(site=group_bio_wide$sample_site, fish=group_bio_wide$fish_species,
         date_id=group_bio_wide$date_id_names) %>%
  gather(key="taxa", value="rel_bio", Amphipods:Other
         ) %>% 
  group_by(site, date_id, fish, taxa) %>%
  summarise(rel_bio=mean(rel_bio))

group_bio_mat %>%
  spread(taxa, rel_bio) %>%
  ungroup() %>% 
  select(-c(fish, site, date_id)) %>% 
  rowSums()

group_bio_mat %>%
  group_by(site, date_id, fish, taxa) %>%
  summarise(sum_bio=sum(rel_bio)) %>%
  View()

temp_levels <- c("Amphipods", "Cyclopoids", "Calanoids", "Decapods", "Euphausiids", "Cladocerans", "Barnacles", "Echinoderms",
                 "Insects", "Harpacticoids", "Eggs", "Gelatinous", "Larvaceans", "Chaetognaths", "Other", "Fish")

color_temp <- c("#E7298A", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00",
                "#E6AB02", "#A6761D", "#666666",
                #clad yell, #barn brown, #echin grey
                "#B2DF8A", "#33A02C",
                "#1B9E77", #Eggs teal 
                "#A6CEE3", "#1F78B4", "#CAB2D6", "#6A3D9A", "black")

group_bio_mat$taxa <- factor(group_bio_mat$taxa, levels = temp_levels)

group_bio_mat %>%
  filter(date_id %in% c("DI_Early_15", "JS_June_Early_15", "DI_June_Early_15",
                              "DI_June_Mid_15", "JS_June_Mid_15", "JS_Late_15")) %>% 
  ggplot(aes(date_id, rel_bio))+
  geom_bar(aes(fill=taxa), stat="identity", position="fill"
           )+
  scale_fill_manual(breaks = c("Amphipods", "Cyclopoids", "Calanoids", "Decapods", "Euphausiids", "Cladocerans", "Barnacles",
                               "Insects", "Eggs", "Gelatinous", "Larvaceans", "Chaetognaths", "Other", "Fish"), values=color_temp)+
  facet_grid(fish~site, scales = "free")+
  theme_bw()+
  theme(panel.grid=element_blank(), strip.text = element_text(size=16),
        axis.title = element_text(size=14), axis.text = element_text(size=12),
        legend.text = element_text(size=12), legend.title = element_text(size=14),
        title = element_text(size=16), plot.title = element_text(hjust=0.5))+
  labs(title="2015 Temporal Diet Composition", x="Sample Site", y="% Biomass")

ggsave("figs/temporal_prey_comp_15.png")

group_bio_mat %>%
  filter(!date_id %in% c("DI_Early_15", "JS_June_Early_15", "DI_June_Early_15",
                              "DI_June_Mid_15", "JS_June_Mid_15", "JS_Late_15")) %>%
  ggplot(aes(date_id, rel_bio))+
  geom_bar(aes(fill=taxa), stat="identity", position="fill"
           )+
  facet_grid(fish~site, scales = "free")+
  theme_bw()+
  scale_fill_manual(breaks=c("Amphipods", "Cyclopoids", "Calanoids", "Decapods", "Euphausiids", "Cladocerans", "Barnacles", "Echinoderms",
                             "Insects", "Harpacticoids", "Eggs", "Gelatinous", "Larvaceans", "Chaetognaths", "Other"), values=color_temp)+
  theme(panel.grid=element_blank(), strip.text = element_text(size=16),
        axis.title = element_text(size=14), axis.text = element_text(size=12),
        legend.text = element_text(size=12), legend.title = element_text(size=14),
        title = element_text(size=16), plot.title = element_text(hjust=0.5))+
  labs(title="2016 Temporal Diet Composition", x="Sample Site", y="% Biomass")

ggsave("figs/temporal_prey_comp_16.png")

#get rid of cyclo, amph, harp and ins. update zoop groups! *****

##### Frequency of occurrence #####

#TBD - see spatial code (but figure out how to divide by number of rows in each group)

# OOH and idea for later: code habitat like how code updated taxa (streamlined!)

##### PERMANOVA/SIMPER #####

