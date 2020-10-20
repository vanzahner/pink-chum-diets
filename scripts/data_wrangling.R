#updated data wrangling code:

#last modified october 20, 2020

#purpose is: transform raw data into spatial and temporal data for analysis

##### SET UP ##### 

#load libraries:
library(tidyverse)
#multiple libraries (reading, manipulating and displaying data)
library(here)
#project workflow
library(vegan)
library(ggnewscale)
#make some combo graphs here now

# Create individual dataframes for spatial and temporal chapters/analysis:

spatial_info <- data.frame(site_id=c("D07", "D09", "D11", "J06", "J08", "J02"),
                           survey_date=c("2016-06-16", "2016-06-14", "2016-06-08",
                                         "2016-06-11", "2016-06-10", "2016-06-09"), stringsAsFactors = FALSE)

temporal_info <- data.frame(site_id=c("D07", "J07", "D07", "D07", "D07", "J07", "J07", "D07", "D07", "J07", "D07", "J07", "J07"),
                            survey_date=c("2015-05-21", "2015-06-02", "2015-06-05", "2015-06-07", "2015-06-13", "2015-06-14", "2015-06-29",
                                          "2016-05-19", "2016-06-03", "2016-06-03", "2016-06-16", "2016-06-20", "2016-07-05"), stringsAsFactors = FALSE)

# Read in data that's needed for more than one code chunk:

survey_data_raw <- read.csv(url("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/survey_data.csv"), stringsAsFactors = FALSE)
#read in salmon survey data file for secchi measurements and other data

survey_data_raw$survey_date <- as.Date(survey_data_raw$survey_date)
#change from character to date to calculate yday

survey_data <- mutate(survey_data_raw, year=str_sub(survey_date, end=4),
                      yday=lubridate::yday(survey_date),
                      week=lubridate::week(survey_date))
#create more manageable date categories to work with for the temporal analysis

survey_data$survey_date <- as.character(survey_data$survey_date)
#change back into character so it can merged with other datasets

##### ENVIRONMENTAL DATA #####

# Read in environmental ysi data:

ysi_data <- read.csv(url("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/ysi.csv"), stringsAsFactors = FALSE)
#read in data file for temperature and salinity, paired with salmon surveys

# Combine datasets:

survey_ysi <- left_join(survey_data, ysi_data, by=c("survey_date", "site_id"))
#combine survey data and ysi data

spat_envr_data <- semi_join(survey_ysi, spatial_info, by=c("site_id", "survey_date"))
#make datafile for only spatial analysis ocean conditions

temp_envr_data <- semi_join(survey_ysi, temporal_info, by=c("site_id", "survey_date"))
#make datafile for only temporal analysis ocean conditions

# Save spatial and temporal envr. datasets for further analysis:

write_csv(spat_envr_data, here("processed", "spatial_data", "spatial_survey_ysi.csv"))
write_csv(temp_envr_data, here("processed", "temporal_data", "temporal_survey_ysi.csv"))
#write csv files for initial transformation and saving of envr data

##### ZOOP DATA #####

# Data relationships for zooplankton:
#weight and comp data, connect by sample_id and sieve (merges 2 datasets)
#weight/comp  and tax data, connect by sample_id, adds tow_id
#weight/comp/tax and tow data, connect by tow_id
#connecting them all together gets site_id and survey_date (all metadata)

# Read in zooplankton data:

zoop_ww_data <- read.csv(here("data", "zoop_wet_weight_raw_data.csv"), stringsAsFactors = FALSE)
#read in data file that has size fractionated wet weight zoop data

zoop_comp_data <- read.csv(here("data", "zoop_taxonomic_raw_data.csv"), stringsAsFactors = FALSE)
#read in data file that has zooplankton taxonomic composition data

zoop_tax_metadata <- read.csv(url("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/zoop_tax.csv"), stringsAsFactors = FALSE)
#read in zoop tax meta data to get tow ID

zoop_tow <- read.csv(url("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/zoop_tows.csv"), stringsAsFactors = FALSE)
#read in zoop tow meta data to put it all together

# Update zoop taxa information using taxonomic columns:

zoop_taxa_data <- zoop_comp_data %>%
  mutate(taxa_info=(if_else(species=="",
                    if_else(genus=="",
                    if_else(family=="",
                    if_else(infraorder=="",
                    if_else(order=="",
                    if_else(subclass=="",
                    if_else(class=="",
                    phylum, class), subclass),
                    order), infraorder), family), genus),
                    paste(genus, species, sep="_"))),
         prey_info = (ifelse((life_stage==""), taxa_info,
                      paste(taxa_info, life_stage, sep="_"))))
#use taxonomic columns to get a final zoop column of taxa + life stage

# Combine zoop data sets:

intermediate_zoop_data <- left_join(zoop_taxa_data, zoop_ww_data, by=c("sample_id", "sieve"))
#join two zooplankton data sets together

tow_tax <- left_join(zoop_tax_metadata, zoop_tow, by="tow_id")
#join two Hakai github meta data sets together

all_zoop_data <- left_join(intermediate_zoop_data, tow_tax, by="sample_id")
#join together Hakai and processed data (filtering out whats needed)

# Create individual dataframes for spatial and temporal chapters/analysis:

spat_zoop_data <- semi_join(all_zoop_data, spatial_info, by=c("site_id", "survey_date"))
#make datafile for only spatial analysis zooplankton

temp_zoop_prep <- semi_join(all_zoop_data, temporal_info, by=c("site_id", "survey_date"))

temp_zoop_prep$survey_date <- as.Date(temp_zoop_prep$survey_date)

temp_zoop_data <- mutate(temp_zoop_prep, year=str_sub(survey_date, end=4),
                         yday=lubridate::yday(survey_date))
#make datafile for only temporal analysis zooplankton

# Save spatial and temporal zoop datasets for further analysis:

write_csv(spat_zoop_data, here("processed", "spatial_data", "spatial_zoop_data.csv"))
write_csv(temp_zoop_data, here("processed", "temporal_data", "temporal_zoop_data.csv"))
#write csv files for initial transformation and saving of zoop data

##### SALMON DATA #####

# Data relationships for salmon:
#diet and lab data, connect by ufn (important for metadata, not connecting)
#diet/lab and field data, connect by ufn, adds seine_id
#diet/lab/field and seine data, connect by seine_id, adds survey_id
#diet/lab/field/seine and survey data, connect by survey_id
#connecting them all together gets site_id and survey_date (all metadata)

# Read in salmon data files: 

diet_data <- read.csv(here("data","pink_chum_diets_raw_data.csv"), stringsAsFactors = FALSE)
#read in juvenile pink and chum salmon diets raw data file

fish_lab_data <- read.csv(url("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/fish_lab_data.csv"), stringsAsFactors = FALSE)  

fish_field_data <- read.csv(url("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/fish_field_data.csv"), stringsAsFactors = FALSE) #%>%
  #select(-species) #drop fish species column (it's redundant)
#unless column is renamed "fish_species" in Hakai data,
#then fish_species can be deleted from raw diet dataset
#since there's a conflicting species column regarding prey

fish_meta_data <- left_join(fish_lab_data, select(fish_field_data, -species), by="ufn")
#combine fish meta data files (weights, lengths, etc.)

seine_data <- read.csv(url("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/seine_data.csv"), stringsAsFactors = FALSE)
#seine data for lat long info and seine ID to connect with zoops ?

# Modify diet data to include an automatically calculated prey category:

updated_diet_data <- diet_data %>%
  mutate(taxa_info=if_else(species=="",
                   if_else(genus=="",
                   if_else(family=="",
                   if_else(infraorder=="",
                   if_else(suborder=="",
                   if_else(order=="",
                   if_else(subclass=="",
                   if_else(class=="",
                   if_else(subphylum=="",
                   if_else(phylum=="",
                    kingdom, phylum), subphylum), class), subclass),
                    order), suborder), infraorder), family), genus),
                    paste(genus, species, sep="_")),
         prey_info = ifelse(life_stage=="", taxa_info,
                             paste(taxa_info, life_stage, sep="_"))) %>%
  filter(prey_info!="Goop") #delete stomach goop, it's not a prey item
#use taxonomic columns to get a final prey column of taxa + life stage

# Join together salmon data files:

intermediate_fish_data <- left_join(updated_diet_data, fish_meta_data,
                                    by=c("ufn"))
#join tables to merge the meta data with the diet data

filter_salmon_data <- left_join(intermediate_fish_data, seine_data, by="seine_id")
#transformed dataset - deleted stomach goop since it's not a food item

all_salmon_data <- left_join(filter_salmon_data, survey_data, by=c("survey_id")) %>%
  select(ufn, fish_species, site_id, everything())
#dataset with all 312 fish (spatial + temporal), bring site_id to the front

# Create individual dataframes for spatial and temporal chapters/analysis:

spat_fish_data <- semi_join(all_salmon_data, spatial_info, by=c("site_id", "survey_date"))
#make datafile for only spatial analysis fish

temp_fish_data <- semi_join(all_salmon_data, temporal_info, by=c("site_id", "survey_date"))
#make datafile for only temporal analysis fish

# Save spatial and temporal salmon diet datasets for further analysis:

write_csv(spat_fish_data, here("processed", "spatial_data", "spatial_pink_chum_diets.csv"))
write_csv(temp_fish_data, here("processed", "temporal_data", "temporal_pink_chum_diets.csv"))
#write csv files for initial transformation and saving of diet data

# note: salmon data is merged with summarized zoop+envr (but kept detailed zoop & env)

##### CPUE #####

seine_survey_data <- left_join(seine_data, survey_data, by="survey_id")

all_fish_totals <- seine_survey_data %>%
  select(seine_id, site_id, survey_date, year, yday, so=so_total, pi=pi_total, cu=cu_total, co=co_total, he=he_total, ck=ck_total)

all_fish_calc <- all_fish_totals %>%
  gather(key="species", value ="cpue", so:ck) %>% 
  group_by(seine_id, site_id, year, yday, survey_date, species) %>%
  filter(is.na(cpue)!=TRUE & yday<191
         & site_id %in% c("D07", "D09", "D11", "J08", "J06", "J02")) %>% # cut it off at july 7th (johnson etal 2019) 
  summarise(cpue=sum(cpue)) %>%
  arrange(year, species, yday) %>%
  mutate(region=str_sub(site_id, start = 1, end=1))

full_migration_fish <- all_fish_calc %>%
  group_by(year, species#, region
           ) %>%
  summarise(peak=sum(cpue)/2, .groups="keep") %>%
  arrange(year, species#, region
          )
# to get 50% "peak outmigration"

median_fish <- all_fish_calc %>%
  group_by(year, species#, region
           ) %>%
  arrange(year, species, yday) %>% 
  summarise(cum_cpue=cumsum(cpue)) %>%
  arrange(year, species#, region
          )

median_fish_dates <- cbind(median_fish, yday=all_fish_calc$yday, survey_date=all_fish_calc$survey_date)

final_fish_dates <- left_join(median_fish_dates, full_migration_fish, by=c("year", "species"#, "region"
                                                                           )) %>%
  filter(cum_cpue>=peak) %>%
  group_by(year, species#, region
           ) %>%
  summarise(survey_date=first(survey_date), yday=first(yday))

ave_fish_dates <- left_join(median_fish_dates, full_migration_fish, by=c("year", "species")) %>%
  filter(cum_cpue>=peak) %>%
  group_by(species) %>%
  summarise(yday=first(yday))

ave_fish_dates <- final_fish_dates %>%
  group_by(species) %>%
  summarise(yday=mean(yday))

# I finally figured out how to calculate the peak outmigration period! Hooray.

# next step: make an actual table (for appendix??) of peak dates + ave for pink/chum

p <- all_fish_calc %>%
  #mutate(log_cpue=log(cpue+1)) %>% 
  filter(species %in% c("pi", "cu")
         & year %in% c("2015", "2016")
         ) %>%
  ggplot(aes(yday, cpue, group=interaction(species, year)))+
  geom_bar(aes(fill=species), stat="identity", position="dodge")+
  geom_vline(xintercept = 160, color="#516959", linetype="dashed")+
  scale_fill_manual(values=c("#516959", "#d294af"))+
  facet_wrap(~year, nrow=2)
  
mean_dates <- data.frame(
  year=c("2015", "2015", "2015", "2015", "2016", "2016", "2016", "2016"),
  species=c("pi", "cu", "pi", "cu", "pi", "cu", "pi", "cu"),
  size_code=c("lil", "lil", "big", "big", "lil", "lil", "big", "big"),
  yday=c(175, 160, 171, 168, 168, 163, 171, 168),
  color_vals=c("#d294af", "#516959", "#d294af", "#516959", "#d294af", "#516959", "#d294af", "#516959"),
  thickness=c(0.5, 0.5, 0.75, 0.75, 0.5, 0.5, 0.75, 0.75))

p + geom_vline(mean_dates, aes(xintercept=yday, group=interaction(species, year),
                   color=species, size=size_code), stat="identity")

# figure this out later... only 2015 pink late (non-FR) and rest are early in 2015/2016.

#specifically, pi2015 = june 24, pi2016 = june 16, piave= 170.6/june 19,
# and cu2015 = june 9, cu2016 = june 11, cuave = 168.2/june 17

##### SIZES #####

fish_field_survey_data <- left_join(fish_field_data, seine_survey_data, by="seine_id")

fish_lab_survey_data <- left_join(fish_field_survey_data, fish_lab_data, by="ufn") %>%
  mutate(region=str_sub(seine_id, start=1, end=1))

fish_lab_survey_data$fork_length <- as.numeric(fish_lab_survey_data$fork_length)

fish_fl_lab <- filter(fish_lab_survey_data, is.na(fork_length)!=TRUE)

fish_fl_field_all <- filter(fish_lab_survey_data, is.na(fork_length_field)!=TRUE) %>%
  select(-fork_length)

fish_fl_field_relevant <- anti_join(fish_fl_field_all, fish_fl_lab) %>%
  select(fork_length=fork_length_field, everything())

fish_fl_all <- full_join(fish_fl_field_relevant, fish_fl_lab)

fish_fl_all %>%
  #mutate(fl=if_else(is.na(fork_length), fork_length_field, fork_length)) %>% 
  filter(species %in% c("PI", "CU")
  #       & is.na(fl)!=TRUE
  ) %>% 
  ggplot(aes(yday, fork_length, group=interaction(species, region, year)))+
  geom_bar(aes(fill=species), stat="identity", position="dodge")+
  scale_fill_manual(values=c("#516959", "#d294af"))+
  #facet_wrap(region~year, nrow = 2)
  facet_wrap(year~region, nrow=5)

fish_fl_all %>%
  filter(species %in% c("PI", "CU") & week<29) %>% 
  group_by(species, region, week) %>%
  summarise(ave_fl=mean(fork_length)) %>%
  ggplot(aes(week, ave_fl), group=interaction(species, region))+
  geom_bar(aes(fill=species), stat="identity", position="dodge")+
  scale_fill_manual(values=c("#516959", "#d294af"))+
  facet_wrap(~region, nrow=1)

##### ALL DATA #####

all_diet_copy <- all_salmon_data

# Merge rare (< 3 stom.) taxonomic groups to higher prey levels:

all_diet_data <- all_diet_copy %>%
  filter(prey_info!="Digested_food") %>% 
  group_by(ufn, fish_species, site_id, taxa_info, prey_info,
           kingdom, phylum, subphylum, class, subclass, order, suborder, infraorder, family, genus, species, life_stage) %>%
  summarise(biomass=sum(prey_weight_corr))
#create dataframe that sums any duplicates of prey groups in each stomach

all_diet_sum <- all_diet_data %>%
  ungroup() %>%
  group_by(kingdom, phylum, subphylum, class, subclass, order, suborder, infraorder, family, genus, species, life_stage,
           taxa_info, prey_info) %>%
  tally() %>%
  arrange(n)
#calculate how many stomachs each prey group appears in

# reassign any less than 3 stomach taxa

all_diet_groups <- all_diet_sum %>%
  #filter(taxa_info!="Detritus") %>% 
  mutate(taxa_new=if_else(n<3 & genus!="Neotrypaea", 
                  if_else(life_stage=="Object" | life_stage=="Detritus", "",
                  if_else(class=="Insecta" | class=="Arachnida" | class=="Actinopterygii", class,
                  if_else(genus=="Monstrilla" | order=="Mysida", subclass,
                  if_else(genus=="Candacia" | genus=="Oithona" | order=="Harpacticoida" | genus=="Primno", order,
                  if_else(genus=="Eualus" | family=="Paguridae", infraorder,
                  if_else(family=="Oncaeidae" | family=="Corophiidae" | family=="Pinnotheridae" | genus=="Nematoscelis", family,
                  if_else(family=="Caligidae", "Parasites",
                  if_else(species!="", genus,
                  taxa_info)))))))),
                  if_else(phylum=="Echinodermata", phylum,
                  if_else(family=="Calanidae" & life_stage=="Nauplii", order,
                  if_else(class=="Arachnida", class,
                  if_else(genus=="Neocalanus", genus,
                  if_else(phylum=="Nematoda" | class=="Trematoda", "Parasites",
                  taxa_info)))))),
         life_stage_new=if_else(str_detect(life_stage, "Zoea") | life_stage=="Megalopa" |
                                order=="Decapoda" & life_stage=="Juvenile", "Larvae", 
                        if_else(str_detect(life_stage, "Copepodite"), "Copepodite",
                        if_else(phylum=="Echinodermata", "Larvae",
                        if_else(prey_info=="Senticaudata_Juvenile" | prey_info=="Euphausiidae_Juvenile"| prey_info=="Calanoida_Egg" |
                                life_stage=="Pupae" | family=="Caligidae" | class=="Actinopterygii" & life_stage!="Egg" | order=="Isopoda" | taxa_info=="Eumalacostraca", "",
                                life_stage)))),
         prey_new=if_else(life_stage_new=="", taxa_new,
                  if_else(taxa_new=="", life_stage_new, 
                          paste(taxa_new, life_stage_new, sep="_")))) %>%
  ungroup %>% 
  select(prey_info, prey_new)
#update any prey groups that occur in less than three fish stomachs!

for (n in all_diet_groups$prey_info) {
  all_diet_copy$prey_info[which(all_diet_copy$prey_info %in% n)] <- all_diet_groups$prey_new[which(all_diet_groups$prey_info == n)]
}
#overwrite old data with new prey groups

all_diet_check <- all_diet_copy %>%
  group_by(ufn, fish_species, site_id, prey_info) %>%
  summarise(biomass=sum(prey_weight_corr))
#create dataframe that sums any duplicates of prey groups in each stomach

all_diet_filtered <- all_diet_check %>%
  ungroup() %>%
  group_by(prey_info) %>%
  tally() %>%
  arrange(n)
#calculate how many stomachs each prey group appears in (none <3!)
#reduced number of taxa from 176 to 112, a lot more manageable now!

# then wide data --> transform --> matrix --> NMDS / etc. (or clustering even... busy tho.)

all_diet_wide <- all_diet_copy %>%
  filter(food_weight_corr!=0 & !ufn %in% c("U2627", "U5285") & #not sure why that one is so erronous... hmm.
         !prey_info %in% c("Coscinodiscophycidae", "Digested_food", "Object", "Parasites", "Detritus")) %>% 
  select(ufn, site_id, survey_date, year, fish_species, prey_info, prey_weight_corr) %>%
  group_by(ufn, site_id, survey_date, year, fish_species, prey_info) %>%
  summarise(ww=sum(prey_weight_corr)) %>%
  spread(key=prey_info, value=ww, fill=0)

all_diet_info <- select(all_diet_wide, ufn:fish_species)

all_diet_matrix <- all_diet_wide %>%
  ungroup() %>% 
  select(Acartia:Tortanus_discaudatus) %>%
  decostand("total")

all_diet_trans <- asin(sqrt(all_diet_matrix))

rownames(all_diet_trans) <- all_diet_info$ufn

# NMDS calculation:

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc<- metaMDS(all_diet_trans,distance="bray",labels=all_diet_info$site_id, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)

NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=all_diet_info$site_id)
#plot NMDS, only once (picking Bray because all similar), no presence absence

ord.bc<-ordiellipse(eco.nmds.bc,all_diet_info$site_id,display="sites",kind="sd", conf = 0.95, label=T)
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
  geom_point(stat = "identity", aes(shape=all_diet_info$fish_species, color=all_diet_info$site_id), fill="white", size=2, stroke = 1)+
  scale_color_manual(values=c("#053061", "#1F78B4", "lightseagreen", 
                              "#F781BF", "#e41a1c", "darkred", "black"), name="Site",
                     guide = guide_legend(reverse = TRUE)) +
  new_scale_color()+
  #geom_path(data=df_ell.bc, aes(x=NMDS1, y=NMDS2,colour=group), size=1, linetype=2) +
  scale_shape_manual(values=c(21, 19), name="Species")+
  guides(fill= guide_legend(override.aes = list(shape=21)))+
  labs(x="NMDS 1", y="NMDS 2")+
  scale_colour_manual(values=c("darkred", "#053061"), name="Region") +
  theme_bw()+
  theme(axis.text.x=element_text(size=10),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(angle=90,size=12),
        axis.text.y=element_text(size=10),
        panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
        axis.ticks = element_blank()) + coord_fixed() +
  annotate("text",x=1.35,y=-1.6,label="(stress = 0.17)",size=4, hjust = 0)
#NMDS graph for the different sites!

a

ggsave("full_data_NMDS.png", width=15, height=13, units = "cm", dpi=800)
# nmds comes out slightly differently everytime unlike other graphs. save once then forget it!

# next: update color/shape properly (find new color for J07... diff light blue??) then CLUSTER!

