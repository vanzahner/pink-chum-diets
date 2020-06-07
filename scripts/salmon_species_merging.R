# Merging pink and chum diet dataset with sockeye diet dataset!

library(tidyverse)

sockeye_diets <- read_csv(url("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/sample_results/sockeye_diets.csv"))
#read in sockeye diet data

pink_chum_diets <- read_csv(url("https://raw.githubusercontent.com/vfladmark/pink-chum-diets/master/data/pink_chum_diets_raw_data.csv"))
#read in pink and chum diet data

colnames(sockeye_diets) <- gsub("[.]", "_", colnames(sockeye_diets))
#replace periods in column names with underscores for merging

sockeye_filtered <- select(sockeye_diets, -c(sample_int, dry_content_w,
                                             fullness_est, plot_taxon_g,
                                             plot_taxon_d, plot_taxon,
                                             length_corr))
#delete columns that aren't needed for merging

sockeye_rename <- rename(sockeye_filtered, stom_proc_date=sam_proc_date,
                         food_weight_raw=wet_content_w,
                         food_weight_corr=corrected_ww,
                         species=species_1,
                         digestion_state=DI,
                         size_class=size,
                         length_ave=length_avg,
                         prey_weight_raw=group_weight,
                         prey_weight_corr=corrected_weight)
#rename other columns that need to be merged

sockeye_data <- mutate(sockeye_rename, processor="SJ", fish_species="Sockeye")
#add columns that specify these data are sockeye and processed by Sam

pink_chum_data <- select(pink_chum_diets, -c(microscope_hours,
                                             stom_weight_full,
                                             stom_weight_empty)) %>%
  mutate(processor="VF")
#delete useless columns that aren't needed, add processor info

pink_chum_sockeye_diets <- left_join(pink_chum_data, sockeye_data)
#final raw diet data with the three species of salmon merged together! :)

