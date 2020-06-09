# How to calculate most detailed level of taxa and life stage of prey/zoops

# June 2020 code from Vanessa as a present to all of my fellow zoop homies

##### set up: #####

library(tidyverse)
library(here)

diet_data <- read.csv(here("data", "diet_data_example.csv"), stringsAsFactors = FALSE)
# update for wherever your file is stored (example data = 1 pink, 1 chum)
# have to do read.csv to have stringsAsFactors=FALSE (or else there's NAs)

# data is be a long format data file with columns for taxa information

# e.g. phylum, class, order, family, genus, species, life stage as columns

# this code combines genus and species if given, and life stage if given

# if there's adult life stages, you should leave it blank as a default

# if you have "digested food" or objects or etc, don't put in species column

##### taxa calculation: #####

# code chunk that checks each column backwards until it finds a non-blank:

updated_diet_data <- diet_data %>%
  mutate(taxa_info=(if_else(species=="",
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
                   paste(genus, species, sep="_"))),
         prey_info = (ifelse(life_stage=="", taxa_info,
                             paste(taxa_info, life_stage, sep="_"))))

# save your data with new columns into a new dataframe and off you go!

# if you don't have any columns, just carefully delete parts like so:
# (if_else(suborder==="",
# suborder)),
# or add accordingly if you add any other levels of taxonomic details
# the order definitely matters here, the if_else statements are nested

##### merging prey groups: #####

# How to merge taxa into broader prey/zoop groups very easily as well:

simpler_data <- updated_diet_data %>%
  mutate(prey_group=if_else(order=="Amphipoda", "Amphipods",
                    if_else(suborder=="Balanomorpha", "Barnacles",
                    if_else(order=="Calanoida", "Calanoids",
                    if_else(family=="Podonidae", "Cladocerans",
                    if_else(order=="Decapoda", "Decapods",
                    if_else(class=="Actinopterygii", "Fish",
                    if_else(family=="Euphausiidae" & life_stage=="Egg", "Euph_eggs",
                    if_else(family=="Euphausiidae" & life_stage!="Egg", "Euphausiids",
                    if_else(class=="Insecta" | class=="Arachnida", "Insects",
                    if_else(order=="Harpacticoida", "Harpacticoids",
                    #if_else(phylum=="Cnidaria" | phylum=="Ctenophora", "Gelatinous",
                    if_else(genus=="Oikopleura", "Larvaceans",
                    if_else(class=="Sagittoidea", "Chaetognaths",
                            "Other")))))))))))))#)

# order does not matter here and is a separate process from previous code

# class=="Insecta" | class=="Arachnida", means insects OR arachnids = same

# taxa levels don't matter, this is very flexible code for whatever ya need

# I left a hashed out example, you can easily delete or add lines this way

# using `& life_stage=="whatever"` you can separate larvae from adults, etc

# code creates a new column, not overwriting old data, so it keeps both info

# and that way you can see what makes up the "other" category more easily!

