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
                             paste(taxa_info, life_stage, sep="_")))
# save your data with new columns into a new dataframe and off you go!

# if you don't have any columns, just carefully delete parts like so:
# if_else(suborder==="",
# suborder),
# or add accordingly if you add any other levels of taxonomic details
# the order definitely matters here, the if_else statements are nested

##### merging prey groups: #####

# How to merge taxa into broader prey/zoop groups very easily as well:

simpler_data <- updated_diet_data %>%
  mutate(prey_group=if_else(class=="Sagittoidea" | phylum=="Mollusca" | phylum=="Echinodermata" | phylum=="Ochrophyta", phylum,
                    if_else(genus=="Oikopleura" | class=="Actinopterygii" | class=="Polychaeta", class,
                    if_else(order=="Calanoida" | order=="Decapoda"  |
                            order=="Amphipoda" | order=="Cumacea" | order=="Isopoda" |
                            order=="Harpacticoida" | order=="Cyclopoida", order,
                    if_else(suborder=="Balanomorpha", suborder,
                    if_else(family=="Euphausiidae" | family=="Podonidae", family,
                    if_else(class=="Insecta" | class=="Arachnida", "Insecta_Arachnida",
                    if_else(phylum=="Cnidaria" | phylum=="Ctenophora", "Cnidaria_Ctenophora",
                    if_else(prey_info=="Copepoda", "Crustacea",
                            prey_info)))))))))
# order does not matter here and is a separate process from previous code

# taxa levels don't matter, this is very flexible code for whatever ya need

# you can also modify and/or group by life_stage with similar code to this

other_data <- simpler_data %>%
  mutate(prey_group_simple=if_else(prey_group!="Calanoida" & prey_group!="Decapoda" & prey_group!="Euphausiidae" & prey_group!="Amphipoda" & prey_group!="Harpacticoida" & 
                                   prey_group!="Insecta_Arachnida" & prey_group!="Cnidaria_Ctenophora" & prey_group!="Appendicularia" & prey_group!="Chaetognatha", "Other", prey_group))
# keep prey groups that are substantial, rest = "Other" prey/zoop category

# code creates a new column, not overwriting old data, so it keeps both info

# and that way you can see what makes up the "other" category more easily!

# e-mail vfladmark@eoas.ubc.ca if you need any help with how to use this :)
