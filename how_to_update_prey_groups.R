# How to calculate the most detailed level of taxa and life stage of prey

# diet data is be a long format data file with columns for taxa information

# e.g. phylum, class, order, family, genus, species, life stage as columns

# this code combines genus and species if given and life stage if given

# if there's adult life stages, you can leave it blank

# if you have "digested food" or objects or etc, don't put in species column

updated_diet_data <- diet_data %>%
  mutate(taxa_info=(if_else(species=="",
                   (if_else(genus=="",
                   (if_else(family=="",
                   (if_else(infraorder=="",
                   (if_else(suborder=="",
                   (if_else(order=="",
                   (if_else(subclass=="",
                   (if_else(class=="",
                   (if_else(subphylum=="",
                   (if_else(phylum=="",
                   kingdom, phylum)), subphylum)), class)), subclass)),
                   order)), suborder)), infraorder)), family)), genus)),
                   paste(genus, species, sep="_"))),
         prey_info = (ifelse(life_stage=="", taxa_info,
                             paste(taxa_info, life_stage, sep="_"))))

# save your data with new columns into a new dataframe and off you go!

# if you don't have any columns, just carefully delete parts like so:
# (if_else(suborder==="",
# suborder)),
# or add accordingly if you add any other levels of taxonomic details

# HOW TO MERGE INTO PREY GROUPS VERY EASILY TOO :)
simpler_data <- updated_diet_data %>%
  mutate(prey_group=if_else(order=="Cyclopoida", "Cyclopoids",
                    if_else(order=="Calanoida", "Calanoids",
                    if_else(order=="Decapoda", "Decapods",
                    if_else(family=="Euphausiidae", "Euphausiids",
                    if_else(class=="Insecta" | class=="Arachnida", "Insects",
                    if_else(order=="Harpacticoida", "Harpacticoids",
                    if_else(phylum=="Cnidaria" | phylum=="Ctenophora", "Gelatinous",
                    if_else(genus=="Oikopleura", "Larvaceans",
                    if_else(class=="Sagittoidea", "Chaetognaths", "Other"))))))))))

