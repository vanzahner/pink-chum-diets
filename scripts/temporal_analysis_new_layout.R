#updated temporal analysis code:

#last modified june 22 2020

#purpose is all temporal data + analysis (diets, zoops, and environment)

##### LIBRARIES #####

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
library(here)
#project oriented workflow
library(kableExtra)
library(knitr)
library(formattable)
#for creating nice tables

##### ENVR + ZOOP DATA #####

# Read in environmental data file:

temp_envr_data_raw <- read.csv(here("processed", "temporal_data", "temporal_survey_ysi.csv"), stringsAsFactors = FALSE)
#read in temporal environmental data

# modify (mutate) diff data sets to include date categories (+site/year/sp?)



##### ENVR + ZOOP GRAPHS #####



##### ENVR + ZOOP TABLES #####



##### SALMON DATA PREP #####

