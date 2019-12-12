# Spatial analysis code for MSc thesis on juvenile pink and chum salmon diets #

rm(list=ls())
#remove other R stuff

library(readr)
#read in files
library(ggplot2)
#graphs

setwd("/Users/Vanessa/Desktop/Nov desktop/R Projects/msc_project")
#set working directory

spat_data <- read_csv("processed/spatial_pink_chum_diets.csv")

