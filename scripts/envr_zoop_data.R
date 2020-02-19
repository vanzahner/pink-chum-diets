#ENV AND ZOOP DATA#

rm(list=ls())
#remove other R stuff

library(readr)
#read in files
library(tidyverse)
#data wrangling

setwd("/Users/Vanessa/Desktop/msc_project")
#set working directory

ysi_data <- read_csv("data/ysi.csv")

ysi_filtered <- ysi_data %>%
  filter(site_id %in% c("D07", "J07", "D09", "D11", 
                        "J02", "J06", "J08") & 
           survey_date>"2015-05-20"&
           survey_date<"2016-07-06")
#need to filter out to only what's relevant to samples (then secchi next)

zoop_data <- read_csv("data/zoop_comp_data_combined.csv")
#need to resolve issues about missing data (JSPK 1154, which is for July 5, 2016 J07)

#zoop_data_ww <- read_csv("data/zoop_data_ww.csv")
#J02 (JSPK1118) has taxa data but no wet weight. ignore all ww since J02 most important

