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
#need to filter out to only what's relevant to samples (then secchi and zoops next)

zoop_data_taxa <- read_csv("data/zoop_data_taxa.csv")

zoop_data_ww <- read_csv("data/zoop_data_ww.csv")

#need to resolve issues about missing data/duplicates. JSPK1093 and see notes for others.
#and hippotylidae zoea blank issue too. AND size class saving as date... ERGH!
#and add location (DI or JS); biomass; volume corrected; etc. to my version of zoop data