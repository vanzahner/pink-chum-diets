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