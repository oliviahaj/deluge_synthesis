#### Precip Cleaning
## OLH 
## Feb. 2, 2025

# still need to discuss some of this with Dave

## Load libraries
library(tidyverse)
library(googledrive)
library(broom)
library(lubridate)

##############################################################
##############################################################
## 1) Download the data
##############################################################
##############################################################
dir.create(file.path("deluge"), showWarnings = F)
dir.create(file.path("deluge", 'data'), showWarnings = F)
dir.create(file.path("deluge", 'precip_data'), showWarnings = F)

# Download data
## Precip
ppt <-'cper_ppt_data_combined.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/18toIfC8jV7s8ZzAD0SV9CWL4SvGgn-RO")) %>% 
  dplyr::filter(name == ppt) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("deluge", 'precip_data', .$name))

###ANPP
trt <-"treatment_table_copy.csv"

googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1-z0EMfF8_LTWr3HZVajzcO3K9jKgYM9p")) %>% 
  dplyr::filter(name == trt) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("deluge", 'data', .$name))

# Read in the data
cper_ppt <- read.csv("deluge/precip_data/cper_ppt_data_combined.csv")
trt_table <- read.csv("deluge/data/treatment_table_copy.csv")

## work to merge
str(cper_ppt)
str(trt_table)

# convert cper_ppt to wide format
cper_ppt_wide <- cper_ppt %>%
  pivot_wider(names_from = Pasture, values_from = c(gs_ppt, ann_ppt)) %>%
  select(c(year, gs_ppt_hm.25SE, ann_ppt_hm.25SE, gs_ppt_hm.CN, ann_ppt_hm.CN, 
           gs_ppt_hq.catch, ann_ppt_hq.catch, gs_ppt_HQ, ann_ppt_HQ,
           gs_ppt_HQ_yr_round, ann_ppt_HQ_yr_round,gs_ppt_25C, ann_ppt_25C))


trt_table.2 <-   

