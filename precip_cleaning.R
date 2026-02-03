## CPER Precipitation File Creation
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
## 1) Download all of the data
##############################################################
##############################################################

# create folders
dir.create(file.path("deluge"), showWarnings = F)
dir.create(file.path("deluge", 'precip_data'), showWarnings = F)

# Identify wanted files
files_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/18toIfC8jV7s8ZzAD0SV9CWL4SvGgn-RO")) %>% 
  dplyr::filter(stringr::str_detect(string = .$name, pattern = "\\.csv"))

# Did that work?
files_drive

# Identify local files
files_local <- dir(path = file.path("deluge", "precip_data"))
files_local

# Overwrite local data files?
update <- TRUE

# Identify desired files
if(update == T) {
  files_wanted <- files_drive 
} else {
  files_wanted <- files_drive %>%
    dplyr::filter(!name %in% files_local)
}

# Download them!
purrr::walk2(.x = files_wanted$id, .y = files_wanted$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("deluge", "precip_data", .y)))

##############################################################
##############################################################
## 2) Read in data and update the format
##############################################################
##############################################################

## HQ Catch - will be the winter data
hq <- read.csv("deluge/precip_data/HQ_catchcan_1939_2021.csv")
str(hq)

# Clean the data - going to get monthly summaries
hq.2 <- hq %>%
  # if NA replace with 0 
  mutate(ppt_mm = replace_na(ppt_mm, 0)) %>%
  # Calculate the monthly precip
  group_by(year, month) %>%
  summarize(mo_ppt_mm = sum(ppt_mm)) %>%
  mutate(Pasture = "HQ_yr_round") %>%
  # Fix for water year
  mutate(year = ifelse(month > 9, year + 1, year)) %>%
  filter(year > 2013) %>%
  filter(year < 2019) %>%
  pivot_wider(
    names_from = month,
    values_from = mo_ppt_mm,
    names_prefix = "m"
  )

hq.winter <- hq.2 %>%
  select(c(year, m1, m2, m3, m4, m10, m11, m12))

## Pasture data
pasture <- read.csv("deluge/precip_data/CPERpasturePPT1939_2021.csv")
str(pasture)

pasture.2 <- pasture %>%
  # get rid of 2019 - 2021 as this is in the cleaned data alread
  filter(Year < 2019) %>%
  # replace the very few #VALUE! with the min recorded ppt on that dat
  mutate(mm = as.numeric(ifelse(mm == "#VALUE!", "25.654", mm)))%>%
  # get rid of october ppt
  # fix the month
  mutate(Month = as.numeric(case_when(
    Month == "June" ~ "6", 
    Month == "May" ~ "5", 
    Month == "July" ~ "7", 
    Month == "August" ~ "8",
    Month == "September" ~ "9", 
    TRUE ~ Month
  )))%>%
  filter(Month != 10) %>%
  # calculate monthly precip
  group_by(Pasture, Year, Month) %>%
  summarize(mo_ppt_mm = sum(mm)) %>%
  # rename to be able to join with the winter precip
  rename(year = "Year", month = "Month") %>%
  # just get this century 
  filter(year > 2013) %>%
  filter(year < 2019)%>%
    pivot_wider(
      names_from = month,
      values_from = mo_ppt_mm,
      names_prefix = "m"
    )
  

# full join with the winter months
ppt_pre_2017 <- full_join(pasture.2, hq.winter)

# add teh hq catch can row too in case
ppt_pre_2017 <- rbind(hq.2, ppt_pre_2017)

# compare the hq 
# it seems like this one is consistently lower
hq.comp <- pasture.2 %>%
  filter(Pasture == "HQ")

## cleaned 2019 - 2021 data - water year
ppt_clean <- read.csv("deluge/precip_data/cper_ppt_gapfilled_2019-2022_v1.0.csv")
str(ppt_clean)

ppt_clean.2 <- ppt_clean %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"), 
         year = year(date), 
         month=month(date))%>%
  # summarize by month
  group_by(site, month, year) %>%
  summarize(mo_ppt_mm = sum(ppt.gapfilled.mm))%>%
  # rename
  rename(Pasture = "site") %>%
  pivot_wider(
    names_from = month,
    values_from = mo_ppt_mm,
    names_prefix = "m"
  )

# hq comparison
ppt_clean_hq <- ppt_clean.2 %>%
  filter(Pasture == "hq.catch")

## combine wiht the other data
full_ppt <- rbind(ppt_pre_2017, ppt_clean.2)
full_ppt$gs_ppt <- full_ppt$m5 + full_ppt$m6 + full_ppt$m7 + full_ppt$m8
full_ppt$ann_ppt <- full_ppt$m1 + full_ppt$m2 + full_ppt$m3 + full_ppt$m4 +
  full_ppt$m5 + full_ppt$m6 + full_ppt$m7 + full_ppt$m8 +
  full_ppt$m9 + full_ppt$m10 + full_ppt$m11 + full_ppt$m12
