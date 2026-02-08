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
  mutate(year = ifelse(month > 9, year + 1, year)) %>%
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

## Most recent cleaned data
ppt_clean23 <- read.csv("deluge/precip_data/cper_ppt_gapfilled_2023-2025_v1.0.csv")
str(ppt_clean23)

ppt_clean.25 <- ppt_clean23 %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"), 
         year = year(date), 
         month=month(date))%>%
  mutate(year = ifelse(month > 9, year + 1, year)) %>%
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

## Merged the cleaned up data if there is overlap
cleaned_combo <- full_join(ppt_clean.2, ppt_clean.25, by = c("Pasture", "year"))

# make it so just one column for eahc month
cleaned_combo2 <- cleaned_combo%>%
  mutate(
    m1 = coalesce(m1.x, m1.y),
    m2 = coalesce(m2.x, m2.y),
    m3 = coalesce(m3.x, m3.y),
    m4 = coalesce(m4.x, m4.y),
    m5 = coalesce(m5.x, m5.y),
    m6 = coalesce(m6.x, m6.y),
    m7 = coalesce(m7.x, m7.y),
    m8 = coalesce(m8.x, m8.y),
    m9 = coalesce(m9.x, m9.y),
    m10 = coalesce(m10.x, m10.y),
    m11 = coalesce(m11.x, m11.y),
    m12 = coalesce(m12.x, m12.y)
  ) %>%
  select(Pasture, year, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12)

## combine wiht the other data - another full join to merge data where relevant, will priortize the cleaned dataset
## in this case though...
full_ppt <- full_join(ppt_pre_2017, cleaned_combo2, by = c("Pasture", "year"))

# make it so just one column for eahc month
full_ppt2 <- full_ppt%>%
  mutate(
    m1 = coalesce(m1.y, m1.x),
    m2 = coalesce(m2.y, m2.x),
    m3 = coalesce(m3.y, m3.x),
    m4 = coalesce(m4.y, m4.x),
    m5 = coalesce(m5.y, m5.x),
    m6 = coalesce(m6.y, m6.x),
    m7 = coalesce(m7.y, m7.x),
    m8 = coalesce(m8.y, m8.x),
    m9 = coalesce(m9.y, m9.x),
    m10 = coalesce(m10.y, m10.x),
    m11 = coalesce(m11.y, m11.x),
    m12 = coalesce(m12.y, m12.x)
  ) %>%
  select(Pasture, year, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12)

## Data quality check
# really it is just September 2015 that is missing in nearly every data
# goign to replace with the HQ catch can
library(naniar)
# Basic missing data visualization
gg_miss_var(full_ppt2)

full_ppt2$m9 <- ifelse(is.na(full_ppt2$m9), 1.016, full_ppt2$m9)

## Calculate Growing season and annual precipitation
## remove monthly
full_ppt2$gs_ppt <- full_ppt2$m5 + full_ppt2$m6 + full_ppt2$m7 + full_ppt2$m8
full_ppt2$ann_ppt <- full_ppt2$m1 + full_ppt2$m2 + full_ppt2$m3 + full_ppt2$m4 +
  full_ppt2$m5 + full_ppt2$m6 + full_ppt2$m7 + full_ppt2$m8 +
  full_ppt2$m9 + full_ppt2$m10 + full_ppt2$m11 + full_ppt2$m12

ppt_sum <- full_ppt2 %>%
  select(c("Pasture", "year", "gs_ppt", "ann_ppt"))

## Export and write to the drives
# Save your dataframe locally first
write.csv(ppt_sum, "cper_ppt_data_combined.csv", row.names = FALSE)
drive_upload(
  media = "cper_ppt_data_combined.csv",
  path = file.path("deluge", "precip_data"),
  name = "cper_ppt_data_combined.csv"
)
