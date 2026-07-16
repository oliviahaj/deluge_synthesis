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
# write.csv(ppt_sum, "cper_ppt_data_combined.csv", row.names = FALSE)
# 
# drive_upload(
#   media = "cper_ppt_data_combined.csv",
#   path = file.path("deluge", "precip_data"),
#   name = "cper_ppt_data_combined.csv"
# )


# Prep data for monthly precip values
# Convert to long
str(full_ppt2)
ppt_long <- full_ppt2 %>%
  pivot_longer(m1:m12, names_to = "month", values_to ="precip.mm") %>%
  mutate(month = as.numeric(str_remove(month, "m")))

## some quick visualizations
ggplot(ppt_long, aes(month, precip.mm, color=Pasture))+
  geom_point()+
  geom_line()+
  theme_bw() + 
  facet_wrap(~year)+
  theme(legend.position = "none")

# select the pastures
unique(ppt_long$Pasture)
pastures <- c("hm.15E", "hm.25SE", "hm.25NW", "25C", "31W")

ppt.select <- ppt_long %>%
  filter(Pasture %in% pastures) 

ppt.select.wide <- full_ppt2 %>%
  filter(Pasture %in% pastures) %>%
  arrange(year) %>%
  filter(Pasture != "hm.25NW" | year == 2024) %>%
  filter(Pasture != "31W" | year == 2018) %>%
  mutate(location = ifelse(Pasture == "hm.15E", "CHANGE", "DEX"))

ggplot(ppt.select, aes(month, precip.mm, color=Pasture))+
  geom_point()+
  geom_line()+
  theme_bw() + 
  facet_wrap(~year)+
  theme(legend.position = "none")

# read in the selected treatments file
trts <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1fpoG0wLLqjFLkucR2ZlNMon31DIW1GH-")) %>%
  dplyr::filter(name == "selected_treatments")

# Did that work?
trts

# Download the excluded treatmetn file
googledrive::drive_download(file = trts$id, overwrite = T, type = "csv",
                            path = file.path("deluge", trts$name))

# Read in excluded treatment file
trt <- read.csv(file = file.path("deluge", "selected_treatments.csv"))


ppt.select.wide_lag <- ppt.select.wide %>%
  mutate(year = year + 1) %>%        # shift forward so previous year aligns
  rename_with(~ paste0(.x, "_prev"), -c(year, location)) %>%
  select(-Pasture_prev)
## Precip with lag
ppt.wide <- left_join(ppt.select.wide, ppt.select.wide_lag, by = c("year", "location"))
ppt.wide2 <- ppt.wide %>%
  select(-Pasture)


# Select the treatments and their precip year only
trt_years <- trt %>%
  select(Study, Treatment_raw, response_year) %>%
  rename(year = "response_year") %>% 
  filter(Study != "Siggers et al. ") %>%
  mutate(location = ifelse(Study == "Linbabury et al. CHANGE", "CHANGE", "DEX"), 
         year = ifelse(Treatment_raw == "SEP", 2022, year)) %>%
  left_join(ppt.wide2) 

deluge_trt <- trt %>%
  select(Study, Treatment_raw, response_year, Deluge_size_mm, Deluge_month) %>%
  rename(year = "response_year") %>% 
  # fix mary's mmissing month
  mutate(Deluge_month = ifelse(Study == "Linbabury et al. CHANGE" & Treatment_raw == "Del" & year == 2022, "6", Deluge_month))%>%
  mutate(Deluge_month = paste0("m", as.character(Deluge_month), by = "")) %>%
  filter(Deluge_month != "mNA") %>%
  mutate(month = ifelse(Treatment_raw == "SEP", "m9_prev", Deluge_month), 
         month = ifelse(Study == "Tooley et al. DRE", "m9_prev", month)) %>%
  select(-Deluge_month)

str(trt_years)
ppt.long <- trt_years %>%
  pivot_longer(
    cols = m1:ann_ppt_prev,
    names_to = "month",
    values_to = "precip",
    values_drop_na = F
  ) %>%
  left_join(deluge_trt)%>%
  mutate(Deluge_size_mm = ifelse(is.na(Deluge_size_mm), 0, Deluge_size_mm), 
         precip.mm = precip + Deluge_size_mm) %>%
  # fix alison's so that the treatments subtract 8 mm - it is minus 8 in july 2018 for 2021 study
  mutate(precip.mm = ifelse(Study == "Post and Knapp 2021" & year == 2018 & month == "m7", precip.mm-8, precip.mm)) %>%
  # Fix Dave's study names
  mutate(Treatment_raw = case_when(
    Treatment_raw == "DRT (3)" ~ "DRT", 
    Treatment_raw == "DRT+DEL (4)" ~ "DRT+DEL", 
    TRUE ~ Treatment_raw))

# update Hoover et al. 2022 precip
hoover.ppt <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1-z0EMfF8_LTWr3HZVajzcO3K9jKgYM9p")) %>%
  dplyr::filter(name == "Hoover2022_CPER-DEX_Precipitation.csv")

# Did that work?
hoover.ppt 

# Download the excluded treatmetn file
googledrive::drive_download(file = hoover.ppt $id, overwrite = T, type = "csv",
                            path = file.path("deluge","data", hoover.ppt $name))

# Read in hoover ppt
hoover.ppt  <- read.csv(file = file.path("deluge","data", "Hoover2022_CPER-DEX_Precipitation.csv"))
str(hoover.ppt)

hppt <- hoover.ppt %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"), month=month(date), month = paste("m", as.character(month), sep=""), year = year(date)) %>%
  group_by(treatment, month, year) %>%
  summarize(precip.mm2 = sum(precipitation)) %>%
  rename(Treatment_raw = "treatment") %>%
  mutate(precip.mm2 = ifelse(month == "m5", precip.mm2 + 37, precip.mm2)) 

ppt.long2 <- left_join(ppt.long, hppt)
ppt.long3 <- ppt.long2 %>%
  mutate(ppt.mm = coalesce(precip.mm2, precip.mm))
  
# add in Alex's data
alex.ppt <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/18toIfC8jV7s8ZzAD0SV9CWL4SvGgn-RO")) %>%
  dplyr::filter(name == "CCE_PPT_Clean_wMeta.xlsx")

# Did that work?
alex.ppt 

# Download the excluded treatmetn file
googledrive::drive_download(file = alex.ppt $id, overwrite = T, type = "xlsx",
                            path = file.path("deluge","precip_data", alex.ppt$name))

library(readxl)
alex.ppt  <- read_excel("/Users/olhajek/Desktop/deluge_synthesis/Deluge_synthesis/deluge/precip_data/CCE_PPT_Clean_wMeta.xlsx", sheet = "CCE_PPT_Clean")
str(alex.ppt)

alex <- alex.ppt %>%
  select(c("date","ppt.ctrl", "ppt.del", "ppt.drt", "ppt.drtdel"))%>%
  mutate(month = month(date), year = year(date), year = ifelse(month > 9, year + 1, year), month = paste("m", as.character(month),sep ='')) %>%
  filter(year != 2023) %>%
  pivot_longer(cols = starts_with("ppt."), 
               names_to="Treatment_raw", 
               values_to="precip.mm2") %>%
  group_by(month, year, Treatment_raw) %>%
  summarize(ppt.mm = sum(precip.mm2)) %>%
  mutate(month = ifelse(year == 2024, paste(month, "prev",sep="_"), month), 
         Study = "Siggers et al.", 
         year = 2025)

## Join
str(ppt.long3)

ppt.join <- ppt.long3 %>%
  select(c(Study, Treatment_raw, year, month, ppt.mm)) %>%
  rbind(alex) %>%
  pivot_wider(values_from = ppt.mm, names_from = month) %>%
  # update growing and ann ppt
  mutate(gs_ppt = m5 + m6 + m7 + m8,
         ann_ppt = m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12,
         gs_ppt_prev = m5_prev + m6_prev + m7_prev + m8_prev,
         ann_ppt_prev = m1_prev + m2_prev + m3_prev + m4_prev + 
           m5_prev + m6_prev + m7_prev + m8_prev + m9_prev + m10_prev + m11_prev + m12_prev,)

# TREATMENT NAME UPDaTE
names <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1fpoG0wLLqjFLkucR2ZlNMon31DIW1GH-")) %>%
  dplyr::filter(name == "treatment_codes_byData")

# Did that work?
names 

# Download the excluded treatmetn file
googledrive::drive_download(file = names$id, overwrite = T, type = "csv",
                            path = file.path("deluge", names$name))

# Read in hoover ppt
names.update <- read.csv(file = file.path("deluge","treatment_codes_byData.csv"))


# 1) fix/update treatment names on the precip data
names.update  <- names.update %>%
  filter(raw_filename == "precip_data_cleaned_trts.csv") %>%
  rename(Study = "Full.Study", 
         Treatment_raw = "trt_names") %>%
  select(c(Study, Study_shorthand, Our_trt, Treatment_raw)) %>%
  mutate(across(where(is.character), ~gsub("\u00A0", " ", .x)))

ppt.2 <- ppt.join %>%
  mutate(across(c(Study, Treatment_raw), str_trim)) %>%
  left_join(
    names.update, #%>% mutate(across(c(Study, Treatment_raw), str_trim)),
    by = c("Study", "Treatment_raw")
  )                         
                         
              
# Fix Greg's Precipitation Data
# Get rid of Greg's ppt in the last file and replace wiht the new data
greg <- read.csv(file = file.path("deluge", "precip_data", "greg_ppt_wide.csv")) %>%
  mutate(Study_shorthand = "Tooley_DRE", 
         Our_trt = case_when(
           Treatment_raw == "Leg.Drought_Trt.Deluge" ~ "DR_DEL", 
           Treatment_raw == "Leg.Drought_Trt.LTA" ~ "DR", 
           Treatment_raw == "Leg.Control_Trt.LTA" ~ "CON", 
           TRUE ~ "CON_DEL"
         ))

ppt.3 <- ppt.2 %>%
  filter(Study != "Tooley et al. DRE") %>%
  rbind(greg)
           
# save and export

write.csv(ppt.3, "precip_data_cleaned_trts.csv", row.names = FALSE)

drive_upload(
  media = "precip_data_cleaned_trts.csv",
  path = file.path("deluge", "precip_data"),
  name = "precip_data_cleaned_trts.csv"
)




