# ANPP - PPT W/ Long-Term Data
# goal of this is to produce a graph that has all of the experimental ANPP - PPT data 
# alongisde the historical precipitation and ANPP data. 
# using daily data to determine largest precipitation event in the growing season and will color points accordingly
# need to maintain the max ppt and the month

## load libraries
library(tidyverse)
library(googledrive)
library(lubridate)

# Create desktop folders
dir.create(file.path("deluge"), showWarnings = F)
dir.create(file.path("deluge", 'data'), showWarnings = F)
dir.create(file.path("deluge", 'precip_data'), showWarnings = F)
dir.create(file.path("deluge", 'data', 'R_exports'), showWarnings = F)

# Read in the historical precipitation and calculate deluge size
ppt <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/18toIfC8jV7s8ZzAD0SV9CWL4SvGgn-RO")) %>%
  dplyr::filter(name == "CPER_LTNPP_CoMoMeteorology.csv")


# Download the precipitation file
googledrive::drive_download(file = ppt$id, overwrite = T, type = "csv",
                            path = file.path("deluge","precip_data", ppt$name))

# Read in precipitation file
ppt <- read.csv(file = file.path("deluge","precip_data", "CPER_LTNPP_CoMoMeteorology.csv")) %>%
  select(Date,Precip.mm.d) %>%
  unique() %>%
  mutate(Date = as_date(mdy_hms(Date)), precip = ifelse(is.na(Precip.mm.d), 0, Precip.mm.d)) %>%
  mutate(rel_events = case_when(
    precip <= 2 ~ 0,
    precip > 2 ~ precip))

# Calculate annual precipitation AND max rain event and month
library(data.table)
ppt.del <- ppt %>%
  mutate(group=rleid(rel_events >0))%>%
  group_by(group)%>%
  summarise(n.days=n(),
            start_date=min(Date),
            end_date=max(Date),
            total_precip=sum(rel_events))%>%
  filter(total_precip>0)%>%
  #filter(total_precip>20) %>%
  mutate(month = month(end_date), year = year(end_date))

map <- ppt %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarize(ann.ppt = sum(precip)) 

del <- ppt.del %>%
  ungroup() %>%
  group_by(year) %>%
  slice_max(total_precip, n = 1)

ppt.sum <- left_join(map, del)

mean(ppt.sum$total_precip)
median(ppt.sum$total_precip)

# Visualzie
ggplot(ppt.sum, aes(ann.ppt, total_precip, color = year))+
  geom_point()+
  theme_bw()


######################################################
##########ANPP########################################
######################################################
# read in long-term ANPP
anpp <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1-z0EMfF8_LTWr3HZVajzcO3K9jKgYM9p")) %>%
  dplyr::filter(name == "LTAR_cper_pre-process.csv")

# Did that work?
anpp 

# Download the excluded treatmetn file
googledrive::drive_download(file = anpp$id, overwrite = T, type = "csv",
                            path = file.path("deluge","data", anpp$name))

anpp <- read.csv(file = file.path("deluge","data", "LTAR_cper_pre-process.csv")) %>%
  mutate(Date = as_date(mdy_hms(Date)), year = year(Date)) %>%
  group_by(year, Treatment.ID) %>%
  summarize(anppkg = mean(biomass_kg_ha)) %>%
  ungroup()%>%
  group_by(year) %>%
  summarize(anpp = mean(anppkg)/10)


######################################################
##### Historial ANPP and PPT #########################
######################################################
anpp.ppt <- left_join(ppt.sum, anpp)

ggplot(anpp.ppt, aes(ann.ppt, anpp, color = total_precip))+
  geom_point()+
  scale_color_viridis_c(option = "mako", direction = -1, end = 0.9,
                        limits = c(NA, quantile(anpp.ppt$total_precip, 0.95, na.rm = TRUE)),
                        oob = scales::squish) +
  theme_bw()

