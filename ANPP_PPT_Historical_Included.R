# ANPP - PPT W/ Long-Term Data
# goal of this is to produce a graph that has all of the experimental ANPP - PPT data 
# alongisde the historical precipitation and ANPP data. 
# using daily data to determine largest precipitation event in the growing season and will color points accordingly
# need to maintain the max ppt and the month

## load libraries
library(tidyverse)
library(googledrive)
library(lubridate)

`%!in%` = Negate(`%in%`)

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


######################################################
##### Experimental ANPP and PPT #########################
######################################################

##### ANPP
anpp <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1dmzGwkGC1Y4uWW8nLXDkhtB_4A4suyF3")) %>%
  dplyr::filter(name == "ANPP_filtered.csv")

# Did that work?
anpp 

# Download the excluded treatmetn file
googledrive::drive_download(file = anpp$id, overwrite = T, type = "csv",
                            path = file.path("deluge","data","R_exports", anpp$name))

# Read in hoover ppt
anpp <- read.csv(file = file.path("deluge","data","R_exports", "ANPP_filtered.csv")) %>%
  filter(!(Study_shorthand == "Tooley_DRE" & str_detect(trt, "DR"))) %>%
  filter(year %!in% c("July-Sept Regrowth 2021","July 2021")) %>%
  mutate(year = as.integer(ifelse(year == "Sept 2021", "2021", year))) %>%
  # create a new func group that's just grass, anpp, or total
  mutate(func = case_when(
    funct_group == "ANNUALS" ~ "GRASS", 
    funct_group == "Annual" ~ "GRASS", 
    funct_group == "Forb" ~ "FORB",
    funct_group == "C3" ~ "GRASS",
    funct_group == "C4" ~ "GRASS",
    funct_group == "C4.ANNUALS" ~ "GRASS",
    TRUE ~ funct_group
  )) %>%
  # summarize into grass, annual, or total
  group_by(Study_shorthand, year, Block, plot, trt, func) %>%
  summarize(anpp = sum(ANPP)) %>%
  # get rid of woody
  filter(func != "WOODY") %>%
  filter(func != "Woody") %>%
  ungroup()

anpp.3 <- anpp%>%
  mutate(del = as.numeric(ifelse(trt %in% c("CON", "DR"), "0", "1"))) %>%
  group_by(Study_shorthand, year, Block, trt, func, del) %>%
  summarize(mean_ANPP = mean(anpp), se_ANPP = (sd(anpp))/(sqrt(n())))

block_summary <- anpp.3%>%
  filter(!is.na(Block)) %>%
  group_by(Study_shorthand, year, trt, func, del) %>%
  summarise(mean_anpp = mean(mean_ANPP), se_ANPP = (sd(mean_ANPP))/(sqrt(n())), .groups = "drop") %>%
  rename(mean_ANPP = "mean_anpp")

anpp.4 <- anpp.3 %>%
  filter(is.na(Block)) %>%
  ungroup()%>%
  select(-Block) %>%
  rbind(block_summary)

# precip data
ppt <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/18toIfC8jV7s8ZzAD0SV9CWL4SvGgn-RO")) %>%
  dplyr::filter(name == "precip_data_cleaned_trts.csv")

# Did that work?
ppt

# Download the excluded treatmetn file
googledrive::drive_download(file = ppt$id, overwrite = T, type = "csv",
                            path = file.path("deluge","precip_data", ppt$name))

# Read in the PRS file
ppt <- read.csv(file = file.path("deluge","precip_data", "precip_data_cleaned_trts.csv")) %>%
  rename(trt = Our_trt)

anpp.ppt.ex <- left_join(anpp.4, ppt, by = c("Study_shorthand", "year", "trt"))

# Look into merging and need to make sure that get deluge size
trt<- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1dmzGwkGC1Y4uWW8nLXDkhtB_4A4suyF3")) %>%
  dplyr::filter(name == "deluge_characteristics_mergeable.csv")

# Did that work?
trt

# Download the excluded treatmetn file
googledrive::drive_download(file = trt$id, overwrite = T, type = "csv",
                            path = file.path("deluge","data","R_exports", trt$name))

# Read in hoover ppt

trt <- read.csv(file = file.path("deluge","data","R_exports", "deluge_characteristics_mergeable.csv")) %>%
  select(Study_shorthand, year, trt, del_size, del_end) %>%
  mutate(del_end = as.Date(del_end, format = "%m/%d/%y")) %>%
  mutate(month = month(del_end)) %>%
  select(-del_end)

glimpse(trt)
anpp.ppt.ex2 <- left_join(anpp.ppt.ex, trt)


######################################################
##### Join hist and exp data #########################
######################################################
glimpse(anpp.ppt)
glimpse(anpp.ppt.ex2)

exp <- anpp.ppt.ex2 %>%
  filter(func == "ANPP") %>%
  select(c(Study_shorthand, year, trt, mean_ANPP, ann_ppt, del_size, month)) %>%
  mutate(data = "EXP")

hist <- anpp.ppt %>%
  select(-c(group, n.days, start_date, end_date)) %>%
  rename(mean_ANPP = anpp, del_size = total_precip, ann_ppt = ann.ppt) %>%
  mutate(Study_shorthand = "LTNPP", trt = "HIST", data = "HIST")
glimpse(exp)
glimpse(hist)

hist.exp <- rbind(exp, hist)

ggplot(hist.exp, aes(ann_ppt, mean_ANPP, color = del_size,shape = data))+
  geom_point(size = 2)+
  scale_color_viridis_c(option = "mako", direction = -1, end = 0.9,
                        limits = c(NA, quantile(anpp.ppt$total_precip, 0.95, na.rm = TRUE)),
                        oob = scales::squish) +
  theme_bw()


# To do
# Need to make historical water year instead.....
# Fix Greg's ppt or confirm that it is correct...
# maybe also just loook at grass, i don't think that changes things too much. greg's don't also seem to have the deluge



