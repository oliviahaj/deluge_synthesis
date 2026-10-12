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
  mutate(month = month(end_date), year2 = year(end_date), year = ifelse(month>9, year2 + 1, year2)) %>%
  filter(year < 2021)

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
library(ggpubr)
fit <- summary(lm(total_precip ~ ann.ppt, data = ppt.sum))
lab <- sprintf("R² = %.2f, p = %.3g", fit$r.squared, coef(fit)[2, 4])

ggplot(ppt.sum, aes(ann.ppt, total_precip)) +
  geom_point() +
  geom_smooth(method = "lm")+
  annotate("text", x = -Inf, y = Inf, label = lab, hjust = -0.1, vjust = 1.5, size = 6) +
  theme_bw(base_size = 16) +
  ylab("Max Event Size (mm)")+
  xlab("WaterYear Precipitation (mm)")+
  theme(panel.grid = element_blank())


del.num <- ppt.del %>%
  # note that at 60, get a nice distribution too
  filter(total_precip > 20) %>%
  group_by(year) %>%
  dplyr::count()

num <- ppt.sum %>%
  left_join(del.num) %>%
  mutate(n = replace_na(n, 0))

ggplot(num, aes(ann.ppt, n, color = total_precip)) +
  geom_point(size = 3) +
  scale_color_viridis_c(option = "mako", direction = -1, end = 0.9,
                        limits = c(NA, quantile(num$total_precip, 0.95, na.rm = TRUE)),
                        oob = scales::squish) +
  theme_bw(base_size = 16) +
  ylab("No. events over 20 mm")+
  xlab("WaterYear Precipitation (mm)")+
  theme(panel.grid = element_blank())

# what proportion of a years rain is in deluge - we're going to consider anythign over 20 here??
del.ppt <- ppt.del %>%
  filter(total_precip > 20) %>%
  group_by(year) %>%
  summarize(del_ppt = sum(total_precip), n_events = n())

deluge.precip <- ppt.sum %>%
  left_join(del.ppt) %>%
  mutate(del_ppt = replace_na(del_ppt, 0), n_events = replace_na(n_events, 0)) %>%
  mutate(prop_del = ifelse(del_ppt>0, del_ppt/ann.ppt,0), ppt_wo_del = ann.ppt - del_ppt)

ggplot(deluge.precip, aes(year, ppt_wo_del))+
  geom_point(size = 3)+
  theme_bw()

ggplot(deluge.precip, aes(year, ann.ppt))+
  geom_point(size = 3)+
  theme_bw()

ggplot(deluge.precip, aes(year, del_ppt))+
  geom_point(size = 3)+
  theme_bw()

ggplot(deluge.precip, aes(ann.ppt, ppt_wo_del))+
  geom_point() +
  theme_bw()

ggplot(deluge.precip, aes(ann.ppt, prop_del))+
  geom_point(size = 4) +
  ylab("Prop of rain falling in rain event > 20mm")+
  xlab("Water Year ppt (mm)")+
  theme_bw(base_size=15)

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

ggplot(hist.exp, aes(ann_ppt, mean_ANPP, shape = data))+
  geom_point(aes(color = del_size), size = 3)+
  #geom_smooth(method = "lm") +
  scale_color_viridis_c(option = "mako", direction = -1, end = 0.9,
                        limits = c(NA, quantile(anpp.ppt$total_precip, 0.95, na.rm = TRUE)),
                        oob = scales::squish) +
  theme_bw()


# To do
# Fix Greg's ppt or confirm that it is correct...
# maybe also just loook at grass, i don't think that changes things too much. greg's don't also seem to have the deluge
# would love to get 2021 - 2025 in here too b/c right now just 1983 - 2020
# control doens't have the deluge treatments - maybe just filter these out for now
# color by deluge - how many in a year? start with maybe like 30 or 40 mm
# how much of rain falls as a deluge? could look at subtracting out teh deluge amount/proportion that falls as a deluge
# annual precip and deluge size correlation figure

