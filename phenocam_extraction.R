## Phenocam Extraction and Analysis

## load libraries
library(tidyverse)
library(phenocamr)


##download data
# AGM
phenocamr::download_phenocam(
  frequency = 3,
  veg_type = "GR",
  roi_id = 1000,
  site = "cperagm",
  phenophase = TRUE,
  out_dir = "/Users/olhajek/Desktop/deluge_synthesis/Deluge_synthesis/deluge/data/")


#TGM
phenocamr::download_phenocam(
  frequency = 3,
  veg_type = "GR",
  roi_id = 1000,
  site = "cpertgm",
  phenophase = TRUE,
  out_dir = "/Users/olhajek/Desktop/deluge_synthesis/Deluge_synthesis/deluge/data/")

# UVB
phenocamr::download_phenocam(
  frequency = 3,
  veg_type = "GR",
  roi_id = 1000,
  site = "cperuvb",
  phenophase = TRUE,
  out_dir = "/Users/olhajek/Desktop/deluge_synthesis/Deluge_synthesis/deluge/data/")


##read in data
agm <- read.table("/Users/olhajek/Desktop/deluge_synthesis/Deluge_synthesis/deluge/data/cperagm_GR_1000_3day.csv", header = TRUE, sep = ",") %>%
  mutate(station = "AGM")

plot(as.Date(agm$date), agm$smooth_gcc_90, type = "l", xlab = "Date",
     ylab = "Gcc (90th percentile)")

tgm <- read.table("/Users/olhajek/Desktop/deluge_synthesis/Deluge_synthesis/deluge/data/cpertgm_GR_1000_3day.csv", header = TRUE, sep = ",")%>%
  mutate(station = "TGM")

plot(as.Date(tgm$date), tgm$smooth_gcc_90, type = "l", xlab = "Date",
     ylab = "Gcc (90th percentile)")

uvb <- read.table("/Users/olhajek/Desktop/deluge_synthesis/Deluge_synthesis/deluge/data/cperuvb_GR_1000_3day.csv", header = TRUE, sep = ",")%>%
  mutate(station = "UVB")

plot(as.Date(uvb$date), uvb$smooth_gcc_90, type = "l", xlab = "Date",
     ylab = "Gcc (90th percentile)")

## Graph all of the data
pheno <- rbind(agm, tgm, uvb)

ggplot(pheno, aes(as.Date(date), smooth_gcc_90, color = station))+
  geom_line()+
  theme_bw()

# average by day
glimpse(pheno)
pheno.avg <- pheno %>%
  mutate(date = as.Date(date)) %>%
  group_by(date, year, doy) %>%
  summarize(mean_gcc_90 = mean(smooth_gcc_90), mean_gcc_75 = mean(smooth_gcc_75), sd90 = sd(smooth_gcc_90),
            sd75 = sd(smooth_gcc_75))

ggplot(pheno.avg, aes(date, mean_gcc_90))+
  geom_line()+
  theme_bw()

ggplot(pheno.avg, aes(doy, mean_gcc_90, color = as.factor(year)))+
  geom_line()+
  xlim(65, 260)+
  theme_bw()

# Now for each year extract the max

yr.max <- pheno.avg %>%
  ungroup()%>%
  group_by(year) %>%
  slice_max(mean_gcc_90, n=1)
  

# For the full time series - mean across all years


