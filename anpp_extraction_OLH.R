library(tidyverse)
library(googledrive)
library(broom)

theme_set(theme_bw(12))

dir.create(file.path("deluge"), showWarnings = F)
dir.create(file.path("deluge", 'data'), showWarnings = F)


## Post and Knapp 2020
###ANPP
#read in annp, precip, and trt info
file2<-'Post2020_ANPP.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1-z0EMfF8_LTWr3HZVajzcO3K9jKgYM9p")) %>% 
  dplyr::filter(name == file2) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("deluge", 'data', .$name))

## Calculate treatment level means
post2020 <- read.csv(file = file.path("deluge", 'data', file2))
str(post2020)

post2020.plot.sum <- post2020 %>%
  # first grouping by plot
  group_by(Plot, Trt) %>%
  summarize(mean = mean(ANPP_total, na.rm = T))%>%
  ungroup()
post2020.trt.sum <- post2020.plot.sum %>%
  group_by(Trt)%>%
  summarize(mean_ANPP = mean(mean, na.rm = TRUE), se_ANPP = sd(mean) / sqrt(length(mean)))
           

###BNPP
#read in annp, precip, and trt info
file2<-'Post2020_BNPP.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1-z0EMfF8_LTWr3HZVajzcO3K9jKgYM9p")) %>% 
  dplyr::filter(name == file2) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("deluge", 'data', .$name))

## Calculate treatment level means
post2020 <- read.csv(file = file.path("deluge", 'data', file2))
str(post2020)

post2020.plot.sum <- post2020 %>%
  # first grouping by plot
  group_by(Plot, Trt) %>%
  summarize(sum_20 = sum(BNPP))%>%
  ungroup()
post2020.trt.sum <- post2020.plot.sum %>%
  group_by(Trt)%>%
  summarize(mean_BNPP = mean(sum_20, na.rm = TRUE), se_ANPP = sd(sum_20) / sqrt(length(sum_20)))


## Post and Knapp 2021
###ANPP
#read in annp, precip, and trt info
file2<-'Post2021_ANPP.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1-z0EMfF8_LTWr3HZVajzcO3K9jKgYM9p")) %>% 
  dplyr::filter(name == file2) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("deluge", 'data', .$name))

## Calculate treatment level means
post2021 <- read.csv(file = file.path("deluge", 'data', file2))
str(post2021)

post2021.plot.sum <- post2021 %>%
  # first grouping by plot
  group_by(Plot, Trt) %>%
  summarize(mean = mean(ANPP_total, na.rm = T))%>%
  ungroup()
post2021.trt.sum <- post2021.plot.sum %>%
  group_by(Trt)%>%
  summarize(mean_ANPP = mean(mean, na.rm = TRUE), se_ANPP = sd(mean) / sqrt(length(mean)))


###BNPP
#read in annp, precip, and trt info
file2<-'Post2021_BNPP.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1-z0EMfF8_LTWr3HZVajzcO3K9jKgYM9p")) %>% 
  dplyr::filter(name == file2) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("deluge", 'data', .$name))

## Calculate treatment level means
post2021 <- read.csv(file = file.path("deluge", 'data', file2))
str(post2021)

post2021.plot.sum <- post2021 %>%
  # first grouping by plot
  group_by(Plot, Trt) %>%
  summarize(sum_30 = sum(BNPP))%>%
  ungroup()
post2021.trt.sum <- post2021.plot.sum %>%
  group_by(Trt)%>%
  summarize(mean_BNPP = mean(sum_30, na.rm = TRUE), se_ANPP = sd(sum_30) / sqrt(length(sum_30)))


## Hoover et al. 2022
#read in productivity data
file2<-'Hoover2022_CPER-DEX_Productivity.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1-z0EMfF8_LTWr3HZVajzcO3K9jKgYM9p")) %>% 
  dplyr::filter(name == file2) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("deluge", 'data', .$name))

## Calculate treatment level means
hoover2022<- read.csv(file = file.path("deluge", 'data', file2))
str(hoover2022)

## Summarize by treatment
hoover2022.trt.sum <- hoover2022 %>%
  group_by(treatment)%>%
  summarize(
    mean_ANPP = mean(anpp, na.rm = TRUE), se_ANPP = sd(anpp) / sqrt(length(anpp)),
    mean_BNPP = mean(bnpp, na.rm = TRUE), se_BNPP = sd(bnpp) / sqrt(length(bnpp)))




###Felton et al. 2019
#read in productivity data
file2<-'Felton2019_sgs_ANPP.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1-z0EMfF8_LTWr3HZVajzcO3K9jKgYM9p")) %>% 
  dplyr::filter(name == file2) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("deluge", 'data', .$name))

## Calculate treatment level means
felton2019<- read.csv(file = file.path("deluge", 'data', file2))
str(felton2019)

felton2019.plot.sum <- felton2019 %>%
  # first grouping by plot
  group_by(Plot, Block, percentile) %>%
  summarize(mean = mean(x.100, na.rm = T))%>%
  ungroup()
felton2019.trt.sum <- felton2019.plot.sum %>%
  group_by(percentile)%>%
  summarize(mean_ANPP = mean(mean, na.rm = TRUE), se_ANPP = sd(mean) / sqrt(length(mean)))

