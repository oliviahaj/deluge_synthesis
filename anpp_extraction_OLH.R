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
