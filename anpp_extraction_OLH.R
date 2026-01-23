library(tidyverse)
library(googledrive)
library(broom)
library(corrplot)

theme_set(theme_bw(12))

dir.create(file.path("deluge"), showWarnings = F)
dir.create(file.path("deluge", 'data'), showWarnings = F)


## Post and Knapp 2020
#read in annp, precip, and trt info
file2<-'Post2020_ANPP.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1-z0EMfF8_LTWr3HZVajzcO3K9jKgYM9p")) %>% 
  dplyr::filter(name == file2) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("deluge", 'data', .$name))

## Calculate s