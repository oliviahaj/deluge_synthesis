## ANPP and Continuous REsponses
# October 15, 2026
#OLH

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

# Read in the ANPP data
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

anpp_var <- anpp.4 %>%
  filter(func == "ANPP")

con <- anpp_var %>%
  filter(del == 0) %>%
  mutate(pairing = str_sub(trt, 1, 2)) %>%
  select(pairing, Study_shorthand, year, func, mean_ANPP, se_ANPP) %>%
  rename(ctrl_anpp = "mean_ANPP", 
         ctrl_se = "se_ANPP")

trts <- anpp_var  %>%
  filter(del==1) %>%
  mutate(pairing = str_sub(trt, 1, 2), 
         pairing = ifelse(pairing == "DR", "DR", "CO")) %>%
  select(pairing, trt, Study_shorthand, year, func, mean_ANPP, se_ANPP) %>%
  rename(del_anpp = "mean_ANPP", 
         del_se = "se_ANPP")

paired <- left_join(con, trts) %>%
  mutate(anpp_diff =del_anpp - ctrl_anpp )

# Set up the ANPP comparison with the control and treatment to calculate the difference

# Read in the continuous files
cont <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1dmzGwkGC1Y4uWW8nLXDkhtB_4A4suyF3")) %>%
  dplyr::filter(name == "continuous_response_calcs.csv")

# Did that work?
cont

# Download the excluded treatmetn file
googledrive::drive_download(file = cont$id, overwrite = T, type = "csv",
                            path = file.path("deluge","data","R_exports", cont$name))

# Read in hoover ppt
cont <- read.csv(file = file.path("deluge","data","R_exports", "continuous_response_calcs.csv")) %>%
  mutate(year = ifelse(Study_shorthand == "Hajek2024", 2022, year))
glimpse(cont)
glimpse(paired)

# merge
vars <- left_join(cont, paired)

# visualize
glimpse(vars)


## GCC vs ANPP
ggplot(vars, aes(diff_AUC28_daily, anpp_diff))+
  geom_point(aes(color =timing))+
  geom_smooth(method = "lm")+
  theme_bw(base_size = 16)+
  xlab("28-Day Daily AUC Difference")+
  ylab("ANPP Diff")+
  theme(panel.grid = element_blank())+
  facet_wrap(~Variable, scales = "free")

ggplot(vars, aes(diff_AUC28_daily, anpp_diff, color =Study_shorthand))+
  geom_point()+
  theme_bw(base_size = 16)+
  xlab("28-Day Daily AUC Difference")+
  ylab("ANPP Diff")+
  theme(panel.grid = element_blank())+
  facet_wrap(~Variable, scales = "free")

ggplot(vars, aes(diff_AUC28_daily, anpp_diff, color =timing))+
  geom_point()+
  theme_bw(base_size = 16)+
  xlab("28-Day Daily AUC Difference")+
  ylab("ANPP Diff")+
  theme(panel.grid = element_blank())+
  facet_wrap(~Variable, scales = "free")


cor_labs <- vars %>%
  group_by(Variable) %>%
  summarise(
    r = cor.test(diff_AUC28_daily, anpp_diff)$estimate,
    p = cor.test(diff_AUC28_daily, anpp_diff)$p.value
  ) %>%
  mutate(lab = sprintf("r = %.2f\np = %s", r, format.pval(p, digits = 2, eps = 0.001)))

ggplot(vars, aes(diff_AUC28_daily, anpp_diff)) +
  geom_point(aes(color = timing)) +
  geom_smooth(method = "lm") +
  geom_text(data = cor_labs, aes(x = -Inf, y = Inf, label = lab),
            hjust = -0.1, vjust = 1.2, size = 4.5) +
  theme_bw(base_size = 16) +
  xlab("28-Day Daily AUC Difference") +
  ylab("ANPP Diff") +
  theme(panel.grid = element_blank()) +
  facet_wrap(~Variable, scales = "free")
