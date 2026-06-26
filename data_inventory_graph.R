# Variables incldued
# OLH
# 6-26-2026

# script to pull in teh data table adn make a quick visual of what data exists

library(googlesheets4)
library(tidyverse)

df <- read_sheet("https://docs.google.com/spreadsheets/d/1dvwtvZkqs7lHylrUnbClLWch6PIou40XXl3vDTV1dko/edit?gid=0#gid=0")

# clean up the sehet
str(df)
data <- df %>%
  filter(!is.na(Study))%>%
  filter(Study != "Felton et al. 2019") %>%
  filter(Study != "Post and Knapp 2019")%>%
  select(c(Study, ANPP, funct_groups, BNPP, Soil_moisture, Sentek, Soil_respiration, Canopy_greenness, PRS,
           Water_potential))


library(patchwork)

data_long <- data %>%
  pivot_longer(cols = -Study, names_to = "variable", values_to = "value")

# summary for bar chart — count only full "yes" (1) or adjust as needed
df_counts <- data_long %>%
  group_by(variable) %>%
  summarise(n = sum(value == 1, na.rm = TRUE))

# top bar chart
p_top <- ggplot(df_counts, aes(x = variable, y = n)) +
  geom_col(fill = "grey33") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab("") + ylab("# Studies")

# main tile plot
p_main <- ggplot(data_long, aes(x = variable, y = Study, fill = factor(value))) +
  geom_tile(color = "grey80") +
  scale_fill_manual(values = c("0" = "white", "0.5" = "orange", "1" = "darkolivegreen", "2" = "darkslateblue"),
                    labels = c("No", "Some", "Yes", "Exists?"),
                    name = "") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") + ylab("")

# combine
p_top / p_main + plot_layout(heights = c(1, 4))

