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



# Updated ESA figure
unique(data_long$Study)
combined_colors<-c(
  # Post2020
  "Post and Knapp 2020"       = "#332288",
  
  # Post2021
  "Post and Knapp 2021"       = "#88ccee",

  # Hoover
  "Hoover et al. 2022"     = "#999933",
  
  # Condon_SL
  "Condon and Knapp Ecosphere"     = "#117733",

  # Linabury
  "Linabury CHANGE plots"     = "#cc6677",
  
  # CCE
  "Siggers et al. CEE"     = "#ddcc77",

  # Hajek
  "Hajek and Knapp 2024" = "#44AA99",

  # Tooley - 
  "Tooley DRE" = "#882255"
)


data_long.2 <- data_long %>%
  filter(value == 1)
unique(data_long.2$variable)

data_long.2 <- data_long.2 %>%
  mutate(variable = recode_values(variable,
                               "ANPP"              ~ "ANPP",
                               "funct_groups"      ~ "ANPP - Func. Gp.",
                               "BNPP"              ~ "BNPP",
                               "Soil_moisture"     ~ "20cm Soil Moisture",
                               "Soil_respiration"  ~ "Soil Respiration",
                               "Canopy_greenness"  ~ "Canopy Greenness",
                               "Water_potential"   ~ "Water Potential",
                               "Sentek"            ~ "Deep Soil Moisture",
                               "PRS"               ~ "Nutrient Probes"
  )) %>%
  mutate(variable = factor(variable, levels = c(
    "Water Potential", "Nutrient Probes", "Soil Respiration", "Canopy Greenness",
    "Deep Soil Moisture", "20cm Soil Moisture", "BNPP", "ANPP - Func. Gp.",
    "ANPP"
  )))


esa.det <- ggplot(data_long.2, aes(x = variable, fill = Study)) +
  geom_bar() +
  coord_flip()+
  scale_fill_manual(values=combined_colors)+
  labs(x = "Variable", y = "Number of studies", fill = "Study") +
  theme_minimal(base_size = 16)

ggsave("/Users/olhajek/Desktop/deluge_synthesis/Deluge_synthesis/deluge/figures/data_inventory.pdf", 
       plot = esa.det, width = 12, height = 6, dpi = 600)


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

