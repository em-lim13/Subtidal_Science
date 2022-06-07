# Cup coral density vs depth and max size
# Em Lim
# June 6, 2022

# Load packages -----
library(tidyverse) # Package for data manipulation
library(ggplot2) # Graphing package
library(visreg)
library(lmerTest)# Mixed effects linear models
source("Code/themes_code.R")

# Load data ----
cup <- read_csv("Data/2022_06_06_cup_coral.csv") %>%
  rename(diam_cm = LargestCoralDiameter_cm,
         density = Density_num_m2) %>%
  mutate(depth_zone = ifelse(Depth_m < 6, "5 m", "9 m"))


# Plot data ----
ggplot(data = cup, aes(diam_cm, density, colour = depth_zone)) +
  geom_point() +
  theme_white() +
  geom_smooth(method = lm) +
  labs(x = "Diameter (cm)", y = "Density", colour = "Depth")

ggsave("Output/Cup_corals.png", device = "png",
       height = 9, width = 16, dpi = 400)

# Stats ----

cup_model <- lmer(density ~ diam_cm * Depth_m + (1|Group), data = cup)
summary(cup_model)
