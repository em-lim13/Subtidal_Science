# Sea star assemblages over time
# Em Lim
# June 4, 2022

# Load packages -----
library(tidyverse) # Package for data manipulation
library(ggplot2) # Graphing package
library(visreg)
library(lmerTest)# Mixed effects linear models
source("Code/themes_code.R")

# Load data, just keep sea stars
stars <- read_csv("Data/2022_06_06_star_data.csv") %>%
  filter(species != "purple urchin") %>%
  filter(species != "green urchin") %>%
  filter(species != "red urchin") 

# Let's just look at sun stars
sun_stars <- stars %>%
  filter(species %in% c("sunflower star", "striped sunflower star", "morning star"))


# Which stars are the most abundant?
stars_ranked <- stars %>%
  group_by(species) %>% 
  summarise(density = sum(density))  %>%
  arrange(desc(density))

abundant_stars <- stars %>%
filter(species != "velcro star") %>%
  filter(species != "giant pink star") %>%
  filter(species != "cushion star") %>%
  filter(species != "ochre star")

no_zeros <- abundant_stars %>%
  filter(density != 0)

# Plot the most abundant stars ???
ggplot(data = abundant_stars, aes(year, density, colour = species)) +
  geom_jitter(size = 2.8) +
  geom_smooth(alpha = 0.01, size = 1.6) +
  theme_white() +
  labs(x = "Year", y = "Density", colour = "Species") +
  xlim(c(2014, 2022))

ggsave("Output/Star_trend.png", device = "png",
       height = 9, width = 16, dpi = 400)

# Just sun stars
ggplot(data = sun_stars, aes(year, density, colour = species)) +
  geom_jitter(size = 2.8) +
  geom_smooth(alpha = 0.01, size = 1.6) +
  theme_white() +
  xlim(c(2014, 2022)) +
  labs(x = "Year", y = "Density", colour = "Species")

ggsave("Output/Sunstar_trend.png", device = "png",
       height = 9, width = 16, dpi = 400)

# Just non zeros?

ggplot(data = no_zeros, aes(year, density, colour = species)) +
  geom_jitter() +
  geom_smooth(alpha = 0.2, method = lm) +
  theme_white() +
  xlim(c(2014, 2022)) +
  labs(x = "Year", y = "Density", colour = "Species")

ggsave("Output/Star_trend.png", device = "png",
       height = 9, width = 16, dpi = 400)

