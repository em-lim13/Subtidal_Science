# Orange sea cucumber density versus rugosity
# Em Lim
# June 4, 2022

# Load packages -----
library(tidyverse) # Package for data manipulation
library(ggplot2) # Graphing package
library(visreg)
library(lmerTest)# Mixed effects linear models
source("Code/themes_code.R")

# Load data ----
cukes <- read_csv("Data/2022_06_04_cuke_rugosity.csv") %>%
  rowwise() %>%
  mutate(mean_r = mean(c(r1_m, r2_m)),
         rugosity = 1 - (mean_r/3)
  )

# This assumes the quadrat was 1 m x 1 m so the cuke count = density

# Plot data ------

ggplot(data = cukes, aes(rugosity, cukes)) +
  geom_point(aes(colour = depth_m), size = 3) +
  geom_smooth(method = lm, colour = "black", alpha = 0.2) +
  labs(x = "Rugosity", 
       y = "Orange sea cucumber density", 
       colour = "Depth (m)") +
  theme_white() +
  scale_colour_gradient(guide = guide_colourbar(reverse = TRUE),
                        low="#5EB7F8", high="#1A334B")


ggsave("Output/Cuke_rugosity.png", device = "png",
       height = 9, width = 16, dpi = 400)

# Stats -----

# Try a simple linear model
simple_model <- lm(cukes ~ rugosity, data = cukes)
summary(simple_model)
visreg(simple_model)

par(mfrow = c(2,2)) 
plot(simple_model)
dev.off()

# Try including depth and a random effect of observer
mixed_model <-lmer(cukes ~ rugosity * depth_m + (1|pair), data = cukes)
summary(mixed_model)  

AIC(simple_model, mixed_model)
