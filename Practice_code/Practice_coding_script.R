# Welcome to R
# Em Lim
# May 31, 2022

# A quick forward:
# I have "soft wrap" turned on, so R automatically starts a new line when my code hits the edge of my page
# If you don't have this on, you'll get some super long lines (sorry)
# To turn it on, go Tools > Global Options > Code > check "soft wrap R source files" > Apply !

# OK, let's begin!!!!

# Getting started ------
# Do you see how adding a bunch of dashes after the hash mark creates a section heading, and you can now see the heading at the bottom left corner of this script box?
# And you can click that to navigate between sections

# We must first install required packages
# Once a package is installed we don't need to install them again
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("ggeffects")
#install.packages("lme4")

# Now we'll load them into this project so we can use them!
# Note that you don't need "" marks here
# You need to load packages each time you use them in a new project
library(tidyverse) # Package for data manipulation
library(ggplot2) # Graphing package
library(ggeffects)  # calculates effects 
library(lme4)

# Now, let's learn how to load in your data!
# You need to first enter your data into an excel spreadsheet and then save the file in a .csv (Comma Separated Values) format that R can understand. R does not like capitals or spaces in data column names so try to use periods or underscores as spacers, e.g., barnacle.data or barnacle_data. 

# Tidyverse expects tidy data: That is, data where each row is an observation, each column is a variable, and each cell is a single value

# The magic of working out of a repository is that the file path will be the same for everyone! We all have the project saved in a different place on our computers, but once we're inside the project R is pulling the files from the same place

# Barnacles ------
# This is a data set obtained by students where they placed a 5 cm^2 quadrat over patches of barnacles and recorded the number of barnacles in the quadrat (density) and the number of feeding strokes (strokes)
barnacles <- read_csv("Practice_data/MBE2015_BarnacleData.csv")

# Now we can take a quick peek at this data using the following functions:

str(barnacles)  # #data points & variables, kind of data and first few values
head(barnacles) #var names and first few values
names(barnacles) #names of variables
summary(barnacles) #stats summary of each variable
View(barnacles) #opens up the whole data frame

# Now let's make a simple plot of our barnacle data which has just two continuous variables, strokes/min (our dependent variable) and barnacles per quadrat (our independent variable):

ggplot(data = barnacles, aes(x = density, y = strokes)) + 
  geom_point()
# ggplot() is where we tell ggplot where the data is coming from
# aes() sets the parameters of the plot, this is where we can assign the x and y variables
# geom_point() tells ggplot to add the points to this plot

# We can see the data, but let's spruce up this plot a little
ggplot(data = barnacles, aes(x = density, y = strokes)) + 
  geom_point() +
  theme_classic() + # this will make the background look nicer
  ylab("Feeding (strokes per min)") +
  xlab("Barnacle density") + # We can add labels to the axis 
  geom_smooth(method = lm) # Add a best fit line to the data. method = lm specifies that we want a straight line

# What if we want to specify the units for density? barnacles / 5 cm^2?
# Let's get super fancy
ggplot(data = barnacles, aes(x = density, y = strokes)) + 
  geom_point() +
  theme_classic() + 
  ylab("Feeding (strokes per min)") +
  xlab(expression(paste("Density (barnacles per cm"^"2",")"))) + # oooh ahhhh
  geom_smooth(method = lm) 

# It looks like there might be a trend in our data! Let's try making a linear model of the relationship between barnacle density and number of strokes per minute. Remember linear models always take data in the form dependent variable ~ independent variable.

barnacle_model <- lm(strokes ~ density, data = barnacles) # the basic model in the form of y = mx + b

# DIAGNOSTICS
plot.new() #now let's look at some diagnostic plots to make sure our model is appropriate
par(mfrow = c(2,2)) #this tells R to set up a 2x2 grid of 4 plots
plot(barnacle_model) #create the diagnostic plots
# Top left: Residual vs fitted = tests for the of variance - red line to be as straight as possible & smear of data with no pattern
# Top right: QQ plot = normality, should be more or less straight
# This top right plot doesn't look great
# Bottom left: Residual vs leverage = outliers (Cook's distance) Solid line is fairly straight.  All points within dotted lines means no outliers

dev.off() #turn off the 4x4 grid setup or all your future plots will go onto a grid

  
summary(barnacle_model) #the plots look fine, so now let's look at our model summary

# In our summary, you get the estimate for the intercept (b), the slope (m), the standard error for each estimate, and the p-values for those estimates (remember <0.05 is a significant value).

# The estimate of the intercept b = 62.6346  
# The estimate for density is the slope of the line, so m = 0.4092
# The Pr(>|t|) is your p-value

# The R-squared values tells you what proportion of the variation in the data is explained by the independent variable (here barnacle density).

# Anemones ----------
# Now let's look at a more complicated data set from past student's anemone observations which have two continuous variables (reaction time, pool area) and one categorical variable (clone vs. non-clone tentacles).

# Let's load that data
anemones <- read_csv("Practice_data/MBE2015_AnemoneData.csv")

# Let's plot the continuous data!
ggplot(data = anemones, aes(x = reaction.time.s, y = pool.area.cm2)) +
         geom_point() +
  theme_classic()

# Now let's plot continuous versus categorical data
# We'll use a boxplot to plot this first

ggplot(anemones, aes(x = different.colony, y = reaction.time.s, colour = different.colony)) + # note that colour goes inside the aes brackets
  geom_boxplot() +
  theme_classic() +
  labs(x = "Colony", y = "Reaction time (s)") +
  theme(legend.position = "none") # get rid of the legend

# Let's use a model to see if reaction times are different between clonal and nonclonal anemones
anemone_model <- lm(reaction.time.s ~ different.colony, data = anemones)
summary(anemone_model)

# Boxplots aren't the greatest way to display data, so consider using dot and whisker plots because:
  # https://bayesbaes.github.io/2022/04/13/fuck-boxplots.html

# In order to plot a dot and whiskers we need the means and standard errors for each categorical variable
# We can use ggpredict() from the ggeffects package to calculate these using our model

sum_stats_gg <- ggpredict(anemone_model, terms = "different.colony") %>% 
  #and then we'll just rename one of the columns so it's easier to plot
  rename(different.colony = x,
                reaction.time.s = predicted)
View(sum_stats_gg)

# Now let's plot this 
ggplot() +
  geom_point(data = sum_stats_gg, 
             aes(x = different.colony, y = reaction.time.s),
             size = 4) +
  geom_errorbar(data = sum_stats_gg, 
                aes(x = different.colony,
                    y = reaction.time.s,
                    # and you can decide which type of error to show here
                    # we're using 95% CI
                    ymin = conf.low,
                    ymax = conf.high),
                width = 0.2,
                size = 1.2)  +
  geom_jitter(data = anemones, aes (x = different.colony, y = reaction.time.s),
              alpha = 0.2, height=0) +
  labs(x = "Colony", y = "Reaction time (s)") +
  theme_classic()

# You'll notice that we didn't specify the data inside ggplot(), instead we specified the data frame and x and y within each geom_()
# This is because we're using our summarized data frame to make the dot and whiskers and we used our full data frame to add the jittered raw data

# But look how nice that looks! And you can see the real trends in the data

# Now let's try playing around with dplyr!

# Reef Life Survey --------

# Here we're going to play with some messy data
# This is the data I helped collect last spring with Siobhan and Jasmin for reef life surveys
# RLS uses a standardized spreadsheet which doesn't read super cleanly into R

# let's see what happens if we just load in the data without any manipulations
rls_ugly <- read_csv("Practice_data/updated_RLS.csv")
View(rls_ugly) # you can see there's a  blank row, and the first column is also blank, and some of the column names have spaces. Let's fix this using tidyverse!

rls <- rls_ugly %>% # The %>% symbol is called a pipe. We can create a new df by "piping" our old data frame through a series of manipulations
  select(-1) %>% #  remove that first blank column
  slice(2:n()) %>% # remove the first blank row
  rename(
    site_ID = `Site No.`,
    site_name = `Site Name`, 
    common_name = `Common name`
  ) %>% # rename the columns with spaces in their names
  filter(Species != "Debris - Metal") %>%
  filter(Species != "Debris - Other") %>%
  filter(Species != "Debris - Wood") %>%
  filter(Species != "Debris - Glass") %>%
  filter(Species != "Debris - Fishing Gear") # Filter our the data on debris, we just want living species data!

# Let's take a look at this tidy dataframe now
View(rls)
# That looks better!

# I wonder what the most abundant species are?
# We can use group_by() and summarise() to group all counts of the same species together, and then calculate summary statistics on these groups
# Then we can arrange the summary stats into a nice table and use View() to look at it
rls_ranked <- rls %>% 
  group_by(common_name) %>% 
  summarise(sum = sum(Total))  %>%
  arrange(desc(sum)) %>%
  View()

# We can see now that red sea urchins were the most abundant thing we encountered on RLS surveys last year

# What if we wanted to find the site with the most bat stars?
rls_bats <- rls %>%
  filter(Species == "Patiria miniata") %>% # only retain observations of bat stars
  group_by(site_ID, site_name) %>%
  summarize(bat_stars = sum(Total)) %>% # sum total number of bat stars at each site
  arrange(desc(bat_stars)) %>% # sort the table from highest to lowest
  as.data.frame() # make this into a data frame

View(rls_bats)
# Faber and Wouwer have the most bat stars, and  both in the Broken group, maybe we can use this data to convince Siobhan and Isa to take us there!

# Other important functions in tidyverse: mutate() and left_join()

# We might also be interested in leather stars, how many of those did we find at each site?
rls_leather <- rls %>%
  filter(Species == "Dermasterias imbricata") %>%
  group_by(site_ID, site_name) %>%
  summarize(leather_stars = sum(Total)) %>%
  arrange(desc(leather_stars)) %>%
  as.data.frame()

View(rls_leather)

# I wonder where we found the most leather and bat stars, can we join these?
# YES! Using left_join() and mutate()
rls_stars <- rls_bats %>% # We start with one of the two data frames we want to join
  left_join(rls_leather, by = c("site_ID", "site_name")) %>% # then we specify the second data frame and choose the variable we want to join the two by. in our case, site_ID and site_name
  mutate(stars = leather_stars + bat_stars) %>% # create a new column with the summed number of bat and leather stars
  arrange(desc(stars))  

# Wouwer is throwing a NA because it's trying to add NA + a number
# FIX THIS

View(rls_stars)

# BONUS FUN ------

# Let's import some pee data to play with 
pee <- read_csv("Practice_data/RSL_pee.csv") %>%
  as.data.frame()

# I wonder if sites with higher numbers of stars also have higher concentrations of pee?
# Let's join the two data frames and find out
# We can also use a combination of mutate() and ifelse() to do manipulations that are dependent on variables in other columns
# ifelse() says if [condition is met], then [create a new variable], else, [create this other variable] 

star_pee <- rls_stars %>% 
  left_join(pee,  by = "site_ID") %>%
  mutate(begging = ifelse(site_name == "Faber Islets", "Please take us to the broken group", "Please please please"))

ggplot(star_pee, aes(stars, pee)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_classic()

ggsave("Output/star_pee.png", device = "png",
     height = 9, width = 16, dpi = 400)

# Fun tricks --------

# Option + drag your mouse
# This lets you type on multiple rows

# Ctrl + Shift + C ( Command + Shift + C on macOS)
# This hashes out multiple lines of code

# Ctrl+Shift+M (Windows) or Cmd+Shift+M (Mac)
# %>% symbol

# Use Ctrl+L to clear all the code from your console.

# You can rename all instances of a variable name by highlighting one instance of the variable name and then using Code > Rename in Scope. This is better than using Edit > Replace and Find because it only looks for whole word matches.

