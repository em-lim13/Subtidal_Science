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
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("MuMIn")

# Now we'll load them into this project so we can use them!
# Note that you don't need "" marks here
# You need to load packages each time you use them in a new project
library(tidyverse) # Package for data manipulation
library(ggplot2) # Graphing package
library(MuMIn)  #AIC package

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
# We'll use a dot and whiskers plot because... I'll let you click this link yourselves:
# https://bayesbaes.github.io/2022/04/13/fuck-boxplots.html
plot(anemones$reaction.time.s ~ anemones$different.colony)

