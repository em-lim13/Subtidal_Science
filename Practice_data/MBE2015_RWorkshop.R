---
title: "Intro to R for MBE2015"
author: "Natascia Tamburello"
date: "Tuesday, June 02, 2015"
output: html_document
---

Here are some notes to help you work with your project data in R and generate graphs for your reports.

First, let's learn how to load in your data. You need to first enter your data into an excel spreadsheet and then save the file in a .csv (Comma Separated Values) format that R can understand. R does not like capitals or spaces in data column names so try to use periods or underscores as spacers, e.g., barnacle.data or barnacle_data. Remeber also to use FORWARD slashes (/) when specifying whth file path to load the data from.

For more info on which test to use, see: http://www.csun.edu/~amarenco/Fcs%20682/When%20to%20use%20what%20test.pdf
For more info on checking assumptions of tests, see: http://www.statmethods.net/stats/anovaAssumptions.html

```{r, loading data}

barnacles <-read.csv("C:/Users/Isabelle/Desktop/MBE2015_BarnacleData.csv", header=T, sep=",")
str(barnacles)  # #data points & variables, kind of data and first few values
head(barnacles) #var names and first few values
names(barnacles) #names of variables
summary(barnacles) #stats summary of each variable

barnacles$strokes  #what you're looking in, followed by the variable of interest
barnacles$density

#Let's also install some packages that will help us analyze and plot our data
install.packages("scales")
install.packages("MuMIn")
#After installing them you activate them in each R session using...
library("scales")  #Plotting package
library("MuMIn")  #AIC package
```

First let's make a simple plot of our barnacle data which has just two contonuous variables, strokes/min (our dependent variable) and barnacles per quadrat (our independent variable):

```{r, barnacle data}
#The super basic plot, remember that plots date data in the form "independent variable, dependent variable" X then Y
plot(barnacles$density, barnacles$strokes) 

#Let's dress up the plot a bit by adding labels and making our dots look nicer
plot(barnacles$density, barnacles$strokes, xlab = "Barnacles per quadrat (5 cm2)", ylab = "Feeding strokes per minute", main = "Barnacle density influences feeding frequency", 
     pch = 16, #this changes the symbol type - google pch codes
     cex = 1.5, #this changes the symbol size
     col = alpha("red", 0.5)) #this changes the color and transparency (0.5 = 50% transparent)
                              #alpha command helps to see overlapping data points
text (150, 25, "Barnacles")   #text command adds specified text at a given x, y position (here:150, 25)

segments(100, 50, 150, 50) #line starts at X=100, Y = 50 to X=150, Y=50
?segments #to get info about command


#Looks like there's a relationship in our data, let's try making a linear model of the relationship between barnacle density and number of strokes per minute. Remember linear models always take data in the form dependent variable ~ independent variable.

barnaclemodel <- lm(barnacles$strokes ~ barnacles$density) #the basic model in the form of y = a + bx

#DIAGNOSTICS
plot.new() #now let's look at some diagnostic plots to make sure our model is appropriate
par(mfrow = c(2,2)) #this tells R to set up a 2x2 grid of 4 plots
plot(barnaclemodel) #create the diagnostic plots
                    #Top left: Residual vs fitted = tests for het of variance - red line to be as straight as possible & smear of data with no pattern
                    #Top right: QQ plot = normality, should be more or less straight
                    #Bottom left: Residual vs leverage = outliers (Cook's distance) Solid line is fairly straight.  All points within dotted lines means no outliers

dev.off() #turn off the 4x4 grid setup or all your future plots will go onto a grid


summary(barnaclemodel) #the plots look ok, so now let's look at our model summary!

#in our summary, you get the estimate for the intercept (a), the slope (b), the standard error for each estimate, and the p-values for those estimates (remember <0.05 is a significant value).
#The R-squared values tells you what proportion of the variation in the data is explained by the independent variable (here barnacle density).

```

Now let's look at a more complicated data set from our anemone observations which have two continuous variables (reaction time, pool area) and one categorical variable (clone vs. non-clone tentacles).

```{r, anemone data}

anemones <-read.csv("C:/Users/Isabelle/Desktop/MBE2015_AnemoneData.csv", header=T, sep=",")
str(anemones)
head(anemones)
names(anemones)
summary(anemones)

#First let's plot our continuous data like before and then make a model
#Always make as many plots as you can
plot(anemones$reaction.time.s ~ anemones$pool.area.cm2)

#Now let's plot continuous versus categorical data, we'll see it looks different
#R plots continuous vs. categorical data as a box and whisker plot, where the middle line is the mean
plot(anemones$reaction.time.s ~ anemones$different.colony)

#We could also plot this as a bar plot, by first making a vector of the means for each group

#Prep the data [rows wanted, columns wanted]
bar.clones<-anemones [ which(anemones$different.colony=="clone"), ] #which allows to subset the data - this line chooses certain rows and all columns (i.e. blank)
bar.nonclones<-anemones [ which(anemones$different.colony=="nonclone"), ]
bar.means<- c(mean(bar.clones$reaction.time.s), mean(bar.nonclones$reaction.time.s))  #c = concatenate, put two things together

#Make the bar plot
anemone.bar<- barplot(bar.means, ylim = c(0,4000), ylab= " Reaction time (s)", main =  "Anemone reaction time", col = c("blue","red"))
axis(1, labels=c("Clones", "Non-clones"), at = anemone.bar) #x axis labels, pos 1 is bottom
box()

#Add the error bars
# Get standard deviation of each group
# The standard deviations are saved in a matrix of same size 
# as the matrix with midpoints, this is useful for plotting 
# the error bars
stDevs <- matrix(c(sd(bar.clones$reaction.time.s), sd(bar.nonclones$reaction.time.s)), 2)
# Plot the vertical lines of the error bars
# The vertical bars are plotted at the midpoints
segments(anemone.bar, bar.means - stDevs, anemone.bar, bar.means + stDevs, lwd=2) #lwd = line width
# Now plot the horizontal bounds for the error bars
# 1. The lower bar
segments(anemone.bar - 0.1, bar.means - stDevs, anemone.bar + 0.1, bar.means - stDevs, lwd=2)
# 2. The upper bar
segments(anemone.bar - 0.1, bar.means + stDevs, anemone.bar + 0.1, bar.means + stDevs, lwd=2)



#How could we determine if reaction times in these two groups are different? using a T-test!
#Shows that these groups are significantly different!
t.test(anemones$reaction.time.s ~ anemones$different.colony)

#Now let's make a model including both clone/nonclose and pool size
anemonemodel<- lm(anemones$reaction.time.s ~ anemones$different.colony + anemones$pool.area.cm2)
plot.new()
par(mfrow = c(2,2))
plot(anemonemodel)
dev.off()
summary(anemonemodel)

#What if we want to look at more than two groups? Maybe the students collecting the data influenced the results by using different methodology. First let's plot it.
plot(anemones$reaction.time.s ~ anemones$students)

#The data does look like it's different for groups 1 and 4 - why? Because they reported only non-clone data. We can test if more than 2 groups have different means using an ANOVA.

#We need a balanced number of trials for each group for ANOVA, so let's first subset the data to take only groups that did 3 clone and 3 nonclone trials.
an.trim<- (anemones [ which(anemones$students=="Windy&Carly" | anemones$students=="Helen&JessB"| anemones$students=="Gavia&Emma"), ])

anemoneanova <- aov(an.trim$reaction.time.s ~ an.trim$students)
summary(anemoneanova)

#Now let's assume there are small differences in how different groups measured the data that we want to control for, but that we aren't actually interested in. Another example of this is when taking measurements at multiple field sites where conditions vary slightly, and you might want to control for these when getting your  model estimates, but you don't care about the influences of the sites themselves. These nuisance factors are called "random effects" and can be controlled for using a mixed-effects model.

#First we install the mixed model package
install.packages("lme4")
library("lme4")

#Now let's run a mixed model with student group as a random effect and compare with our original model
anemonemodel.2 <- lmer(anemones$reaction.time.s ~ anemones$different.colony + anemones$pool.area.cm2 + (1|anemones$students)) #here +(1|anemone$students) is how you specify a random intercept, that is, if you expect the height of the relationships to vary across sites but not the slope, this is the most basic type of random effect.

#When we compare we see the resulting parameter estimates are only a little different, because we already saw student group doesn't matter, but it can be a lot different!
summary(anemonemodel.2)
summary(anemonemodel)

AICc(anemonemodel.2)
AICc(anemonemodel)
```

Now we'll look at a totally different data set to study interactions in models:
```{r, interactions}

# Here we'll look a data on fish age, fish length, and whether it is mature or not
# Variables are age, lengthcm, mature (where 1 = success (mature)), only response variable is categorical
maturedata <-read.csv("C:/Users/Isabelle/Desktop/matureWB.csv", header=T, sep=",")
str(maturedata)
head(maturedata)
names(maturedata)
summary(maturedata)
#This time let's attach the data so we don't have to call the whole data set name each time
attach(maturedata)

plot(maturedata)
plot(mature~age)
dev.off()

#Now let's make some models with the variables individually
maturedata.a <- glm(mature~age, family=binomial) #family=binomial bc resp var is mature or not
summary(maturedata.a)

maturedata.l <- glm(mature~lengthcm, family=binomial)
summary(maturedata.l)

#But maybe both length and age are important?
maturedata.al <- glm(mature~age+lengthcm, family=binomial)
summary(maturedata.al)

#And we know there is an interaction between age & length, as they grow older, they get longer...
#So we should capture this as an interaction term!
maturedata.alint <- glm(mature~age+lengthcm+age:lengthcm, family=binomial)
summary(maturedata.alint)

#How can we decide which one of these models is best? Using something called the kaike Information Criterion (AIC). AIC measures the tradeoffs between model fit and support for the variables in the model, with lower values indicating a better model. Here we'll use AICc which is adjusted for small sample sizes. You need the package "MuMIn" that we loaded earlier to do this.

AICc(maturedata.a)
AICc(maturedata.l)
AICc(maturedata.al)
AICc(maturedata.alint)


```