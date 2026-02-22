library(readr)
library(EnvStats)
library(nortest)

# set working directory (relative path)
#setwd("~/Courses/Data Analytics/Spring26/labs/lab 1/")

# read data
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

# view dataframe
View(epi.data)

# print vlues in variable
epi.data$gdp

# print summary of variable
summary(epi.data$gdp)


### Explore Variable ###

## take a copy of a variable from a dataframe into a separate variable
GDP <- epi.data$gdp

# find NAs in variable: function outputs vector of logical values, true if NA, false otherwise
NAs <- is.na(GDP)

# print
NAs

# function "which" returns row numbers of rows with NAs
rownums <- which(NAs)

# print rows with NAs
GDP[rownums]

GDP.complete <- GDP[!NAs]

## create copy of new variable

POP <- epi.data$population

# print values in variable
POP

# find NAs inv variavle - outputs vector of logical values, true if NA, false otherwise
NAs <- is.na(POP)

rownums <- which(NAs)

# print NAs
POP[rownums]

# take subset of NOT NAs from variable
POP.complete <- POP[!NAs]

POP.complete

  
# stats
summary(POP.complete)

# boxplot of variable(s)
boxplot(GDP.complete, names = c("GDP"))
boxplot( POP.complete, names = c("POP"))


### Histograms ###

# histogram (frequency distribution)
hist(GDP)

# define sequence of values over which to plot histogram
x <- seq(20., 80., 5)
  
# histogram (frequency distribution) over specified range
hist(GDP, x, prob=TRUE)

# print estimated density curve for variable
lines(density(GDP,bw="SJ")) # or try bw=“SJ”

# print rug under histogram
rug(GDP)


## plot  histogram again

# histogram (frequency distribution) over range
hist(GDP, x, prob=TRUE) 

# range of values
x1<-seq(20,80,1)

# generate probability density values for a normal distribution with given mean and sd
d1 <- dnorm(x1,mean=46, sd=11,log=FALSE)

# print density values
lines(x1,d1)


### Empirical Cumulative Distribution Function ###

# plot ecdfs
plot(ecdf(GDP), do.points=FALSE, verticals=TRUE) 

plot(ecdf(POP), do.points=FALSE, verticals=TRUE) 


### Quantile-quantile Plots ###

# print quantile-quantile plot for variable with theoretical normal distribuion
qqnorm(GDP); qqline(GDP)


# print quantile-quantile plot for random numbers from a normal distribution with theoretical normal distribution

# rnorm generates random values from a normal distribution with given mean and sd
x <- rnorm(180, mean=46, sd=10)

qqnorm(x); qqline(x)


# print quantile-quantile plot of two variables

qqplot(GDP, POP, xlab = "Q-Q plot for GDP & POP") 



## Statistical Tests

x <- rnorm(500)
y <- rnorm(500)

hist(x)
hist(y)

shapiro.test(x)
shapiro.test(y)

ad.test(x)
ad.test(y)

ks.test(x,y)

wilcox.test(x,y)

var.test(x,y)
t.test(x,y)

hist(x, col='lightsteelblue')
hist(y, col='lightgreen', add=TRUE)


### THE END ###

