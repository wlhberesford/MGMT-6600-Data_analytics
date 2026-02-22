library("ggplot2")
library("readr")



## read dataset
NY_House_Dataset <- read_csv("./NY-House-Dataset.csv")

dataset <- NY_House_Dataset


ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()



## filter data
dataset <- dataset[dataset$PRICE<195000000,]


## column names
names(dataset)

## ----- Price ~ PROPERTYSQFT ----- ##
## fit linear model
lmod01 <- lm(PRICE~PROPERTYSQFT, data = dataset)

## print model output
summary(lmod01)

## scatter plot of 2 variables
plot(PRICE~PROPERTYSQFT, data = dataset)
abline(lmod01)

## better scatter plot of 2 variables with best fit line
ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

## residual scatter plot
ggplot(lmod01, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)

lmod11 <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)

## print model output
summary(lmod11)

## scatter plot of 2 variables with best fit line
plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod11)

## better scatter plot of 2 variables with best fit line

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


ggplot(lmod11, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)

## ----- Price ~ BEDS + BATHS ----- ##
## fit linear model
lmod02 <- lm(PRICE~BEDS+BATH, data = dataset)

## print model output
summary(lmod02)
plot(PRICE ~ BATH, data = dataset)

## residual scatter plot
ggplot(lmod02, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)



## ----- Price ~ PROPERTYSQFT + BATH + BEDS + (BATH * BEDS) ----- ##

dataset_log_friendly <- dataset[dataset$BATH>0,]

## fit linear model
lmod03 <- lm(PRICE~PROPERTYSQFT + BATH + BEDS + (BATH * BEDS), data = dataset)

## print model output
summary(lmod03)

## scatter plot of 2 variables
plot(PRICE~(BATH*BEDS), data = dataset)
abline(lmod03)

## residual scatter plot
ggplot(lmod03, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)

### THE END ###

