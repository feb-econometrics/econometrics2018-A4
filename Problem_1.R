library(ggplot2)

sales <- read.csv('https://s3.eu-central-1.amazonaws.com/econometrics2018/data/sales.csv')
str(sales)

## Visualise the data (scatterplot)

ggplot(data = sales, aes(x = log(price), y = log(quantity))) +
  geom_point()

fit0 <- lm(log(quantity) ~ log(price), data = sales)
summary(fit0)

## Visualise the estimated regression line

ggplot(data = sales, aes(x = log(price), y = log(quantity))) +
  geom_point() +
  geom_smooth(method = "lm") 

## Fit a separate model to observation 1:50 and 51:100

fit1 <- lm(log(quantity) ~ log(price), data = sales[1:50, ])
fit2 <- lm(log(quantity) ~ log(price), data = sales[51:100, ])

## Visualise the estimated models (fit0, fit1, fit2) in the log(quantity)/log(price) scatterplot
## 
ggplot(data = sales, aes(x = log(price), y = log(quantity))) +
  geom_point() + 
  geom_smooth(method = "lm") + ## fit0 (original model)
  geom_abline(slope = , intercept = , color = 'red') + ## fit1
  geom_abline(slope = , intercept = , color = 'blue') + ## fit2

## Fit a model with separate intercepts for period

## Create period = TRUE for the first 50 observations and period = FALSE for the 
## last 50 observations

fit3 <- lm(log(quantity) ~ log(price) + period, data = sales)
summary(fit3)

## Visualise the estimated model in the log(quantity)/log(price) scatterplot

ggplot(data = sales, aes(x = log(price), y = log(quantity))) +
  geom_point() + 
  geom_smooth(method = "lm") + ## fit0 (original model)
  geom_abline(slope = , intercept = , color = 'red') + ## fit3 period == FALSE 
  geom_abline(slope = , intercept = , color = 'blue') + ## fit3 period == TRUE
