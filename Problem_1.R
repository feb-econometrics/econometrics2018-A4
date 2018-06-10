## Problem set 4 / Problem 1
## Problem description:
## https://firebasestorage.googleapis.com/v0/b/uni-sofia.appspot.com/o/assignments%2FA4.pdf?alt=media

library(ggplot2)

sales <- read.csv('https://s3.eu-central-1.amazonaws.com/econometrics2018/data/sales.csv')
str(sales)

## 1) The estimated price elasticity is the coefficient for log(price)
fit0 <- lm(log(quantity) ~ log(price), data = sales)

## 2)
summary(fit0)

## 3)
ggplot(data = sales, aes(x = log(price), y = log(quantity))) +
  geom_point() +
  geom_abline(slope = 3.2358, intercept = -22.3813) 

## 4)
plot(log(sales$quantity))
plot(log(sales$price))


## 5) Fit a separate model to observation 1:50 and 51:100

fit1 <- lm(log(quantity) ~ log(price), data = sales[1:50, ])
fit2 <- lm(log(quantity) ~ log(price), data = sales[51:100, ])
summary(fit1)
summary(fit2)

## Visualise the estimated models (fit0, fit1, fit2) in the log(quantity)/log(price) scatterplot
ggplot(data = sales, aes(x = log(price), y = log(quantity))) +
  geom_point() + 
  geom_abline(slope = 3.2358, intercept = -22.3813) + ## fit0 (original model)
  geom_abline(slope = -3.735, intercept = 13.752, color = 'red') + ## fit1
  geom_abline(slope = -3.2187, intercept = 0.9107, color = 'green') ## fit2

## Instead of estimating the model on subsets of the sample
## we estimate it by including an additional regressor 
## that identifies the the two periods
  
## Create period = TRUE for the first 50 observations and period = FALSE for the 
## last 50 observations

sales$period <- 1:100 <= 50  

## Add period to the model
fit3 <- lm(log(quantity) ~ log(price) + period, data = sales)
summary(fit3)

## Visualise the estimated model in the log(quantity)/log(price) scatterplot

ggplot(data = sales, aes(x = log(price), y = log(quantity))) +
  geom_point() + 
  geom_abline(slope = 3.2358, intercept = -22.3813) + ## fit0 (original model)
  geom_abline(slope = -3.2527, intercept = 1.0401, color = 'red') + ## fit3 period == FALSE 
  geom_abline(slope = -3.2527, intercept = 1.0401 + 10.3008, color = 'blue') ## fit3 period == TRUE
  
## NOT that this multiple regression model is not equivalent to the 
## separate regressions above because the slopes of the two lines
## are constrained to be equal. This can also be relaxed in the multiple
## regression model but we have not discussed it in class.

## For those interested it would looke like this (not relevant for the exam)

fit4 <- lm(log(quantity) ~ log(price)*period, data = sales)
summary(fit4)
