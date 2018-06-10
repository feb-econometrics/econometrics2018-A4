## Problem set 4 / Problem 2
## Problem description: 
## https://firebasestorage.googleapis.com/v0/b/uni-sofia.appspot.com/o/assignments%2FA4.pdf?alt=media

adv <- read.csv('https://s3.eu-central-1.amazonaws.com/econometrics2018/data/Advertising.csv')
str(adv)

## 1) Fit separete regressio models for each advertising channel

fitTV <- lm(sales ~ TV, data = adv)
fitRadio <- lm(sales ~ radio, data = adv)
fitNewspaper <- lm(sales ~ newspaper, data = adv)

summary(fitTV)
summary(fitRadio)
summary(fitNewspaper)


## 2) Fit a model with all three predictors
fitAll <- lm(sales ~ TV + radio + newspaper, data = adv)

## 3) 

## Note that the coefficient of newspaper is not significantly
## different from zero in this model. To see why this happens
## examine the relationship between newspaper and radio budgets

ggplot(data = adv, aes(x = newspaper, y = radio)) + geom_point()
summary(lm(radio ~ newspaper, data = adv))

## 4)

summary(fitAll)
2.938 ## 100,000 USD

2.938 - 2 * 0.311908
2.938 + 2 * 0.311908

# 5)
summary(fitAll)

## Estimated increase
0.0457


## 6)
## Television and radio advertising contribute to sales
## because their coefficients are positive and significantly
## different from zero.
