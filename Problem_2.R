adv <- read.csv('https://s3.eu-central-1.amazonaws.com/econometrics2018/data/Advertising.csv')

## Fit separete regressio models for each advertising channel

fitTV <- lm(sales ~ TV, data = adv)
fitRadio <- lm(sales ~ radio, data = adv)
fitNewspaper <- lm(sales ~ newspaper, data = adv)

## Examine the estimated models

summary(fitTV)
summary(fitRadio)
summary(fitNewspaper)

