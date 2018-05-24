adv <- read.csv('https://s3.eu-central-1.amazonaws.com/econometrics2018/data/sales.csv')
str(adv)

##  https://s3.eu-central-1.amazonaws.com/econometrics2018/data/sales.csv 

library(dplyr)
library(ggplot2)

dds <- read.delim('./dds_data.csv')
dds1 <- subset(dds, Ethnicity %in% c('White not Hispanic', 'Hispanic'))
str(dds)

summary(dds)

## Plot

ggplot(data = dds, aes(x = Ethnicity, y = Expenditures)) + 
  stat_summary(fun.y = 'mean', geom='bar')

averageExpendituresByEthnicity <- 
  dds %>%
    group_by(Ethnicity) %>%
    summarise(averageExpenditures = mean(Expenditures))

ggplot(data = averageExpendituresByEthnicity, 
       aes(y = Ethnicity, x = averageExpenditures)
       ) +
  geom_point()

ggplot(data = dds1, aes(x = Age, y = Expenditures)) +
  geom_point() +
  facet_grid(~Ethnicity)

fit0 <- lm(Expenditures ~ Ethnicity, data = dds1)
summary(fit0)

fit1 <- lm(Expenditures ~ Ethnicity + Age, data = dds1)
summary(fit1)

n <- 100
price <- arima.sim(list(2, 1, 0), n = n, innov = rnorm(n, 5000, 400)) - 40 * (1:n)
logQuantity <- -0.05 * log(price) + rnorm(n, 1, 1)
sales <- data.frame(price, quantity = exp(logQuantity))
ggplot(data = sales, aes(x = log(price), y = log(quantity))) + 
  geom_point() +
  geom_smooth(method = "lm")

fit <- lm(log(quantity) ~ log(price), data = sales)
summary(fit)

plot(sales$price)
plot(sales$quantity)

sales1 <- sales[1:50, ]
sales2 <- sales[51:80, ]
sales3 <- sales[81:100, ]

fit1 <- lm(log(quantity) ~ log(price), data = sales1)
summary(fit1)
fit2 <- lm(log(quantity) ~ log(price), data = sales2)
summary(fit2)
fit3 <- lm(log(quantity) ~ log(price), data = sales3)
summary(fit3)


n <- 50
period <- rep(1:0, each = n)

price <- 100 * period + rnorm(n, 50, 20)
plot(log(price))


logQuantity <- 10 * period - 3 * log(price) + rnorm(n)
sales <- data.frame(price, quantity = exp(logQuantity))
sales
ggplot(data = sales, aes(x = log(price), y = log(quantity))) + 
  geom_point() +
  geom_smooth(method = "lm")

fitAll <- lm(log(quantity) ~ log(price), data = sales)
summary(fitAll)

fit1 <- lm(log(quantity) ~ log(price), data = sales[1:50, ])
summary(fit1)

fit2 <- lm(log(quantity) ~ log(price), data = sales[51:100, ])
summary(fit2)

sales$day <- 1:n
write.csv(sales[, c('day', 'price', 'quantity')], file='sales.csv', row.names = FALSE)
