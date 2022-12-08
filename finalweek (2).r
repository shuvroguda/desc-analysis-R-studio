#Final Week Responses
library(stats)
#Question 1
n <- 29
xbar <- 71.9
s <- 29.6

#calculation for interval:
margin <- qt(1-0.005,df=n-1)*s/sqrt(n)
upper <- xbar+margin
upper


#Z <- qnorm(1-0.005, df=n-1)
#xbar + (Z*s)/sqrt(n)


#Question 2
p_hat <- 37/388
z <- qt(1-0.01, df=n-1)
n <- 388
#calculation for interval:
p_hat - z * sqrt((p_hat*(1-p_hat))/n)


#Question 3
n <- 15
x <- c(0.36, 0.16, 0.49,0.43,0.12,0.44,0.32,0.48,0.29,0.28,0.22,0.39,0.42,0.32,0.20)
sumlogx <- sum(log(x))
(n/(-sumlogx))


#Question 4
#Solving for sample size, same equation as Q2 but rewritten
E <- 26.1
sigma <- 95.3
z <- qnorm(1-(0.03/2))
(z*sigma/E)^2

#Question 5

set.seed(123)
n <- 200
s <- 1
x1 <- rnorm(n,2,0.52)
x2 <-  rnorm(n,-1,0.12)
x3 <- x1*x2
e <- rnorm(n=200, mean= 0, sd = 1)
y <- 5 + 0.9*x1 + 3*x2 + e
summary(y)

#Question 6

q6 <- lm(formula = y ~ x1 + x2 + x3)
summary(q6)


#Question 7

q7 <- lm(formula = y ~ x1 + x2)
summary(q7)


#Question 8

q8 <- lm(formula = y ~ x1 + x3)
summary(q8)


#Question 10
q10 <- anova(q6, q7)
q10

anova(q7,q8)

#Question 12

setwd("D:/Works/Upworks/Ronak") #set directory to the path containing the ToyotaCorolla.csv file

toyota <- read.csv("ToyotaCorolla(2).csv")

#anova for multiple variables (over 4 doesn't work)

model1 <- lm(toyota[[1]] ~ toyota$Age +toyota$HP+toyota$cc+toyota$Weight+toyota$KM+toyota$Fuel_Type+toyota$Doors+toyota$Gears+toyota$Automatic)

summary(model1)
#Question 13

model2 <- lm(toyota[[1]] ~ toyota$Age +toyota$HP+toyota$Weight+toyota$KM+toyota$Fuel_Type+toyota$Gears)

summary(model2)

#check F values for each variable
anova(lm(toyota[[1]] ~ toyota$Fuel_Type, data = toyota))
anova(lm(toyota[[1]] ~ toyota$Automatic, data = toyota))
anova(lm(toyota[[1]] ~ toyota$Gears, data = toyota))


#Question 15

attach(toyota)
model1 <- lm(toyota[[1]] ~ toyota$Age +toyota$HP+toyota$cc+toyota$Weight+toyota$KM+toyota$Fuel_Type+toyota$Doors+toyota$Gears+toyota$Automatic+toyota$Quarterly_Tax)
model2 <- lm(toyota[[1]] ~ toyota$Age +toyota$HP+toyota$cc+toyota$KM+toyota$Fuel_Type+toyota$Doors+toyota$Gears+toyota$Automatic+toyota$Quarterly_Tax)


pred <- predict.lm(model1, data.frame(age=c(57), (KM=c(45000)),(Fuel_Type="Diesel"),(HP=c(100)), (Automatic=c(0)),(cc=c(1600)),(Doors=c(4)), (Gears=c(5)), (Quarterly_Tax=c(88))))
summary(pred)

pred <- predict.lm(model2, data.frame(age=57, KM=45000,Fuel_Type="Diesel",HP=100, Automatic=0,cc=1600,Doors=4, Gears=5, Quarterly_Tax=88))
summary(pred)
