##Please set your working directory where your data file exists##

setwd("D:/Works/Upworks/D")

#install.packages(c("ggplot2", "dplyr", "broom", "ggpubr", "readr", "tidyverse"))
#install.packages("e1071")
## Library to be used ##
library(stats)
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(readr)# reader will be applied
library(e1071)
#library(tidyverse)



## we have made a single dataset by appending the two data set and there is variable called status- underdeveoped (1) and developed (2)

## Definition of DATA:

# We are dealing with two variables- access to clean fuel and access to electricity. We can summarize to look into the data.

## Import Data Files ##

data <- read_csv("Data.csv", col_names = TRUE)

## Descriptive Analysis # 2.	(a) Analysis: Do a comprehensive descriptive statistical analysis for the following. Mean, median, mode, standard deviation, skewness, and kurtosis on the data

data$status <-  factor(data$Status, levels=c(1,2), labels=c("Underdeveloped", "Developed"))


summary(data$Access_to_Clean_Fuels_for_cooking)
summary(data$Access_to_electricity)

#Tables

attach(data)

# Access to Clean Fuel 
summarise(data, mean(Access_to_Clean_Fuels_for_cooking))

table(data$status)
prop.table(table(data$status))*100
xtabs(~status+data$`Country Name`, data)

pie(table(data$status), main="% of Developed and Underdeveloped countries")




data %>% group_by(status) %>%
  summarise(mean(Access_to_Clean_Fuels_for_cooking), 
            median(Access_to_Clean_Fuels_for_cooking),
            sd(Access_to_Clean_Fuels_for_cooking))


data %>% group_by(status="Developed", Year) %>%
  summarise(mean(Access_to_Clean_Fuels_for_cooking), 
            sd(Access_to_Clean_Fuels_for_cooking), 
            median(Access_to_Clean_Fuels_for_cooking))

data %>%  group_by(status=="Underdeveloped", Year) %>%
  summarise(mean(Access_to_Clean_Fuels_for_cooking), 
            sd(Access_to_Clean_Fuels_for_cooking), 
            median(Access_to_Clean_Fuels_for_cooking))

data %>%  group_by(status, Year) %>%
  summarise(mean(Access_to_Clean_Fuels_for_cooking), 
            median(Access_to_Clean_Fuels_for_cooking))


skewness(data$Access_to_Clean_Fuels_for_cooking)
kurtosis(data$Access_to_Clean_Fuels_for_cooking)

# Make a plot to compare

hist (data$Access_to_Clean_Fuels_for_cooking)
shapiro.test(data$Access_to_Clean_Fuels_for_cooking)

ggplot(data, aes(x=Access_to_Clean_Fuels_for_cooking, fill=status, color=status))+ 
  geom_histogram(alpha = 0.5, position = "identity")+
  guides(fill = guide_legend(title = "By Country Status"),
         colour = guide_legend(title = "By Country Status"))


cor.test(data$Access_to_Clean_Fuels_for_cooking, data$Access_to_electricity, method="pearson")
cor.test(data$Access_to_Clean_Fuels_for_cooking, data$Renewable_energy_consumption, method="pearson")

## ANOVA: how a quantitative dependent variable changes according to the levels of one or more categorical independent variables

## ANOVA tests whether there is a difference in means of the groups at each level of the independent variable.

# Null Hypothesis: no difference in mean (access to clean fuel)

anova1 <- aov(Access_to_Clean_Fuels_for_cooking ~ status, data=data)
summary(anova1)

#The larger the F value, the more likely it is that the variation caused by the independent variable is real and not due to chance.
#P-value confirms that the difference between the developed and underdeveloped countries for access to clean fuel is real. The country status has real impact on the access to clean fuel


## Access to clean fuels for cooking is highly correlated with the Access to Electricity and p-value is much lower than 0.0000. So, true correlation is not equal to 0. First of all, correlation ranges from -1 to 1.


library("ggpubr")
ggscatter(data, x = "Access_to_electricity", y = "Access_to_Clean_Fuels_for_cooking", 
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", color="grey",
          xlab = "Access to Electricity", ylab = "Access to Clean Fuels for cooking")

# Shapiro-Wilk normality test for Access to Clean Fuel

shapiro.test(c(data$Access_to_Clean_Fuels_for_cooking, data$Total_final_energy_consumption))

# So Access to clean fuel is normally distributed because p-value is less the alpha (0.05). That means we can run regression



#Access to electricity

summary(data$Access_to_electricity)

hist (data$Access_to_electricity)
ggplot(data, aes(x=Access_to_electricity, fill=status, color=status))+ 
  geom_histogram(alpha = 0.5, position = "identity")+
  guides(fill = guide_legend(title = "By Country Status"),
         colour = guide_legend(title = "By Country Status"))
#Total Final Energy Consumption

summary(data$Total_final_energy_consumption)



## Regression Equation-1: Access to clean Fuels 

model1 <- lm(data$Access_to_Clean_Fuels_for_cooking~data$Total_electricity_output+data$status, data=data)
summary(model1)

# 66% of the variation in dependent variable is explained by the model. We can include the other variable in the model. 
#Developed countries have higher access to the clean fuels compared to underdeveloped countries


## Regression Equation-2: Access to Electricity

model2 <- lm(data$Access_to_electricity~data$Total_electricity_output+data$status, data=data)
summary(model2)

# R-square value: 58% variation in the dependent variable is explained by model


model3 <- lm(data$Total_final_energy_consumption~data$Total_electricity_output
             +data$Access_to_Clean_Fuels_for_cooking
             +data$status
             +data$Renewable_energy_consumption)
summary(model3)


## total electricity output, renewable consumption and being a developed country have a significant association with total energy consumption. 
## The Model 3 representing 96% variation. 
## 


## Time series Plotting

plot(data$Access_to_Clean_Fuels_for_cooking~data$Year)

ggplot(data, aes(data$Year, data$Access_to_Clean_Fuels_for_cooking)) +
  geom_point(aes(color=status))


## We use time series regression for forecasting about future

#https://ds4ps.org/pe4ps-textbook/docs/p-020-time-series.html#fig:f11
access_elect.ts <- ts(data$Access_to_electricity, start=2000, end=2014, frequency=1)
plot(access_elect.ts)
