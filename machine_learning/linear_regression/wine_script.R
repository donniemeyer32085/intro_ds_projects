library(tidyverse)
setwd("/home/donniemeyer32085/git/springboard_projects/machine_learning/")

### Linear Regression Project ###

#import data
wine <- read_csv("wine.csv")
wine_test <- read_csv("wine_test.csv")

str(wine)
summary(wine)

#scatter plot price(y) and age(x)
wine %>% 
  ggplot(aes(x = Age, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + scale_y_log10()
wine %>% 
  ggplot(aes(x = AGST, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + scale_y_log10()
wine %>% 
  ggplot(aes(x = WinterRain, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + scale_y_log10()
wine %>% 
  ggplot(aes(x = HarvestRain, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + scale_y_log10()

#linear models
lm_AGST <- lm(Price ~ AGST, data = wine)
summary(lm_AGST)
lm_AGST$residuals
SSE_AGST <- sum(lm_AGST$residuals^2)
SSE_AGST

MR_wine <- lm(Price ~ AGST + HarvestRain, data = wine)
summary(MR_wine)
SSE_MR <- sum(MR_wine$residuals^2)
SSE_MR

MR2_wine <- lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)
summary(MR2_wine)
SSE_MR2 <- sum(MR2_wine$residuals^2)
SSE_MR2

#remove france population from model (insignicant)
#now that we have removed frances population we see that more variables became significant
#this is due to multicolinarity, francepop and age where highly correlated
MR3_wine <- lm(Price ~ AGST + HarvestRain + WinterRain + Age, data = wine)
summary(MR3_wine)
SSE_MR3 <- sum(MR3_wine$residuals^2)
SSE_MR3

#this graph shows the multicolinarity between the two independent variables
#very strong negative relationship -0.99
wine %>% 
  ggplot(aes(x = Age, y = FrancePop)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + scale_y_log10()

#the correlation between these two vars is -0.06
wine %>% 
  ggplot(aes(x = AGST, y = HarvestRain)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#lets compute the correlation using r
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)

#calculate correltion on data set wine
cor(wine)

#when removing variables from a model, we only want to remove one at a time
#lets seee what happens when we remove more thhan one variable
#removing as well gives a worse model
MR4_wine <- lm(Price ~ AGST + HarvestRain + WinterRain, data = wine)
summary(MR4_wine)
SSE_MR4 <- sum(MR4_wine$residuals^2)
SSE_MR4

#for the rest of the lesson, we will use MR3_wine as our best model
summary(MR3_wine)
SSE_MR3 <- sum(MR3_wine$residuals^2)
SSE_MR3

#we want a model that performs well on data it has never seen before.
#the data used to build the model is called the training data
#the new data is called the test data
#accuracy of model on test data is often called out-of-sample accuracy

#we will the wine test data using the predict function
glimpse(wine_test)
predictTest <- predict(MR3_wine, newdata = wine_test)
predictTest

#lets compute the R^2 value for our test set
SSE_wine_test <- sum((wine_test$Price - predictTest)^2)
SST_wine_test <- sum((wine_test$Price - mean(wine$Price))^2)
r_sqaured_predict <- 1 - SSE_wine_test/SST_wine_test
r_sqaured_predict

#the best model has the highest R2 on both the in sample and out sample data sets which was MR3_wine