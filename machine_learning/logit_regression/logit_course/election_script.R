library(tidyverse)
library(mice)
library(corrplot)
setwd("/home/donniemeyer32085/git/springboard_projects/machine_learning/logit_regression/")

### Election forecasting: Predicting the winners before any votes are cast ###
poll <- read_csv("PollingData.csv")
poll_imp <- read_csv("PollingData_Imputed.csv")
glimpse(poll)

#missing state data, should be 150 obs.
table(poll$Year)

#Missing Values, Ramussen and SurveyUSA has misisng data
summary(poll)

#replacing NA's using the mouse package, first create new data frame
simple <- poll[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
glimpse(simple)
summary(simple)

#with multiple imputation, if you ran it twice, you would get different values each time, we need to set a seed
set.seed(85)
imputed <- complete(mice(simple))
summary(imputed)
poll$Rasmussen <- imputed$Rasmussen
poll$SurveyUSA <- imputed$SurveyUSA
summary(poll)
summary(poll_imp)

#a sophsiticated baseline method
train <- subset(poll, Year == 2004 | Year == 2008)
test <- subset(poll, Year == 2012)

#base model - the republican will win the state the majority of the time with a 53% accuracu rate
table(train$Republican)

#lets build a btter base line model with the rasmussen poll data
#sign function takes a vector and passes a 1 if its a positive number, -1 for a negative number, and zero for 0
sign(train$Rasmussen)
table(sign(train$Rasmussen))
#we see that 56 of the 100 obs, this model predicts that 56 states are won by republicans, 42 by dems, 2 whwere inconclusive

#best base line model
table(train$Republican, sign(train$Rasmussen))

#find correlation betwen variables - alot of variabples are correlated. Use PropR
cor(select(train, -State))
corrplot(cor(select(train, -State)))

#logistic regression
mod1 <- glm(Republican ~ PropR, family = "binomial", data = train)
summary(mod1)

#see how model performs on the training set
pred1 <- predict(mod1, type = "response")
table(train$Republican, pred1 >= 0.5) #TRUE means we predict republican, FALSE means we predicted democrat

#try a model that has low correlated variables, Survey USA and diff count are the lowest correlated
mod2 <- glm(Republican ~ SurveyUSA + DiffCount, family = "binomial", data = train)
summary(mod2)

pred2 <- predict(mod2, type = "response")
table(train$Republican, pred2 >= 0.5)






