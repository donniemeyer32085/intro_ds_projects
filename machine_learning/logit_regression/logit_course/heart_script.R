library(tidyverse)
library(broom)
library(ROCR)
library(caTools)
setwd("/home/donniemeyer32085/git/springboard_projects/machine_learning/logit_regression/")
  
### The Framingham Heart Study: Evaluating risk factors to save lives ###

fram <- read_csv("framingham.csv")
glimpse(fram)

#split data into traing set and test se
set.seed(1000)
split <- sample.split(fram$TenYearCHD, SplitRatio = 0.65)
train <- subset(fram, split == TRUE)
test <- subset(fram, split == FALSE)

#glm model
fram_log <- glm(TenYearCHD ~ ., data = train, family = binomial)
summary(fram_log)

#use fram_log to make predictions on the test set
predict_test <- predict(fram_log, type = "response", newdata = test)

#creat confusion matrix, the model rarly predicts a 10 year chd above 50 % 
table(test$TenYearCHD, predict_test > 0.5)

#accuracy of the model at threshold of 0.5
(1069+11)/(1069+6+187+11)

#simple base line method
(1069+6)/(1069+6+187+11)

#lets compute the out of sample AUC
ROCRpred <- prediction(predict_test, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)

