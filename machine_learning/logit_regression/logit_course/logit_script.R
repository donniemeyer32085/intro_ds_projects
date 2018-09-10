library(tidyverse)
library(broom)
library(ROCR)
setwd("/home/donniemeyer32085/git/springboard_projects/machine_learning/logit_regression/")

### LOGIT REGRSSSION NOTES ###

quality <- read_csv("quality.csv")
quality <- na.omit(as.tibble(quality))
quality.info <- data.frame(attributes(quality)[c("names")])
View(quality)
glimpse(quality)

#scatter plot of two independent variables
quality %>% 
  ggplot(aes(x = OfficeVisits, y = Narcotics, color = PoorCare)) +
  geom_jitter()

#table function for PoorCare
table(quality$PoorCare)

#creat new variable care
quality <- quality %>% 
mutate(Care = ifelse(PoorCare == 1, "Bad", "Good"))
glimpse(quality)

#barof poorCare
  quality %>% 
    ggplot(aes(x = Care, fill = factor(Care))) +
    geom_bar() +
    guides(fill=FALSE) 
  
#base line method, predict frequent outcome for all observations, predict that all pateints are receiving good care
#base line method has an accuracy of about 75 %
98/(98+33)

#we need to randomally split our data set into a traing and test data set
#install package "caTools install.packages("caTools"), this package can randomly split your data
library(caTools)
set.seed(88)
#sample.split first argument dependet variable, and second argument percentage of data we want in the traing set
split <- sample.split(quality$PoorCare, SplitRatio = 0.75)
split

#TRUE means we should put that observation in the training set, and FALSE means we should put that observation in the test set
quality_train <- subset(quality, split == TRUE)
quality_test <- subset(quality, split == FALSE)

#logistic regression model
quality_log <- glm(PoorCare ~ OfficeVisits + Narcotics, data = quality_train, family = binomial)
summary(quality_log)
augment(quality_log, type = )

#the coefficients are positive indicating higher values of these two variables are indicitive of poor care
#AIC is a measure of quality in the model, similar to R^2, takes into account number variables used vs the size of the data set
#the best model is the AIC with the lowest value

#now lets use the model on the training set
predict_train <- predict(quality_log, data = quality_train, type = "response")
summary(predict_train)

#average 
tapply(predict_train, quality_train$PoorCare, mean)

#threshold value and confusion matrices, classification tables, specificty and sensitivity
table(quality_train$PoorCare, predict_train > 0.5)

#specificty, the true negative rate and sensitivity, the true positve rate
10/25 #sensitivity
70/74 #specificity

#increasing the threshold increased specificity and decreased sensitivity
table(quality_train$PoorCare, predict_train > 0.7)
8/25
73/74

#decreasing the threshold decreased specificity and increased sensitivity
table(quality_train$PoorCare, predict_train > 0.2)
8/25
54/74


#ROC curves
rocr_pred <- prediction(predict_train, quality_train$PoorCare)
rocr_perf <- performance(rocr_pred, "tpr", "fpr")
plot(rocr_perf)
plot(rocr_perf, colorize = TRUE)
plot(rocr_perf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))

#compute out of sample metrics on the test data set
predict_test <- predict(quality_log, type = "response", newdata = quality_test)
summary(predict_test)
summary(predict_train)
table(quality_test$PoorCare, predict_test > 0.3)

19/24
6/8

(.8+.75)/2

rocr_pred_2 <- prediction(predict_test, quality_test$PoorCare)
rocr_perf_2 <- performance(rocr_pred_2, "tpr", "fpr")
plot(rocr_perf_2, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))




