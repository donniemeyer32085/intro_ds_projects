library(tidyverse)
setwd("/home/donniemeyer32085/git/springboard_projects/machine_learning/linear_regression/")

##Playing Moneyball in the NBA##

#import data sets
test <- read_csv("NBA_test.csv")
train <- read_csv("NBA_train.csv")
glimpse(test)
glimpse(train)

#table
table(train$W, train$Playoffs)

#difference between points scored and points allowed
train <- train %>% 
  mutate(points_diff = PTS - oppPTS) %>% 
  select(SeasonEnd:oppPTS, points_diff, everything())
train

#scatter plot between W and points_diff
train %>% ggplot(aes(W, points_diff)) + 
  geom_jitter() + 
  geom_smooth(method = "lm", se = FALSE)

#create linear model
pred_W <- lm(W ~ points_diff, data = train)
summary(pred_W)

#swe want to win 42 or more games

# 42 >= 4.100e+01 + 3.259e-02 * points_diff
# points_diff = 30.67

points_reg <- lm(PTS ~ `2PA`+ `3PA` + FTA + AST + ORB + DRB + TOV + STL + BLK, data = train)
summary(points_reg)
points_reg$residuals
SSE <- sum(points_reg$residuals^2)
SSE
RMSE = sqrt(SSE/nrow(train))
RMSE

#remove varaibles that are insignificant
points_reg_2 <- lm(PTS ~ `2PA`+ `3PA` + FTA + AST + ORB + DRB  + STL + BLK, data = train)
summary(points_reg_2)

points_reg_3 <- lm(PTS ~ `2PA`+ `3PA` + FTA + AST + ORB + STL + BLK, data = train)
summary(points_reg_3)

points_reg_4 <- lm(PTS ~ `2PA`+ `3PA` + FTA + AST + ORB + STL, data = train)
summary(points_reg_4)

SSE_4 <- sum(points_reg_4$residuals^2)
SSE_4
RMSE_4 <- sqrt(SSE_4/nrow(train))
RMSE_4

#use test data and make predictions

points_predic <- predict(points_reg_4, data = test)
length(points_predic)
length(test$PTS)

#compute out of sample R^2
SSE <- sum((points_predic - test$PTS)^2)
SST <- sum((mean(train$PTS)- test$PTS)^2)
