
#### Linear Regression Exercise Script ####

## set the working directory and libraries
library(tidyverse)
library(corrplot)
setwd("/home/donniemeyer32085/git/springboard_projects/machine_learning/linear_regression/linear_regression_project/dataSets")

## import data and get information about data
states.data <- readRDS("states.rds") 
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
states.data <- na.omit(as.tibble(states.data))



## 1. Examine/plot the data before fitting the model

#scatter plots
states.data %>% 
  ggplot(aes(x = metro, y = energy)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)  #there are some large energy values, appears non-linear, negative relationship
states.data %>% 
  ggplot(aes(x = cut(metro, breaks = 4), y = energy)) + 
  geom_boxplot()
#histograms
states.data %>% 
  ggplot(aes(x = energy)) +
  geom_histogram(color = "black", binwidth = 30)  #we again notice some large values on the dependent variable
#correlation
subset_sd <- subset(states.data, select = c("energy", "metro"))
summary(subset_sd)
cor(subset_sd, use = "complete.obs") # a negative relationship between the two variables is present



## 2. Print and interpret the model `summary'
model_sd <- lm(data=states.data, energy ~ metro)
summary(model_sd)   
SSE_model_sd <- sum(model_sd$residuals^2)
SSE_model_sd
RMSE_model_sd = sqrt(SSE_model_sd/nrow(states.data))
RMSE_model_sd

##Try a non linear model ()
states.data$metro2 <- -(states.data$metro^2)
model_sd_nl <- lm(data=states.data, energy ~ metro + metro2)
summary(model_sd_nl)
SSE_model_sd_nl <- sum(model_sd$residuals^2)
SSE_model_sd_nl
RMSE_model_sd_nl = sqrt(SSE_model_sd/nrow(states.data))
RMSE_model_sd_nl
#check for model assumptions
par(mfrow=c(2,2))
plot(model_sd_nl)
par(mfrow=c(1,1))
hist(model_sd_nl$residuals)
#create a data frame with predicted values and variable of interest for plotting
predicted_nl <- data.frame(energy_pred = predict(model_sd_nl, states.data), metro=states.data$metro)
predicted_nl
##Create a graph that plots energy vs. metro, plot non-linear line on top of graph to check fit
#within geom_line we supply the function a new data set, which contains energy_pred
#(https://stackoverflow.com/questions/15633714/adding-a-regression-line-on-a-ggplot)
states.data %>% 
  ggplot(aes(metro, energy)) +
  geom_point(color = "blue") +
  geom_line(color='red', data = predicted_nl, aes(x=metro, y=energy_pred))


## 3. `plot' the model to look for deviations from modeling assumptions
par(mfrow=c(2,2))
plot(model_sd)
par(mfrow=c(1,1))
hist(model_sd$residuals)



##   Select one or more additional predictors to add to your model and
sd_numeric_energy <- na.omit(select(states.data, energy, everything(), -region, -state))
par(mfrow=c(1,1))
corrplot(cor(sd_numeric_energy))
cor(sd_numeric_energy)
#variables that are highly corrleated with energy are (toxic, green, miles, house, senate, percent, income)
#lowly correlated with metro are (toxic, house, senate, green)


## 1.   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?
states.data %>% 
  ggplot(aes(x = toxic, y = energy)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) 
states.data %>% 
  ggplot(aes(x = green, y = energy)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) 
states.data %>% 
  ggplot(aes(x = house, y = energy)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) 
states.data %>% 
  ggplot(aes(x = senate, y = energy)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) 
#correlations for second model
subset_sd_2 <- subset(states.data, select = c("energy", "metro", "toxic", "green", "senate", "house"))
summary(subset_sd_2)
corrplot(cor(subset_sd_2), use = "complete.obs")
cor(subset_sd_2, use = "complete.obs")


# 2. linear model with more variables
model_sd_2 <- lm(data=states.data, energy ~ metro  + toxic + house + senate + green)
summary(model_sd_2)
SSE_model_sd_2 <- sum(model_sd_2$residuals^2)
SSE_model_sd_2
RMSE_model_sd_2 = sqrt(SSE_model_sd_2/nrow(states.data))
RMSE_model_sd_2
#check model assumptions
par(mfrow=c(2,2))
plot(model_sd_2)
par(mfrow=c(1,1))
hist(model_sd_2$residuals)
#anova of two linear models
anova(model_sd, model_sd_2)

#adding polynomial to linear model with more variables
model_sd_2_nl <- lm(data=states.data, energy ~ metro + metro2 + toxic + house + senate + green)
summary(model_sd_2_nl)
SSE_model_sd_2_nl <- sum(model_sd_2_nl$residuals^2)
SSE_model_sd_2_nl
RMSE_model_sd_2_nl = sqrt(SSE_model_sd_2_nl/nrow(states.data))
RMSE_model_sd_2_nl
#anova of models that include a polynomial
anova(model_sd_nl, model_sd_2_nl)


# 3. check model assumptions
par(mfrow=c(2,2))
plot(model_sd_2_nl)
par(mfrow=c(1,1))
hist(model_sd_2_nl$residuals)

#simple model
model_simple <- lm(data=states.data, energy ~ toxic + green)
summary(model_simple)par(mfrow=c(2,2))
plot(model_sd_2_nl)
par(mfrow=c(1,1))
hist(model_sd_2_nl$residuals)
anova(model_simple, model_sd_2_nl)


