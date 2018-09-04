
#### Linear Regression Exercise Script ####

## set the working directory and libraries
library(tidyverse)
library(corrplot)
library(broom)
setwd("/home/donniemeyer32085/git/springboard_projects/machine_learning/linear_regression/linear_regression_project/dataSets")

## import data and get information about data
states.data <- readRDS("states.rds") 
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
states.data <- na.omit(as.tibble(states.data))



################################################# EXERCISE PART 1 ###############################################



######################################## 1. Examine/plot the data before fitting the model

#scatter plots - there are some large energy values, appears non-linear, negative relationship
states.data %>% 
  ggplot(aes(x = metro, y = energy)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)  

#boxplot -  there are some large energy values, appears non-linear, negative relationship, there is more variation
# in very low metro arears and very high metro areas
states.data %>% 
  ggplot(aes(x = cut(metro, breaks = 4), y = energy)) + 
  geom_boxplot()

#histograms - we again notice some large values on the dependent variable
states.data %>% 
  ggplot(aes(x = energy)) +
  geom_histogram(color = "black", binwidth = 40)  
states.data %>% 
  ggplot(aes(x = energy)) +
  geom_histogram(color = "black", binwidth = 40)  +
  xlim(c(500,900))
  
#correlation - a negative relationship between the two variables is present
subset_sd <- subset(states.data, select = c("energy", "metro"))
summary(subset_sd)
cor(subset_sd, use = "complete.obs") 


#################################### 2. Print and interpret the model `summary'

#simple linear regression model 
mod_sd <- lm(data=states.data, energy ~ metro)
summary(mod_sd)   

#######Model Interpretation
#A one percent increase in metro area popultion is associeted with a -1.6526 btu decrease in per capita energy consumption.
#The p-value idicates that there is a statitically significant reltionship between metro and energy consumption. 
#There is only a 1 percnt chance we will reject the null hypothesis of no correlation between X an Y, when in fact there is no correlation. 
#The residual mean is -21.64 indicating that are residuals are not normally distributed. This can lead to biased estimates
#The r-sqaured values are low indicating that less than 1 percent of the variation Y is explained by X
#The F-test concludes that the model fits the data better than a model with no independent variables. 
#The p-value for the F-Test is less than the chosen significance level of 0.05

###################################### 3. `plot' the model to look for deviations from modeling assumptions
#tidy model object data set
tidy_sd <- augment(mod_sd)
tidy_sd <- tidy_sd %>% 
  arrange(desc(abs(.resid)))
tidy_sd
par(mfrow=c(1,1))
ggplot(tidy_sd, aes(.cooksd)) + 
  geom_histogram(color = "black")

#diagnostic plots
par(mfrow=c(2,2))
plot(mod_sd)
#residual vs. fitted plot - there may be a non linear relationship between metro and energy.
#Normal Q-Q - this plot is showing that our residuals are not normally distributed
#Scale-location - there does not appear to be any heteroskedasciity present 
#Residual vs Leverage - there appears to points with high leveage as well as outliers. Points 48 and 16 in particlar


## 4. Remove Outliers - based on visual and diagnostic plot inspection
outliers <- states.data[c(16,41,48), ]
not_outliers <- states.data[-c(16,41,48), ]
not_outliers %>% 
  ggplot(aes(x = energy)) +
  geom_histogram(color = "black", binwidth = 40)
not_outliers %>% 
  ggplot(aes(metro, energy)) +
  geom_point()
not_outliers %>% 
  ggplot(aes(x = cut(metro, breaks = 4), y = energy)) + 
  geom_boxplot()

#  5. Model with no outliers
mod_O <- lm(energy ~ metro, data = not_outliers)
summary(mod_O)
par(mfrow=c(2,2))
plot(mod_O)
#After removing the three outliers the diagnostic plots meet the mark
#Outliers have significant effect on regression summary
#adjusted R2 improves significantley and the metro coefficient changes from -1.6526 to -1.3271 and is more significant. 
#from observing the residual vs fitted plot there is still  non - linear relationship present


## 6. Try adding a nonlinear componenet to model (use data with no outliers)
not_outliers$metro2 <- -(not_outliers$metro^2)
model_sd_nl <- lm(data=not_outliers, energy ~ metro + metro2)
summary(model_sd_nl)
#The model summary showes an improved F and R2 statististic, and both coeficients are significant
#This model appears to fit the data better

#create a data frame with predicted values and variable of interest for plotting
predicted_nl <- data.frame(energy_pred = predict(model_sd_nl, not_outliers), metro=not_outliers$metro)
predicted_nl
##Create a graph that plots energy vs. metro, plot non-linear line on top of graph to check fit
#within geom_line we supply the function a new data set, which contains energy_pred
#(https://stackoverflow.com/questions/15633714/adding-a-regression-line-on-a-ggplot)
not_outliers %>% 
  ggplot(aes(metro, energy)) +
  geom_point() +
  geom_line(color="green", data = predicted_nl, aes(x=metro, y=energy_pred))

#check for model assumptions
par(mfrow=c(2,2))
plot(model_sd_nl)
#Residuals vs Fiited plot now appears to show that with have fixed the non linear relationship. 
#All other plots meet the conditions that satistfy linear regression assumptions



##################################### EXERCISE PART 2 ############################################################


##   Select one or more additional predictors to add to your model and
sd_numeric_energy <- na.omit(select(not_outliers, energy, everything(), -region, -state))
par(mfrow=c(1,1))
corrplot(cor(sd_numeric_energy))
cor(sd_numeric_energy)
#variables that are highly corrleated with energy are (toxic, green, miles, house, senate, percent, income)
#lowly correlated with metro are (toxic, house, senate, green)

## 1.   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?
not_outliers%>% 
  ggplot(aes(x = toxic, y = energy)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() + scale_y_log10()

not_outliers %>% 
  ggplot(aes(x = green, y = energy)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() + scale_y_log10()

not_outliers %>% 
  ggplot(aes(x = house, y = energy)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) 

not_outliers %>% 
  ggplot(aes(x = senate, y = energy)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) 

#correlations for second model
subset <- subset(not_outliers, select = c("region", "energy", "metro", "metro2", "toxic", "green", "senate", "house"))
summary(subset)


# 2. linear model with more variables
mod_sub <- lm(data=subset, energy ~ metro + metro2 + toxic + green)
summary(mod_sub)

#check model assumptions
par(mfrow=c(2,2))
plot(mod_sub)
par(mfrow=c(1,1))
hist(mod_sub$residuals)

#anova of two linear models - this model fits the data better according to our anova test
anova(model_sd_nl, mod_sub)

#remove oulier
mod_sub_no <- lm(data=subset[-40,], energy ~ metro + metro2 + toxic + green)
summary(mod_sub_no)

#check model assumptions
par(mfrow=c(2,2))
plot(mod_sub_no)
par(mfrow=c(1,1))
hist(mod_sub_no$residuals)

#try a log log model without metro. Metro is probably correlated with green or toxic. Problem of endogeniety
mod_log<- lm(data=subset, log(energy) ~ log(toxic) + log(green))
summary(mod_log)
par(mfrow=c(2,2))
plot(mod_log)
par(mfrow=c(1,1))
hist(mod_log$residuals)


################################################# EXERCISE PART 2 ###############################################

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

# convert region to a factor variable
subset$region <- factor(subset$region)


#log log model from part 
mod_log_int <- lm(data=subset, log(energy) ~ log(toxic) + log(green)*log(toxic))
summary(mod_log_int)
plot(mod_log_int)
hist(mod_log_int$residuals)



##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

# plots -  there appears to be variation accros regions
subset %>% 
ggplot(aes(region, energy, fill = region)) +
  geom_col()
subset %>% 
  ggplot(aes(region, energy, color = region)) +
  geom_boxplot()

# model with region variable
mod_sub_region <- lm(data=subset, log(energy) ~ log(toxic) + log(green)*log(toxic) + region)
summary(mod_sub_region)

#model with different coding scheme
mod_sub_region <- lm(data=subset, log(energy) ~ log(toxic) + C(region, contr.helmert) + log(green)*log(toxic))
summary(mod_sub_region)
# model with different coding scheme - when the coding scheme is changed to contr.helmert, 
# the region variable becomes highly significant.








