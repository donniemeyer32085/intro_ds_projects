#  Introduction
## ══════════════

#   • Learning objectives:
##     • Learn the R formula interface
##     • Specify factor contrasts to test specific hypotheses
##     • Perform model comparisons
##     • Run and interpret variety of regression models in R

##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

# set the working directory and libraries
library(tidyverse)
library(corrplot)
setwd("/home/donniemeyer32085/git/springboard_projects/machine_learning/linear_regression/linear_regression_project/dataSets")
getwd() 
list.files() 

#import data and get information about data
states.data <- readRDS("states.rds") 
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
states.info

## Linear regression
## Examine the data before fitting models
##   Start by examining the data to check for problems.
# summary of expense and csat columns, all rows
#sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
#summary(sts.ex.sat)

# correlation between expense and csat
#cor(sts.ex.sat)
#sd_numeric <- na.omit(select(states.data, csat, everything(), -region, -state))
#corrplot(cor(sd_numeric))

## Plot the data before fitting models
##   Plot the data to look for multivariate outliers, non-linear relationships etc.
# scatter plot of expense vs csat
#ggplot(states.data, aes(expense, csat)) + geom_point() + geom_smooth()
#plot(sts.ex.sat)

## Linear regression example
##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
#sat.mod <- lm(csat ~ expense, data=states.data) 
#summary(sat.mod) 

## Why is the association between expense and SAT scores /negative/?
##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

#plot percent vs csat
#ggplot(states.data, aes(percent, csat)) + geom_point() + geom_smooth() + scale_x_log10()

#sat_2 <- lm(csat ~ expense + percent, data = states.data)
#summary(sat_2)

#sat_3 <- lm(csat ~ expense + log(percent), data = states.data)
#summary(sat_3)
#plot(sat_3)

## The lm class and methods
##   OK, we fit our model. Now what?
##   • Examine the model object:
#class(sat.mod)
#names(sat.mod)
#sat.mod$coefficients
#sat.mod$residuals
#summary(sat.mod$fitted.values)
#plot(sat.mod$fitted.values, sat.mod$residuals)
#methods(class = class(sat.mod))[1:9]

##   • Use function methods to get more information about the fit
#confint(sat.mod) 
#confint((sat_2))
# B1 is the amount that we expect Y(csat) to increase when we increase X(expense) by one unit. 
#So if we collected new data we are 95% confident that the expense variable would fall beteween  -0.03440768  & -0.01014361.
#In other words 19 out of 20 times we collected the data we would get a B1 falling within this interval

#are the models residuals well behaved?
#hist(residuals(sat.mod))
#hist(residuals(sat_2))


## Linear Regression Assumptions
##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

#assumptions of OLS linear regression model
#http://statisticsbyjim.com/regression/ols-linear-regression-assumptions/

##Investigate these assumptions visually by plotting your model:
#par(mfrow=c(2,2))
#plot(sat.mod)

#Understanding diagnostic plots https://data.library.virginia.edu/diagnostic-plots/

#residuals vs. fitted plots (https://onlinecourses.science.psu.edu/stat501/node/277/)

#The plot is used to detect non-linearity, unequal error variances, and outliers.
#The residuals "bounce randomly" around the 0 line. This suggests that the assumption that the relationship is linear is reasonable.
#The residuals roughly form a "horizontal band" around the 0 line. This suggests that the variances of the error terms are equal.
#No one residual "stands out" from the basic random pattern of residuals. This suggests that there are no outliers.

#qqplot  (https://data.library.virginia.edu/understanding-q-q-plots/)


## Comparing models

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
#sat.voting.mod <-  lm(csat ~ expense + house + senate, data = na.omit(states.data))
#summary(sat.voting.mod)

#sat.mod <- update(sat.mod, data=na.omit(states.data))
#summary(sat.mod)

# compare using the anova() function
#if the more complex model fits the data better, we should get a Pr(>F) < 0.05 to justify using the 
#more complex model. Here we have a value of 0.06486 so we should stick with the simpler model
#anova(sat.mod, sat.voting.mod)
#coef(summary(sat.voting.mod))


## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to



## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

#Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income, data=states.data) 
summary(sat.expense.by.percent)
#Show the results
coef(summary(sat.expense.by.percent)) # show regression coefficients table

## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region, data=states.data) 
summary(sat.region)
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4), data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert), data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?
