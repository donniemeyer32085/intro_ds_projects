library(tidyverse)
library(broom)
library(ROCR)
library(caTools)
library(mice)
setwd("/home/donniemeyer32085/git/springboard_projects/machine_learning/logit_regression/logit_project/logistic_regression/dataSets")

## Regression with binary outcomes
## ═════════════════════════════════

## Logistic regression
## ───────────────────────

##   This far we have used the `lm' function to fit our regression models.
##   `lm' is great, but limited–in particular it only fits models for
##   continuous dependent variables. For categorical dependent variables we
##   can use the `glm()' function.

##   For these models we will use a different dataset, drawn from the
##   National Health Interview Survey. From the [CDC website]:

##         The National Health Interview Survey (NHIS) has monitored
##         the health of the nation since 1957. NHIS data on a broad
##         range of health topics are collected through personal
##         household interviews. For over 50 years, the U.S. Census
##         Bureau has been the data collection agent for the National
##         Health Interview Survey. Survey results have been
##         instrumental in providing data to track health status,
##         health care access, and progress toward achieving national
##         health objectives.

##   Load the National Health Interview Survey data:

NH11 <- readRDS("NatHealth2011.rds")
labs <- attributes(NH11)$labels

##   [CDC website] http://www.cdc.gov/nchs/nhis.htm

## Logistic regression example
## ───────────────────────────────

##   Let's predict the probability of being diagnosed with hypertension
##   based on age, sex, sleep, and bmi

str(NH11$hypev) # check stucture of hypev
levels(NH11$hypev) # check levels of hypev
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
              data=NH11, family="binomial")
coef(summary(hyp.out))

## Logistic regression coefficients
## ────────────────────────────────────

##   Generalized linear models use link functions, so raw coefficients are
##   difficult to interpret. For example, the age coefficient of .06 in the
##   previous model tells us that for every one unit increase in age, the
##   log odds of hypertension diagnosis increases by 0.06. Since most of us
##   are not used to thinking in log odds this is not too helpful!

##   One solution is to transform the coefficients to make them easier to
##   interpret
hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

## Generating predicted values
## ───────────────────────────────

##   In addition to transforming the log-odds produced by `glm' to odds, we
##   can use the `predict()' function to make direct statements about the
##   predictors in our model. For example, we can ask "How much more likely
##   is a 63 year old female to have hypertension compared to a 33 year old
##   female?".

# Create a dataset with predictors set at desired levels
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
# predict hypertension at those levels
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

##   This tells us that a 33 year old female has a 13% probability of
##   having been diagnosed with hypertension, while and 63 year old female
##   has a 48% probability of having been diagnosed.

## Packages for  computing and graphing predicted values
## ─────────────────────────────────────────────────────────

##   Instead of doing all this ourselves, we can use the effects package to
##   compute quantities of interest for us (cf. the Zelig package).

#library(effects)
#plot(allEffects(hyp.out))

## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).


#select only variables useful
small <- select(NH11, everwrk, age_p, r_maritl)
glimpse(small)

#check structure of everwrk
glimpse(small$everwrk) 
levels(small$everwrk) 
table(small$everwrk)
summary(small)

#impute missing values for everwrk
set.seed(85)
small <- complete(mice(small))
small$everwrk <- small$everwrk
summary(small)
table(small$everwrk)

#filter everwrk to include only 1 and 0 values, 1 = work and 0 = not work
small <- small %>% 
  separate(everwrk, c("everwrk", "B")) %>% 
  select(-B)  %>% 
  filter(everwrk %in% c("1","2")) %>% 
  mutate(everwrk = ifelse(everwrk == 2, 0, 1))
small$everwrk <- factor(small$everwrk)

small <- small %>% 
  separate(r_maritl, c("marital_status", "mar_desc")) %>% 
  select(-mar_desc)
small$marital_status <- factor(small$marital_status)

#Logistic Regression
ever_worked <- glm(everwrk ~ age_p + marital_status, family = "binomial", data = small)
summary(ever_worked)


##   2. Predict the probability of working for each level of marital
##      status.
table(small$marital_status)
levels(small$marital_status)

predDat2 <- with(small,
                expand.grid(age_p = mean(age_p),
                            marital_status = c("2", "4", "5", "6", "7", "8", "9")))

cbind(predDat2, predict(ever_worked, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat2))

#there was a 76% probablity that someone with marital status 2 has worked 

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.
