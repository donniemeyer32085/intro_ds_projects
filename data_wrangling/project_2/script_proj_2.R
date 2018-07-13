
### Project: Data Wrangling Exercise 2: Dealing with missing values ###

## libraries ##
library("tidyverse")


## 0: Load the data in RStudio ##

df <- read_csv("/home/donniemeyer32085/git/springboard_projects/data_wrangling/project_2/titanic_original.csv")
glimpse(df)

#check to see any NA's
any(is.na(df))
#sum the NA's
sum(is.na(df))
#check to see if any rows have NA's
complete.cases(df)
#keep complete cases
df[complete.cases(df),]
#omit na's
na.omit(df)
#there are no complete cases in this data frame


## 1: Port of embarkation ##
is.na(df$embarked)
sum(is.na(df$embarked))
table(df$embarked, exclude = NULL)

df$embarked <- replace_na(df$embarked, replace = "S")


##  2: Age ##
any(is.na(df$age))
na_count <- sum(is.na(df$age))
summary(df$age)

mean(df$age, na.rm = TRUE)
df$age <- replace_na(df$age, replace = "29.88113")

replace_na_count <- sum(df$age == 29.88113)

identical(na_count, replace_na_count)
class(df$age)
as.integer(max(df$age))
#could have used the median to populate missing NA's