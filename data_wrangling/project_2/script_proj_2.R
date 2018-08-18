
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
sum(is.na(df$age))
summary(df$age)

mean(df$age, na.rm = TRUE)
df$age <- replace_na(df$age, replace = "29.88113")
sum(df$age == 29.88113)


#could have used the median to populate missing NA's to avoid outliers having to great an effetc on the mean
#median less sensitive to outliers

df$age <- as.numeric(df$age)


## 3: Lifeboat ##
any(is.na(df$boat))
sum(is.na(df$boat))
summary(df$boat)

df$boat <- replace_na(df$boat, replace = "None")


## 4: Cabin ##
any(is.na(df$cabin))
sum(is.na(df$cabin))
summary(df$cabin)

df$has_cabin_number <- ifelse(is.na(df$cabin), 0, 1)


## Extra Cleaning ##
number <- as.numeric(str_extract(df$ticket, "[0-9]+"))
any(is.character(number))
letters <- (str_extract(df$ticket, "[aA-zZ]+"))
any(is.numeric(number))
table(letters)
class(letters)
hist(table(letters))

df$ticket <- number
any(is.character(df$ticket))

View(df)

## clean csv file ##
write.csv(df, "titanic_clean.csv")







