
#Project: Data Wrangling Exercise 1: Basic Data Manipulation

#libraries
library(tidyverse)

#read in data
df <- read_csv("/home/donniemeyer32085/git/springboard_projects/refine_original.csv")
View(df)
glimpse(df)

## string manipulation on company column ##
table(df$company)

#make all strings lower case
df <- df %>%
  mutate(company = tolower(company)) 

#philips
df$company <- str_replace(df$company, "^p.*s$", "philips")
df$company <- str_replace(df$company, "^fillips", "philips")

#akzo
df$company <- str_replace(df$company, "ak zo", "akzo")
df$company <- str_replace(df$company, "akz0", "akzo")

#unilever
df$company <- str_replace(df$company, "unilver", "unilever")

table(df$company)

#Separate product code and number
df <- separate(df, `Product code / number`, into = c("product_code", "product_number"), sep = "-")

#create new column for product type, use grepl

df$product_type <- ifelse(grepl("p", df$product_code), "Smartphone", 0)
df$product_type <- ifelse(grepl("v", df$product_code), "TV", df$product_type)
df$product_type <- ifelse(grepl("x", df$product_code), "Laptop", df$product_type)
df$product_type <- ifelse(grepl("q", df$product_code), "Tablet", df$product_type)



