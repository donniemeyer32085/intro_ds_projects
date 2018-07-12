
### Project: Data Wrangling Exercise 1: Basic Data Manipulation ###

## libraries ##
library(tidyverse)

## Read in Data ##
df <- read_csv("/home/donniemeyer32085/git/springboard_projects/data_wrangling/refine_original.csv")
View(df)
glimpse(df)


## String Manipulation on Company Column ##
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


## Separate Product Code and Number ##
df <- separate(df, `Product code / number`, into = c("product_code", "product_number"), sep = "-")

#create new column for product type, use grepl
df$product_type <- ifelse(grepl("p", df$product_code), "Smartphone", 0)
df$product_type <- ifelse(grepl("v", df$product_code), "TV", df$product_type)
df$product_type <- ifelse(grepl("x", df$product_code), "Laptop", df$product_type)
df$product_type <- ifelse(grepl("q", df$product_code), "Tablet", df$product_type)


## Geocoding Addresses ##
df$country <- str_replace(df$country, "the netherlands", "nethherlands")

#moving the number to the left side of the street name
number <- as.numeric(str_extract(df$address, "[0-9]+"))
street <- (str_extract(df$address, "[aA-zZ]+"))
address <- cbind(number, street)
class(address)
address <- as.tibble(address)
df$address <- NULL
df <- cbind(df, address)
df <- unite (df, address, c("number", "street"), sep = " ")

df <- df %>% 
  select(company, product_code, product_number, address, city, country, name, product_type) %>% 
  unite(full_address, c("address", "city", "country"), sep = ", ")


## Create Binary Varaibles for Company##
df$company_philips <- ifelse(grepl("philips", df$company), 1, 0)
df$company_akzo <- ifelse(grepl("akzo", df$company), 1, 0)
df$company_van_houten <- ifelse(grepl("van houten", df$company), 1, 0)
df$company_unilever <- ifelse(grepl("unilever", df$company), 1, 0)


## Create Binary Varaibles for Product##
df$product_smartphone <- ifelse(grepl("Smartphone", df$product_type), 1, 0)
df$product_tv <- ifelse(grepl("TV", df$product_type), 1, 0)
df$product_laptop <- ifelse(grepl("Laptop", df$product_type), 1, 0)
df$product_tablet <- ifelse(grepl("Tablet", df$product_type), 1, 0)



#remove unessesary objects in environment
rm(address)
rm(number)
rm(street)



#Final Cleaned Data Set
View(df)


## Export csv file ##
write.csv(df, "refine_clean.csv")






