library(rvest)
library(tidyverse)
library(dplyr)

# reading html
html <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)")

# extract the tables
gdp_nominal <- html %>% html_table()

# selecting 3rd table
gdp_nominal <- gdp_nominal[[3]]

# drop row 1
gdp_nominal <- gdp_nominal[2:nrow(gdp_nominal) ,] 

# names vector for column names
names <- c("Estimate(2024)","Year(2024)","Estimate(2023)","Year(2023)","Estimate(2022)","Year(2022)")
for(i in 2:7)
{
  colnames(gdp_nominal)[i] <- names[i-1]
}

# function to clean Years Data (it has some of data like "[2][3]2024" to 2024)
cleanYears <- function(year_data)
{
  return(sapply(year_data,function(year)substring(year,nchar(year)-3,nchar(year))))
}

# cleaning Years Data
gdp_nominal$`Year(2024)` <- cleanYears(gdp_nominal$`Year(2024)`)
gdp_nominal$`Year(2023)` <- cleanYears(gdp_nominal$`Year(2023)`)
gdp_nominal$`Year(2024)` <- cleanYears(gdp_nominal$`Year(2022)`)

# function to remove commas
clearCommas <- function(colm_data)
{
  cleaned <- gsub(",","",colm_data)
  return(cleaned)
}

# cleaning Estimate Columns by clearing commas
gdp_nominal$`Estimate(2024)` <- clearCommas(gdp_nominal$`Estimate(2024)`)
gdp_nominal$`Estimate(2023)` <- clearCommas(gdp_nominal$`Estimate(2023)`)
gdp_nominal$`Estimate(2022)` <- clearCommas(gdp_nominal$`Estimate(2022)`)

# casting data as numeric
makeNumeric <- function(data)
{
  return(as.numeric(data))
}

for(i in 2:7)
{
  gdp_nominal[ ,i] <- makeNumeric(gdp_nominal %>% pull(i))
}

# sorting data wrt Countries
gdp_nominal <- gdp_nominal %>% arrange(World)

# dropping rows with NA
gdp_nominal <- gdp_nominal %>% drop_na()
gdp_nominal
