## Continent Data
library(tidyverse)
library(rvest)
library(dplyr)

# read html
html <- read_html("https://statisticstimes.com/geography/countries-by-continents.php")

# extract table
dat <- html %>% html_table()

# Continent Data
continent_data <- dat[[3]] %>% rename(Country = `Country or Area`) %>% select(Country,Continent)
