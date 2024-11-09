library(rvest)
library(tidyverse)
library(dplyr)

# life expectancy data

# read html
html <- read_html("https://www.worldometers.info/demographics/life-expectancy/")

# extracting list of table
life_expectancy <- html %>% html_table() 

# selecting 1st table as tibble
life_expectancy_data <- life_expectancy[[1]]

# Changing the first Column name to Rank
colnames(life_expectancy_data)[1] = "Rank"

life_expectancy_data