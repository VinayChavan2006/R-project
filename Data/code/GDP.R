### More prev year GDP data
library(rvest)
library(tidyverse)
library(dplyr)

# scraping the site
html <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_past_and_projected_GDP_(nominal)")

# extracting all the tables
tables <- html %>% html_table()

# choosing table4

tab1 <- tables[[4]]            
colnames(tab1)[1] = "Country"  # renaming column1 as Country

# choosing table5

tab2 <- tables[[5]]
colnames(tab2)[1] = "Country"  # renaming column1 as Country

# choosing table6

tab3 <- tables[[6]]
colnames(tab3)[1] = "Country"  # renaming column1 as Country

# joining tab1,tab2,tab3 such the columnn with same name is not repeated
data <- full_join(tab1,full_join(tab2,tab3,by = c("Country")),by = c("Country"))

# extracting first 26 columns
data <- data[,1:26]

# function to remove commas
clearCommas <- function(colm_data)
{
  cleaned <- gsub(",","",colm_data)
  return(cleaned)
}

# casting data as numeric
makeNumeric <- function(data)
{
  return(as.numeric(data))
}

# cleaning data
for(i in 2:26)
{
  data[ ,i] <- clearCommas(data %>% pull(i))
  data[ ,i] <- makeNumeric(data %>% pull(i))
}

# assigning the data to gdp_data
gdp_data <- data
