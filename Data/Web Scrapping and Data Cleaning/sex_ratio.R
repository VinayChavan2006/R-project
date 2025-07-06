library(rvest)
library(tidyverse)
library(dplyr)


# reading html
html <- read_html("https://en.wikipedia.org/wiki/List_of_sovereign_states_by_sex_ratio")

# extracting table
table <- html %>% html_table() 

# selecting 1st table from list of tables
sex_ratio_data <- table[[1]]

# function to cast into numeric
makeNumeric <- function(data)
{
  return(as.numeric(data))
}

for(i in 2:8)
{
  sex_ratio_data[ ,i] <- makeNumeric(sex_ratio_data %>% pull(i))
}

# dropping NA rows
sex_ratio_data <- sex_ratio_data %>% drop_na() %>% .[2:230,]
sex_ratio_data

#save(sex_ratio_data, file = "SexRatio.RData")
