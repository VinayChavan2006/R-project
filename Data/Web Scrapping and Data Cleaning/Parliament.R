library(rvest)
library(tidyverse)

ta1 <- read_html("http://archive.ipu.org/wmn-e/classif.htm") %>% html_elements("table") %>% html_table()
ta1
t1 <- ta1[[4]] %>% .[3:196,]

#getwd()
#save(t1, file = "WomeninPower.Rdata")
