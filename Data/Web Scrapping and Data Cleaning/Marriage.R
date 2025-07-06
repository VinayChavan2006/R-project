ta2 <- read_html("https://en.wikipedia.org/wiki/Marriageable_age#By_country") %>% html_elements("table") %>% html_table()
ta2
africa <- ta2[[3]] %>% .[2:49,]
americas <- ta2[[4]] %>% .[2:37,]
asia <- ta2[[5]] %>% .[2:51,]
europe <- ta2[[6]] %>% .[2:44,]
oceania <- ta2[[7]] %>% .[2:16,]
cont <- list(africa,americas,asia,europe,oceania)
save(cont, file="MarriageableAge.Rdata")
