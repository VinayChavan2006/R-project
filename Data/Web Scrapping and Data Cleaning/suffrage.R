ta3 <- read_html("https://en.wikipedia.org/wiki/Women%27s_suffrage") %>% html_table()
t4<-ta3[[9]]
t4

save(t4, file = "suffrage.Rdata")
