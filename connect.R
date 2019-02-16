library(httr)

con <- GET("http://soilanimals.myspecies.info/")

http_status(con)

webpage <- content(con,"text")

wb <- readLines(tc <- textConnection(con))
close(tc)
str(content(con, "parsed"))
http_error(con)
query<-list(page="2")
con2 <- GET("http://soilanimals.myspecies.info/?format=json",query = query)
webpage2 <- content(con2, "text")


url <- "http://iczn.org"
dt <- readHTMLList(url)
dt <- xml()
dt2 <- readHTMLTable(url)
dt <- htmlTreeParse()
