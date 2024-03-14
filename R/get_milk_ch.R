
rm(list=ls())




# packages ----------------------------------------------------------------

library(httr)
library(readr)
library(ggplot2)
library("tidyverse")
library("XML")

proxy_url <- "http://proxy-bvcol.admin.ch:8080"
httr::set_config(httr::use_proxy(proxy_url))


# SPARQL query to LINDAS --------------------------------------------------

sparql_string <- paste0('
  PREFIX cube: <https://cube.link/>
  PREFIX sh: <http://www.w3.org/ns/shacl#>
  PREFIX schema: <http://schema.org/>
  PREFIX time: <http://www.w3.org/2006/time#>
  PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
  
  SELECT DISTINCT 
                 ?measure
    ?year ?month 
  WHERE {
    GRAPH <https://lindas.admin.ch/foag/agricultural-market-data> {
      VALUES(?product) { 
(<https://agriculture.ld.admin.ch/foag/product/4>) }

      <https://agriculture.ld.admin.ch/foag/cube/MilkDairyProducts/Production_Quantity_Month> cube:observationSet ?observationSet .
      ?observationSet cube:observation ?observation .
    ?observation <https://agriculture.ld.admin.ch/foag/dimension/date> ?date .
    ?observation <https://agriculture.ld.admin.ch/foag/dimension/product> ?product .
    ?observation <https://agriculture.ld.admin.ch/foag/measure/quantity> ?measure .
    
      }
  ?date time:year ?year.
    OPTIONAL { ?date time:month ?month. } 
  }
                        ')

print(sparql_string)

query <- POST("https://ld.admin.ch/query", 
              add_headers("Accept" = "text/csv"), 
              content_type("application/x-www-form-urlencoded; charset=UTF-8"),
              body = paste("query=", sparql_string, sep = ""))

data <- content(query, encoding = "UTF-8")

days_in_month <- lubridate::days_in_month(as.Date(with(data, paste(year, month, "01", sep = "-"))))

data[,"milkPerDay"] <- data[,"measure"]/days_in_month * 0.001

TS <- ts(data$milkPerDay, start = 2001, frequency = 12)

y <- TS - decompose(TS)$seasonal
plot(TS, ylab = "Täglich produzierte Milchmenge [t]", xlab = "Zeit")
grid()
lines(y, col = "red", lwd = 2)

layout(mat = matrix(c(1,1,2), ncol = 3))
plot(diff(y)/mean(y)*100, ylab = "Veränderung der Milchproduktion im Vergleich zum Vormonat [%]", xlab = "Zeit")
grid()

z <- scale(diff(y))
data[,"z"] <- c(NA, z)
hist(z, col = "transparent", probability = TRUE, xlim = c(-4,4), main = "", ylab = "Dichte")
x <- seq(-5,5,l=1000)
lines(x, dnorm(x))
x2 <- x[x< -1.96]
y2 <- dnorm(x)[x< -1.96]
polygon(x = c(x2, rev(x2)), y = c(rep(0,length(x2)), rev(y2)), col = adjustcolor("red", 0.5))

data[,"extreme"] <- c(NA, z< -1.96)

