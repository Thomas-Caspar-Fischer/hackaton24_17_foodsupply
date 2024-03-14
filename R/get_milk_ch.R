
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

