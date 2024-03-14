library(glue)
library(httr)
library(dplyr)
library(lubridate)

#' Get legal product ids for Agridata taxud API
getAgridataProducts <- function() {
  req <- glue::glue("http://www.ec.europa.eu/agrifood/api/taxud/weeklyData/export/products")
  result <- httr::GET(req)
  return(unlist(content(result)))
}

#' Get legal sector ids for Agridata taxud API
getAgridataSectors <- function() {
  req <- glue::glue("http://www.ec.europa.eu/agrifood/api/taxud/weeklyData/sectors")
  result <- httr::GET(req)
  return(unlist(content(result)))
}

#' Collect data from Taxud API
#'
#' @description Query the taxud endpoint with weekly eexport data.
#' @examples
#' getAgridataTaxud(2022, products = "Fresh milk")
#' getAgridataTaxud(2022, products = "Cheese", type = "import)
getAgridataTaxud <- function(years, sectors = "Milk And Milk Products", products = "Fresh Milk", type = "export") {
  if (!(type %in% c("import", "export"))) stop("Illegal value in type.")
  years <- paste(years, collapse = ",")
  products <- gsub(" ", "%20", products)
  sectors <- gsub(" ", "%20", sectors)

  req <- glue::glue("http://www.ec.europa.eu/agrifood/api/taxud/weeklyData/{type}?partnerCodes=CH&marketingYears={years}&products={products}&sectors={sectors}")
  resp <- httr::GET(req)

  result <- content(resp)
  df_list <- lapply(result, as.data.frame)

  data <- do.call("rbind", df_list) %>%
    mutate(stichtag = as.Date(paste(marketingYear, 1, 1, sep = "-")) + weeks(week),
           month = lubridate::month(stichtag)) %>%
    group_by(sector, product, marketingYear, month, partner) %>%
    summarise(euroValue = sum(euroValue), kg = sum(kg)) %>%
    ungroup()

  return(data)
}


df <- getAgridataTaxud(2010:2023) %>%
  tidyr::pivot_longer(cols = -c(1,2,3,4,5), names_to = "Einheit", values_to = "Wert") %>%
  write.csv("data/agridata_eu_fresh_milk.csv", row.names = FALSE)

