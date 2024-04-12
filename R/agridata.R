library(glue)
library(httr)
library(dplyr)
library(lubridate)
library(tidyr)

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
#' @description Query the taxud endpoint with weekly export and import data
#'   into EU Member states.
#' @param years Years which records should be collected for
#' @param sectors
#' @param products
#' @param type Whether to query imports or exports (from the perspective of the EU)
#' @examples
#' getAgridataTaxud(2022, products = "Fresh milk")
#' getAgridataTaxud(2022, products = "Cheese", type = "import)
#' getAgridataTaxud(2014, sectors = NULL, products = c("Fresh milk", "White sugar"))
getAgridataTaxud <- function(years,
                             sectors = NULL,#"Milk And Milk Products",
                             products = "Fresh Milk",
                             type = "export") {

  if (!(type %in% c("import", "export"))) stop("Illegal value in type.")


  formatYears <- function(years) {
    non_standard <- glue::glue("{years-1}/{years}")
    collapsed <- paste0(c(non_standard, years), collapse = ",")
    glue::glue("marketingYears={collapsed}")
  }

  f <- function(var) {
    values <- parent.frame()[[var]]

    if (!is.null(values)) {
      values <- paste0(gsub(" ", "%20", values), collapse = ",")
      return(glue::glue("{var}={values}"))
    }
  }

  options <- paste0(c(formatYears(years), f("sectors"), f("products"), "partnerCodes=CH"), collapse = "&")

  req <- glue::glue("http://www.ec.europa.eu/agrifood/api/taxud/weeklyData/{type}?{options}")
  # print(req)
  resp <- httr::GET(req)
  result <- content(resp, type = "application/json", encoding = "UTF-8")
  df_list <- lapply(result, as.data.frame)
  data <- do.call("rbind", df_list) %>%
    mutate(
      stichtag = as.Date(
        ifelse(
          grepl("/", marketingYear),
          paste(stringr::str_extract(marketingYear, "^[0-9]{4}"), 10, 1, sep = "-"),
          paste(marketingYear, 1, 1, sep = "-"))) + weeks(week),
      marketingYear = lubridate::year(stichtag),
      month = lubridate::month(stichtag),
      tons = kg/1000) %>%
    select(-kg)

  return(data)
}

#' Summarise data from Taxud API
#'
#' @param data Data obtained from getAgridataTaxud call
#' @param by column names in data
#' @examples
#' getAgridataTaxud(2023, products = c("Fresh milk")) %>%
#'   summariseAgridataTaxud()
summariseAgridataTaxud <- function(data, by = c("marketingYear", "product", "memberStateName")) {

  by <- intersect(by, colnames(data))
  if (length(by) == 0) stop("No valid variables to aggregate on provided.")

  data %>%
    group_by_at(by) %>%
    summarise(tons = sum(tons)) %>%
    ungroup() %>%
    arrange(desc(tons))
}

# exports <- getAgridataTaxud(2010:2023) %>%
#   tidyr::pivot_longer(cols = -c(1,2,3,4,5), names_to = "Einheit", values_to = "Wert") %>%
#   write.csv("data/agridata_eu_fresh_milk_export.csv", row.names = FALSE)
#
# import <- getAgridataTaxud(2010:2023, type = "import") %>%
#   tidyr::pivot_longer(cols = -c(1,2,3,4,5), names_to = "Einheit", values_to = "Wert") %>%
#   write.csv("data/agridata_eu_fresh_milk_import.csv", row.names = FALSE)
#
