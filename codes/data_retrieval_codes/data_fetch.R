#devtools::install_github("christophergandrud/imfr")
library(imfr)
library(fredr)
library(OECD)
library(eurostat)
library(Rblpapi)

library(httr)
library(readxl)

library(tidyverse)
library(stringr)
library(rebus)
library(lubridate)
library(countrycode)

ccodes <- c("HU", "BG", "CZ", "EE",
            "LT", "LV", "PL", "RO",
            "SI", "SK", "RS", "HR")

countries <- countrycode::codelist %>% 
  select(country = country.name.en, ccode2 = iso2c, ccode3 = iso3c) %>% 
  filter(ccode2 %in% ccodes)

fold_varcodes <- 'codes/data_retrieval_codes/vars'
fold_data <- 'data/input'
fold_datasource <- 'data/source'

fredr_set_key("cda47ae66b38ed7988c0a9c2ec80c94f")

fred_query <- function(ids, freq, start){
  
  require(fredr)
  require(dplyr)
  require(purrr)
  
  #download data
  params <- list(
    series_id = ids,
    frequency = freq,
    observation_start = as.Date(start)
  )
  
  
  data  <- pmap_dfr(
    .l = params,
    .f = ~ fredr(series_id = .x, frequency = .y) ) %>%
    dplyr::select(date, series_id, value) %>%
    spread(key = series_id, value = value) %>%
    drop_na() 
  
  data
}

x13adj <- function(value, freq, date){
  require(forecast)
  require(seasonal)
  require(tidyverse)
  tryCatch(
    value %>% 
      ts(frequency = freq,
         start = c(date %>% min %>% year, 
                   date %>% min %>% month)) %>% 
      seas() %>% 
      seasadj() %>% 
      as.numeric(),
    error = function(error){value}
  )
}


code_folder <- file.path('codes/data_retrieval_codes/vars')
codes <- paste(code_folder, list.files(code_folder), sep = '/') 
lapply(codes, source)

