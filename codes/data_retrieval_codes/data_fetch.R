#devtools::install_github("christophergandrud/imfr")
library(imfr)
library(OECD)
library(eurostat)

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

