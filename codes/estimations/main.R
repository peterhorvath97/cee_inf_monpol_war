library(tidyverse)
library(readr)
library(stringr)
library(broom)
library(lubridate)

folder_input <- file.path('data', 'input')
folder_output <- file.path('data', 'output')
folder_estcodes <- file.path('codes', 'estimations')

source(file.path(folder_estcodes, 'determinants of inflation', 'infl_decomp_simple.R'))
source(file.path(folder_estcodes, 'real rate', 'real_rate.R'))