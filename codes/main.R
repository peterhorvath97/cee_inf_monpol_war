library(tidyverse)
library(readr)
library(stringr)
library(broom)
library(lubridate)
library(rebus)
library(ggthemes)
library(extrafont)
#font_import()
loadfonts()
library(countrycode)
library(readxl)
library(grDevices)




folder_input <- file.path('data', 'input')
folder_output <- file.path('data', 'output')
folder_estcodes <- file.path('codes', 'estimations')
chartout <- file.path('charts')
tabout <- file.path('tables')

#Set to true in case new data is available AND the outputs should be updated
update = F

if(update == T){
  source(file.path('codes', 'data_retrieval_codes', 'data_fetch.R'))
}
rm(update)
source(file.path(folder_estcodes, 'determinants of inflation', 'infl_decomp_simple.R'))
source(file.path(folder_estcodes, 'real rate', 'real_rate.R'))
source(file.path(folder_estcodes, 'descriptives', 'descriptives.R'))
source(file.path(folder_estcodes, 'svar shocks', 'svar_plots.R'))