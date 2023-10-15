#install.packages("rsconnect")
library(rsconnect)

setAccountInfo(name='horvathpeter97',
                          token='4CBD8333811A53EE861CB326369D431D',
                          secret='iQZF8ihvG87tkVKnTZ+kVSAiVnIb0HC9L6m+vu3K')

deployApp("imf_gpr_dat")




library(shiny)
library(shinydashboard)

source("imf_gpr_dat/ui.R")
source("imf_gpr_dat/server.R")

shinyApp(ui = ui, server = server)
