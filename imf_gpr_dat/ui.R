library(tidyverse)
library(stringr)
library(lubridate)
library(shiny)
library(shinydashboard)

df <- readRDS("df.rds")
gpr <- readRDS("gpr.rds")
energy <- readRDS("energy.rds")

mon <- df %>% filter(str_detect(description, "rate")) %>% 
  rename(date = year_month) %>% 
  mutate(description = droplevels(description),
         indicator = droplevels(indicator),
         iso2c = droplevels(iso2c))

prices <- df %>% filter(description %in% c("CPI", "PPI", "Financial Market Price Indicator"))%>% 
  rename(date = year_month) %>% 
  mutate(description = droplevels(description),
         indicator = droplevels(indicator),
         iso2c = droplevels(iso2c))

econ <- df %>% filter(description == "GDP" | 
                        description == "Industrial production") %>% 
  rename(date = year_month) %>% 
  mutate(description = droplevels(description),
         indicator = droplevels(indicator),
         iso2c = droplevels(iso2c))

exchange <- df %>% filter(str_detect(description, "Exchange Rate")) %>% 
  rename(date = year_month) %>% 
  mutate(description = droplevels(description),
         indicator = droplevels(indicator),
         iso2c = droplevels(iso2c))

labor <- df %>% filter(str_detect(description %>% tolower(), "wage") | 
                         str_detect(description, "Unemp") |
                         str_detect(description %>% tolower(), "labour")) %>% 
  rename(date = year_month) %>% 
  mutate(iso2c = droplevels(iso2c),
         indicator = droplevels(indicator),
         description = droplevels(description))


ui <- dashboardPage(
  dashboardHeader(title = "Data dashboard"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 500)),
      
      box(
        title = "Controls",
        sliderInput("range1", "Date:", 
                    min = as.Date(min(mon$date)), 
                    max = as.Date(max(mon$date)),
                    value = c(as.Date(min(mon$date)), as.Date(max(mon$date))),
                    timeFormat="%Y-%m-%d"),
        checkboxGroupInput("country1", "Countries:",
                           choices = levels(mon$iso2c),
                           selected = levels(mon$iso2c)),
        checkboxGroupInput("indicator1", "Indicators:",
                           choices = levels(mon$description),
                           selected = levels(mon$description)
        )
      )
    ),
    fluidRow(
      box(plotOutput("plot2", height = 500)),

      
      box(
        title = "Controls",
        sliderInput("range2", "Date:", 
                    min = as.Date(min(econ$date)), 
                    max = as.Date(max(econ$date)),
                    value = c(as.Date(min(econ$date)), as.Date(max(econ$date))),
                    timeFormat="%Y-%m-%d"),
        checkboxGroupInput("country2", "Countries:",
                           choices = levels(econ$iso2c),
                           selected = levels(econ$iso2c)),
        checkboxGroupInput("indicator2", "Indicators:",
                           choices = levels(econ$description),
                           selected = levels(econ$description)
        )
      )
    ),
    fluidRow(
      box(plotOutput("plot3", height = 500)),
      
      box(
        title = "Controls",
        sliderInput("range3", "Date:", 
                    min = as.Date(min(prices$date)), 
                    max = as.Date(max(prices$date)),
                    value = c(as.Date(min(prices$date)), as.Date(max(prices$date))),
                    timeFormat="%Y-%m-%d"),
        checkboxGroupInput("country3", "Countries:",
                           choices = levels(prices$iso2c),
                           selected = levels(prices$iso2c)),
        checkboxGroupInput("indicator3", "Indicators:",
                           choices = levels(prices$description),
                           selected = levels(prices$description)
        )
      )
    ),
    fluidRow(
      box(plotOutput("plot4", height = 500)),
      
      box(
        title = "Controls",
        sliderInput("range4", "Date:", 
                    min = as.Date(min(exchange$date)), 
                    max = as.Date(max(exchange$date)),
                    value = c(as.Date(min(exchange$date)), as.Date(max(exchange$date))),
                    timeFormat="%Y-%m-%d"),
        checkboxGroupInput("country4", "Countries:",
                           choices = levels(exchange$iso2c),
                           selected = levels(exchange$iso2c)),
        checkboxGroupInput("indicator4", "Indicators:",
                           choices = levels(exchange$description),
                           selected = levels(exchange$description)
        )
      )
    ),
    fluidRow(
      box(plotOutput("plot5", height = 500)),

      box(
        title = "Controls",
        sliderInput("range5", "Date:", 
                    min = as.Date(min(energy$date)), 
                    max = as.Date(max(energy$date)),
                    value = c(as.Date(min(energy$date)), as.Date(max(energy$date))),
                    timeFormat="%Y-%m-%d")
        )
      ),
    fluidRow(
      box(plotOutput("plot6", height = 500)),
      
      box(
        title = "Controls",
        sliderInput("range6", "Date:", 
                    min = as.Date(min(gpr$date)), 
                    max = as.Date(max(gpr$date)),
                    value = c(as.Date(min(gpr$date)), as.Date(max(gpr$date))),
                    timeFormat="%Y-%m-%d")
        )
      ),
    fluidRow(
      box(plotOutput("plot7", height = 500)),
      
      box(
        title = "Controls",
        sliderInput("range7", "Date:", 
                    min = as.Date(min(labor$date)), 
                    max = as.Date(max(labor$date)),
                    value = c(as.Date(min(labor$date)), as.Date(max(labor$date))),
                    timeFormat="%Y-%m-%d"),
        checkboxGroupInput("country7", "Countries:",
                           choices = levels(labor$iso2c),
                           selected = levels(labor$iso2c)),
        checkboxGroupInput("indicator7", "Indicators:",
                           choices = levels(labor$description),
                           selected = levels(labor$description)
        )
      )
    )
    
    )
)



