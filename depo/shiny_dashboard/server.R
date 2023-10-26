server <- function(input, output) {
  
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
  

  
  plot_data <- function(data){
    data %>% 
      ggplot(aes(x = date, y = value, color = iso2c)) +
      geom_line(size = .75) +
      facet_wrap(~description, scales = "free") +
      labs(x = "",
           y = "") +
      theme(legend.title = element_blank()) +
      scale_color_viridis_d(begin = 0, end = .8, option = "turbo") +
      theme(legend.position = "bottom")
  }
  
  
  
  output$plot1 <- renderPlot({
    mon %>% filter(date >=input$range1[1], 
                   date <=input$range1[2],
                   as.character(iso2c) %in% input$country1,
                   as.character(description) %in% input$indicator1) %>%  
      plot_data()
    
  })
  
  output$plot2 <- renderPlot({
    econ %>% 
      filter(description == "GDP") %>% 
      mutate(year = year(date)) %>% 
      group_by(iso2c, year) %>% 
      mutate(mean = mean(value)) %>% 
      ungroup() %>%
      group_by(iso2c) %>% 
      mutate(mean_2010 = ifelse(year == 2010, mean, NA),
             mean_2010 = mean(mean_2010, na.rm = TRUE)) %>% 
      mutate(value = 100*value / mean_2010) %>% 
      select(-year, -mean, -mean_2010) %>% 
      bind_rows(econ %>% filter(description != "GDP")) %>% 
      filter(date >=input$range2[1], 
             date <=input$range2[2],
             as.character(iso2c) %in% input$country2,
             as.character(description) %in% input$indicator2) %>% 
      plot_data() +
      labs(caption = "GDP volume index, 2010 average = 100%")
  })
  
  output$plot3 <- renderPlot({
    prices %>% filter(date >=input$range3[1], 
                      date <=input$range3[2],
                      as.character(iso2c) %in% input$country3,
                      as.character(description) %in% input$indicator3) %>%  
      plot_data()
  })
  
  output$plot4 <- renderPlot({
    exchange %>% 
      filter(str_detect(description, "Nominal")) %>% 
      filter(year(date) > 2000) %>% 
      bind_rows(exchange %>% filter(!str_detect(description, "Nominal"))) %>% 
      filter(date >=input$range4[1], 
             date <=input$range4[2],
             as.character(iso2c) %in% input$country4,
             as.character(description) %in% input$indicator4) %>%  
      plot_data() +
      labs(caption = "Nominal Effective Exchange Rates post 2000")
  })
  
  output$plot5 <- renderPlot({
    energy[1:which(energy$date == max(energy$date))[1], ] %>% 
      filter(date >=input$range5[1], 
             date <=input$range5[2]) %>%  
      plot_data()
      
  })
  
  output$plot6 <- renderPlot({
    gpr  %>% 
      filter(date >=input$range6[1], 
             date <=input$range6[2]) %>%   
      ggplot(aes(x = date, y = value)) +
      geom_line() +
      facet_wrap(~description, scales = "free") +
      labs(x = "",
           y = "")
    
  })
  output$plot7 <- renderPlot({
    labor %>% filter(date >=input$range7[1], 
                     date <=input$range7[2],
                     as.character(iso2c) %in% input$country7,
                     as.character(description) %in% input$indicator7) %>%  
      plot_data()
    
  })
}



