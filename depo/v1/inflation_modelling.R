library(tidyverse)
library(fredr)
library(lubridate)


#data issues
df_all <- read_rds("df_all.rds")
month_quarter <- tibble(month = 1:12) %>% 
  mutate(quarter = case_when(month %in% 1:3 ~ 1,
                             month %in% 4:6 ~ 2,
                             month %in% 7:9 ~ 3,
                             month %in% 10:12 ~ 4))

data <-df_all %>% group_by(iso2c) %>% nest() %>% 
  mutate(data = case_when(iso2c %in% c("BG", "CZ", "HU", "PL", "RO", "RS") ~
                            map(data, ~filter(.x, description %in% c("CPI", "GDP", "Policy rate", 
                                                                     "Real Effective Exchange Rate", "Wages and salaries, Index, B-S"))),
                          iso2c %in% c("EE", "LV", "SI", "SK", "HR") ~ 
                            map(data, ~filter(.x, description %in% c("CPI", "GDP", "Short term interest rate", 
                                                                     "Real Effective Exchange Rate", "Wages and salaries, Index, B-S"))),
                          iso2c == "LT" ~
                            map(data, ~filter(.x, description %in% c("CPI", "GDP", "Govt. bonds rate", 
                                                                     "Real Effective Exchange Rate", "Wages and salaries, Index, B-S"))),
                          iso2c == "Global" ~
                            map(data, ~filter(.x, description %in% c("Energy Price Index", "Geopolitical Risk Index")) %>% mutate(freq = "M"))
  ) 
  ) %>% 
  mutate(data = map(data, ~mutate(.x, year = year(date),
                                  month = month(date)) %>% 
                      left_join(month_quarter, by = "month") %>% 
                      group_by(indicator, quarter, year) %>% 
                      mutate(val = mean(value),
                             name = case_when(description == "CPI" ~ "pi", 
                                              description == "GDP" ~ "y",
                                              description == "Policy rate" ~ "r",
                                              description == "Short term interest rate" ~ "r",
                                              description == "Govt. bonds rate" ~ "r",
                                              description == "Real Effective Exchange Rate" ~ "e",
                                              description == "Wages and salaries, Index, B-S" ~ "w",
                                              description == "Energy Price Index" ~ "gep",
                                              description == "Geopolitical Risk Index" ~  "gpr")) %>% 
                      ungroup() %>% 
                      distinct(indicator, quarter, year, .keep_all = TRUE) %>% 
                      dplyr::select(date, val, name) %>% 
                      spread(name, val) %>% 
                      drop_na()
  ))


global <- data %>% filter(iso2c == "Global") %>% unnest(data) %>% ungroup %>% dplyr::select(-iso2c)

data <- data %>% filter(iso2c != "Global") %>% 
  mutate(data = map(data, ~inner_join(.x, global, by = "date"))) %>% 
  unnest(data)


data <- data %>% mutate(year = year(date)) %>% 
  left_join(data %>% mutate(year = year(date)) %>% 
              group_by(iso2c, year) %>% 
              summarize(mean_y = mean(y)) %>% 
              filter(year == 2010) %>% ungroup(),
            by = "iso2c") %>% 
  mutate(y = 100*y/mean_y) %>% 
  dplyr::select(-starts_with('year'), -mean_y) %>% 
  rename(country = iso2c) %>% ungroup()




#Panel var attempt
library(panelvar)

pforecast <- function(data, h){

panel <- data %>% mutate(date = as.numeric(date)) %>% 
  as.data.frame()



mod_ols <- pvarfeols(dependent_vars = c("y", "pi", "r", "e", "w", "gep", "gpr"),
                     lags = 1,
                     transformation = "demean",
                     data = panel,
                     panel_identifier = c("country", "date")
)


  
coefs <- mod_ols$OLS$coef

fe <- data %>% 
  group_by(country) %>% 
  summarize(across(c(y, pi, r, e, w, gep, gpr), ~mean(.x, na.rm = TRUE)))
colnames(fe)[2:length(colnames(fe))] <- paste(colnames(fe)[2:length(colnames(fe))], "_fe", sep = "") 

country_dates <- data %>% 
  group_by(country) %>% 
  select(date) %>% 
  nest()



data_demean <- data %>% group_by(country) %>% 
  mutate(e = e-mean(e),
         r = r-mean(r),
         pi = pi-mean(pi),
         w = w-mean(w),
         y = y-mean(y), 
         gep = gep-mean(gep),
         gpr = gpr-mean(gpr)) %>% 
  select(y, pi, r, e, w, gep, gpr) %>% 
  nest()

data_listed <- NULL
forecast_listed <- NULL
aggr_listed <- NULL
dates_listed <- NULL
forecast_dates <- NULL

for(i in 1:nrow(data_demean)){
  #Create listed data matrices
  data_listed[[i]] <-data_demean[i,]$data 
  data_listed[[i]] <- data_listed[[i]][[1]] %>% as.matrix()
  
  #Create listed dates
  dates_listed[[i]] <- country_dates[i, ]$data
  dates_listed[[i]] <- dates_listed[[i]][[1]]
  
  #Create listed empty matrices for forecast with inital values
  forecast_listed[[i]] <- matrix(nrow = (h+1), ncol = 7, data = NA)
  forecast_listed[[i]][1, ] <- data_listed[[i]][nrow(data_listed[[i]]),]
  
  #Create listed empty matrices for forecast dates with inital values
  forecast_dates[[i]] <- matrix(nrow = (h+1), ncol = 1, data = NA)
  forecast_dates[[i]][1, ] <- dates_listed[[i]][nrow(dates_listed[[i]]),]$date
  
  #Forecast
  for(j in 2:(h+1)){
    forecast_listed[[i]][j, ] <- forecast_listed[[i]][(j-1), ] %*% t(coefs)
    forecast_dates[[i]][j, ] <- as_date(forecast_dates[[i]][(j-1),]) + months(3)
  }
  colnames(forecast_listed[[i]]) <- colnames(data_listed[[i]])
  forecast_listed[[i]] <- forecast_listed[[i]][2:h, ]
  forecast_dates[[i]] <- forecast_dates[[i]][2:h, ]
  
  #Pool forecast and actual data
  aggr_listed[[i]] <-   bind_rows(data_listed[[i]] %>% 
                                    as_tibble() %>% 
                                    mutate(k = "data") %>% 
                                    bind_cols(dates_listed[[i]]), 
                                  forecast_listed[[i]] %>% 
                                    as_tibble() %>% 
                                    mutate(k = "forecast") %>% 
                                    bind_cols(forecast_dates[[i]]) %>% 
                                    rename(date = ...9) %>% 
                                    mutate(date = as_date(date))) %>% 
    mutate(country = data_demean$country[i])
}

aggr_listed %>% bind_rows() %>% 
  left_join(fe, by = "country") %>% 
  mutate(y = y + y_fe,
         pi = pi + pi_fe,
         r = r + r_fe,
         e = e + e_fe,
         w = w + w_fe,
         gep = gep + gep_fe,
         gpr = gpr + gpr_fe) %>% 
  select(-contains("fe"))

}

forecast_precov <- pforecast(data = data %>% 
                               filter(date <= as_date("2020-01-01")), h = 20) %>% 
  mutate(state = "Pre-Covid")

forecast_prewar <- pforecast(data = data %>% 
                               filter(date <= as_date("2021-10-01")), h = 20) %>% 
  mutate(state = "Pre-War")

bind_rows(data %>% 
            #filter(date >= as_date("2020-01-01")) %>% 
            mutate(state = "Actual Data"),
          forecast_precov %>% filter(k == "forecast"),
          forecast_prewar %>% filter(k == "forecast")) %>% 
  distinct(country, date, state, .keep_all = TRUE) %>% 
  filter(year(date) >= 2015,
         year(date) <= 2024)
  
  
ggplot() +
  geom_line(data = data %>% 
              filter(year(date) >= 2018), aes(x = date, y = pi), 
            size = 1, linetype = "solid", color = "black") +
  geom_line(data = forecast_precov %>% 
              filter(date >= "2020-01-01",
                     year(date) < 2024), aes(x = date, y = pi), 
            size = 1, linetype = "dashed", color = "red") +
  geom_line(data = forecast_prewar %>% 
              filter(date >= "2021-10-01",
                     year(date) < 2024), aes(x = date, y = pi), 
            size = 1, linetype = "dotted", color = "blue") +
  facet_wrap(~country, scales = "free") +
  theme_minimal()


varmod <- data %>% group_by(country) %>% 
  select(-date) %>% 
  nest() %>% 
  filter(country !="LT") %>% 
  mutate(data = map(data, ~ts(.x))) %>% 
  mutate(var = map(data, ~vars::VAR(.x, p = 1, "const"))) %>% 
  mutate(coef = map(var, ~summary(.x)$varresult$pi$coefficients %>%
                      as_tibble() %>%  
                      select(Estimate) %>% t() %>% as_tibble() ))

coefs <- varmod %>% select(coef) %>%  unnest(coef)
colnames(coefs) <- c("country", "e", "pi", "r", "w", "y", "gep", "gpr", "const")

coefs <- coefs %>% ungroup %>% select(-country, -const) %>% as.matrix()

data_mat <- data %>% 
  filter(country != "LT") %>% 
  select(-date) %>% 
  group_by(country) %>% 
  nest()

country_dates <- data %>% 
  filter(country != "LT") %>% 
  group_by(country) %>% 
  select(date) %>% 
  nest()

data_listed <- NULL
fit_listed <- NULL
dates_listed <- NULL
aggr_listed <- NULL


for(i in 1:nrow(data_mat)){
  #Create listed data matrices
  data_listed[[i]] <-data_mat[i,]$data 
  data_listed[[i]] <- data_listed[[i]][[1]] %>% as.matrix()
  
  #Create listed dates
  dates_listed[[i]] <- country_dates[i, ]$data
  dates_listed[[i]] <- dates_listed[[i]][[1]]
  
  #Create listed fit matrices
  fit_listed[[i]] <- matrix(nrow = nrow(data_listed[[i]]), ncol = 7, data = NA)
  
  #Predict
  for(j in 2:nrow(data_listed[[i]])){
    fit_listed[[i]][j,] <- data_listed[[i]][j-1, ] * coefs[i, ]
  }
  colnames(fit_listed[[i]]) <- paste(colnames(data_listed[[i]]), "_fit", sep = "")
  
  aggr_listed[[i]] <- bind_cols(dates_listed[[i]],
                                data_listed[[i]],
                                fit_listed[[i]]) %>% 
    mutate(country = data_demean$country[i])
}

aggr_listed %>% 
  bind_rows() %>% 
  select(country, date, pi, ends_with("fit")) %>% 
  drop_na() %>% 
  filter(date == as_date("2022-01-01") | date == max(date)) %>% 
  group_by(country) %>% 
  summarize(across(-date, ~.x-lag(.x))) %>% 
  drop_na() %>% 
  mutate(across(everything(), ~100*.x/pi)) %>% 
  mutate(check = e_fit + pi_fit + r_fit + w_fit + y_fit + gep_fit + gpr_fit)
  
  
  
  #filter(date >= as_date("2021-01-01")) %>% 
  gather(key = "key", value = "value", pi, gep_fit) %>% 
  ggplot(aes(x = date, y = value, color = key)) +
  geom_line(size = 1) +
  facet_wrap(~country, scales = "free") +
  theme_minimal()


#####
data_fd <- data %>% 
  group_by(country) %>% 
  #mutate(across(c(-date), ~.x-lag(.x))) %>% 
  drop_na()

mod_fullrange <- pvarfeols(dependent_vars = c("y", "pi", "r", "e", "w", "gep", "gpr"),
                           lags = 1,
                           transformation = "demean",
                           data = data_fd %>% mutate(date = as.numeric(date)) %>% 
                             as.data.frame(),
                           panel_identifier = c("country", "date")
)

pi_equation <- mod_fullrange$OLS$coef[2, ]

fe <- data_fd %>% 
  group_by(country) %>% 
  summarize(across(c(y, pi, r, e, w, gep, gpr), ~mean(.x, na.rm = TRUE)))
colnames(fe)[2:length(colnames(fe))] <- paste(colnames(fe)[2:length(colnames(fe))], "_fe", sep = "") 

country_dates <- data_fd %>% 
  group_by(country) %>% 
  select(date) %>% 
  nest()



data_demean <- data_fd %>% group_by(country) %>% 
  mutate(e = e-mean(e),
         r = r-mean(r),
         pi = pi-mean(pi),
         w = w-mean(w),
         y = y-mean(y), 
         gep = gep-mean(gep),
         gpr = gpr-mean(gpr)) %>% 
  select(y, pi, r, e, w, gep, gpr) %>% 
  nest()

data_listed <- NULL
fit_listed <- NULL
dates_listed <- NULL
aggr_listed <- NULL


for(i in 1:nrow(data_demean)){
  #Create listed data matrices
  data_listed[[i]] <-data_demean[i,]$data 
  data_listed[[i]] <- data_listed[[i]][[1]] %>% as.matrix()
  
  #Create listed dates
  dates_listed[[i]] <- country_dates[i, ]$data
  dates_listed[[i]] <- dates_listed[[i]][[1]]
  
  #Create listed fit matrices
  fit_listed[[i]] <- matrix(nrow = nrow(data_listed[[i]]), ncol = 7, data = NA)
  
  #Predict
  for(j in 2:nrow(data_listed[[i]])){
    fit_listed[[i]][j,] <- data_listed[[i]][j-1, ] * pi_equation
  }
  colnames(fit_listed[[i]]) <- paste(colnames(data_listed[[i]]), "_fit", sep = "")

  aggr_listed[[i]] <- bind_cols(dates_listed[[i]],
                                data_listed[[i]],
                                fit_listed[[i]]) %>% 
    mutate(country = data_demean$country[i])
}

aggr_listed %>% 
  bind_rows() %>% 
  select(country, date, pi, ends_with("fit")) %>% 
  drop_na() %>% 
  filter(date == as_date("2022-01-01") | date == max(date)) %>% 
  group_by(country) %>% 
  summarize(across(-date, ~.x-lag(.x))) %>% 
  drop_na() %>% 
  mutate(across(everything(), ~100*.x/pi))
  
  
  
  
  #filter(date >= as_date("2021-01-01")) %>% 
  gather(key = "key", value = "value", pi, y_fit) %>% 
  ggplot(aes(x = date, y = value, color = key)) +
  geom_line(size = 1) +
  facet_wrap(~country, scales = "free") +
  theme_minimal()



%>% 
  group_by(country) %>% 
  mutate(mdate = max(date)) %>% 
  filter(date == as_date("2021_01-01") | date == mdate) %>% 
   %>% 
  filter(country != "LT") %>% 
  summarize(across(c(-date), ~.x-lag(.x))) %>% 
  drop_na()
#####










