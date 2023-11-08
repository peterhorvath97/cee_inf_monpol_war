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


data <- data %>% filter(iso2c != "Global") %>% 
  unnest(data) %>% ungroup()

data <- data %>% mutate(year = year(date)) %>% 
  left_join(data %>% mutate(year = year(date)) %>% 
              group_by(iso2c, year) %>% 
              summarize(mean_y = mean(y)) %>% 
              filter(year == 2010) %>% ungroup(),
            by = "iso2c") %>% 
  mutate(y = 100*y/mean_y) %>% 
  dplyr::select(-starts_with('year'), -mean_y) %>% 
  rename(country = iso2c) %>% group_by(country) %>% 
  #  mutate(y = y - dplyr::lag(y), 
  #         pi = pi - dplyr::lag(pi), 
  #         e = e - dplyr::lag(e)) %>% 
  ungroup() %>% 
  #  drop_na() %>% 
  dplyr::select(country, date, y, pi, r, e)


#Endpoint issue
data <- data %>% 
  mutate(actual = 1) %>% 
  group_by(country) %>% 
  nest()

growth <- c(rep(1.235,4),	rep(2.475,4),	rep(2.513,4),	rep(2.356,4),	rep(2.285,4),	rep(2.272,4))
dates <- (as_date("2023_01_01"):months(3):as_date("2028-10-01")) %>% as_date()
growthrates <- dates %>% as_tibble() %>% 
  mutate(ch = as.character(value),
         chlast = substr(ch, start = nchar(ch)-1, nchar(ch))) %>% 
  filter(chlast == "01") %>% 
  select(date = value) %>% 
  mutate(month = month(date)) %>% 
  filter(month == 1 | month == 4 | month == 7| month == 10) %>% 
  select(date) %>% 
  bind_cols(growth) %>% 
  rename(growth = ...2)



data <- data %>% 
  filter(country != "RS") %>% 
  mutate(data = map(data, ~.x %>% 
                      full_join(growthrates %>% 
                                  mutate(growth = 1 + growth/100,
                                         growth = accumulate(growth, ~.x*.y)),
                                by = "date") %>% 
                      ungroup() %>% 
                      filter((is.na(growth) & !is.na(y)) | (!is.na(growth) & is.na(y)) ) %>% 
                      mutate(actual = replace_na(actual, 0)) %>% 
                      fill(y, .direction = "downup") %>% 
                      mutate(y = ifelse(is.na(growth), y, y*growth)) %>% 
                      filter(year(date) <= 2025)
  ),
  e = map(data, ~.x %>% 
            select(e) %>% ts() ),
  e_mod = map(e, ~.x %>% auto.arima(max.d = 1, max.D = 0,
                                    max.P = 0, max.Q = 0) ),
  nahead = map(e, ~.x %>% is.na() %>% sum()  
  )) %>% 
  unnest(nahead) %>% 
  mutate(fc = map(e_mod, ~.x %>% forecast(nahead) %>% 
                    as_tibble() %>% 
                    select(`Point Forecast`) %>% 
                    rename(e = `Point Forecast`)),
         e = map(e, ~.x %>% 
                   as_tibble() %>% 
                   drop_na() %>% 
                   bind_rows(fc)),
         data = map(data, ~.x %>% 
                      select(-e, - growth) %>% 
                      bind_cols(e) )) %>% 
  select(country, data)







#create gaps
library(mFilter)
library(hpfilter)


data <- data %>% unnest(data) %>% mutate(pi = pi - dplyr::lag(pi, n = 4)) %>% 
  mutate(pi_targ = case_when(country == "BG" ~ 0,
                             country %in% c("CZ", "EE", "LT", "LV", "SI", "SK", "HR") ~ 2,
                             country %in%  c("HU", "RS") ~ 3,
                             country %in%  c("PL", "RO") ~ 2.5
  )) %>% 
  group_by(country) %>% 
  mutate(pi_gap = pi-pi_targ,
         y_gap = hpfilter(y, freq = 1600, type = "lambda")$cycle,
         e_gap = hpfilter(e, freq = 1600, type = "lambda")$cycle) %>% 
  select(-e, -y) %>% 
  drop_na() %>% 
  slice(5:n()) %>% 
  #slice(1:n()-1) %>% 
  ungroup() %>% 
  filter(actual == 1) %>% 
  select(-actual)

pigap <- data %>% 
  ggplot(aes(x = date, y =pi_gap)) +
  geom_line(size = .75) +
  facet_wrap(~country, scales = "free") +
  ggtitle("Inflation Gap")

ygap <- data %>% 
  ggplot(aes(x = date, y =y_gap)) +
  geom_line(size = .75) +
  facet_wrap(~country, scales = "free") +
  ggtitle("Output Gap")



#Estimate bayesian models
library(rstanarm)

#Panel
data_taylor <- data %>% 
  group_by(country) %>% 
  mutate(pi = pi-mean(pi),
         y_gap = y_gap - mean(y_gap),
         e_gap = e_gap - mean(e_gap),
         r = r - mean(r),
         lagr = lag(r)) %>% 
  ungroup()

mod1 <- stan_glm(r ~ -1 + pi + y_gap,
                 data = data_taylor,
                 prior = normal(location = c(1.5, 0.5),
                                scale = c(.1, .1)),
                 seed = 1234)


mod2 <- stan_glm(r ~ -1 + lagr + pi + y_gap,
                 data = data_taylor,
                 prior = normal(location = c(0.8, 0.8*1.5, 0.8*0.5),
                                scale = c(.1, .1, .1)),
                 seed = 1234)

mod3 <- stan_glm(r ~ -1 + lagr + pi + y_gap + e_gap,
                 data = data_taylor,
                 prior = normal(location = c(0.8, 0.8*1.5, 0.8*0.5, 0.8*0.2),
                                scale = c(.1, .1, .1, .1)),
                 seed = 1234)


panel_coefs <- bind_rows(
  mod1$coefficients,
  mod2$coefficients,
  mod3$coefficients,
) %>% 
  mutate(across(everything(), ~as.character(round(.x, digits = 4)))) %>% 
  mutate(model = c("Simple panel", 
                   "Panel with interest rate smoothing", 
                   "Panel with interest rate smoothing and exchange rate adjustment")) %>% 
  select(model, lagr, pi, y_gap, e_gap) %>%  
  replace_na(list(lagr = "",
                  e_gap = "") )

panel_intercept <- data %>% 
  group_by(country) %>% 
  summarize(intercept = mean(r))

panel_plot <- data %>% bind_cols(mod1$fitted.values) %>% 
  rename(r1 = ...9) %>% 
  group_by(country) %>% 
  slice(2:n()) %>% 
  bind_cols(mod2$fitted.values,
            mod3$fitted.values) %>% 
  rename(r2 = ...10,
         r3 = ...11) %>% 
  mutate(r1 = r1 + mean(r),
         r2 = r2 + mean(r),
         r3 = r3 + mean(r)) %>% 
  mutate(r4 = 2 + pi + 0.5*pi_gap + 0.5*y_gap) %>% 
  gather(key = "key", value = "value", r, r1, r2, r3, r4) %>% 
  mutate(key = case_when (key == "r" ~ "Interest",
                          key == "r4" ~ "Simple",
                          key == "r1" ~ "Simple bayes",
                          key == "r2" ~ "Smooth",
                          key == "r3" ~ "Smooth + exchange")) %>% 
  ggplot(aes(x = date, y = value, color = key)) +
  geom_line(size = 1) +
  facet_wrap(~country, scales = "free") +
  theme_minimal() +
  scale_color_manual(breaks = c("Interest", "Simple", 
                                "Simple bayes", "Smooth", 
                                "Smooth + exchange"),
                     values = c("black", "red",
                                "blue", "green",
                                "purple")) +
  ggtitle("Interest rates and Bayesian Taylor rules - Panel estimation") +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

panel_averages <- data %>% bind_cols(mod1$fitted.values) %>% 
  rename(r1 = ...9) %>% 
  group_by(country) %>% 
  slice(2:n()) %>% 
  bind_cols(mod2$fitted.values,
            mod3$fitted.values) %>% 
  rename(r2 = ...10,
         r3 = ...11) %>% 
  mutate(r1 = r1 + mean(r),
         r2 = r2 + mean(r),
         r3 = r3 + mean(r)) %>% 
  mutate(r4 = 2 + pi + 0.5*pi_gap + 0.5*y_gap) %>% 
  group_by(country) %>% 
  summarize(across(c(r, r1, r2, r3, r4), ~mean(.x))) %>% 
  gather(key = "key", value = "value", r, r1, r2, r3, r4) %>% 
  mutate(key = case_when (key == "r" ~ "Interest",
                          key == "r4" ~ "Simple",
                          key == "r1" ~ "Simple empirical",
                          key == "r2" ~ "Smooth",
                          key == "r3" ~ "Smooth + exchange")) %>% 
  ggplot(aes(x = key, y = value)) +
  geom_col() +
  facet_wrap(~country, scales = "free") +
  theme_minimal()

panel_dev <- data %>% bind_cols(mod1$fitted.values) %>% 
  rename(r1 = ...9) %>% 
  group_by(country) %>% 
  slice(2:n()) %>% 
  bind_cols(mod2$fitted.values,
            mod3$fitted.values) %>% 
  rename(r2 = ...10,
         r3 = ...11) %>% 
  mutate(r1 = r1 + mean(r),
         r2 = r2 + mean(r),
         r3 = r3 + mean(r)) %>% 
  mutate(r4 = 2 + pi + 0.5*pi_gap + 0.5*y_gap) %>% 
  mutate(r1 = r-r1,
         r2 = r-r2,
         r3 = r-r3,
         r4 = r-r4) %>% 
  gather(key = "key", value = "value",r1, r2, r3, r4) %>% 
  mutate(key = case_when (
    key == "r4" ~ "Simple",
    key == "r1" ~ "Simple empirical",
    key == "r2" ~ "Smooth",
    key == "r3" ~ "Smooth + exchange")) %>% 
  ggplot(aes(x = date, y = value, color = key)) +
  geom_line(size = 1) +
  facet_wrap(~country, scales = "free") +
  theme_minimal()+
  scale_color_manual(breaks = c( "Simple", 
                                 "Simple empirical", "Smooth", 
                                 "Smooth + exchange"),
                     values = c("red",
                                "blue", "green",
                                "purple")) +
  ggtitle("Deviation from Taylor rules - Panel estimation") +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

#Individual
data_taylor <- data %>% 
  group_by(country) %>% 
  mutate(lagr = lag(r)) %>% 
  nest() %>% 
  mutate(mod1 = map(data, ~stan_glm(r-0.5*pi_targ ~ pi + y_gap,
                                    data = .x,
                                    prior = normal(location = c(1.5, 0.5),
                                                   scale = c(.1, .1)),
                                    prior_intercept = normal(location = 2,
                                                             scale = .1),
                                    seed = 1234)),
         mod2 = map(data, ~stan_glm(r-0.8*0.5*pi_targ ~ lagr + pi + y_gap,
                                    data = .x,
                                    prior = normal(location = c(0.8, 0.8*1.5, 0.8*0.5),
                                                   scale = c(.1, .1, .1)),
                                    prior_intercept = normal(location = 0.8*2,
                                                             scale = .1),
                                    seed = 1234)),
         mod3 = map(data, ~stan_glm(r-0.8*0.5*pi_targ ~ lagr + pi + y_gap + e_gap,
                                    data = .x,
                                    prior = normal(location = c(0.8, 0.8*1.5, 0.8*0.5, 0.8*0.2),
                                                   scale = c(.1, .1, .1, .1)),
                                    prior_intercept = normal(location = 0.8*2,
                                                             scale = .1),
                                    seed = 1234)),
         )

indiv_coefficients <- full_join(
data_taylor %>% 
  mutate(coef1 = map(mod1, ~.x$coefficients)) %>% 
  select(coef1) %>% 
  unnest(coef1) %>% 
  group_by(country) %>% 
  mutate(coef = c("Intercept", "pi", "y_gap")) %>% 
  ungroup(),
data_taylor %>% 
  mutate(
         coef2 = map(mod2, ~.x$coefficients)) %>% 
  select(coef2) %>% 
  unnest(coef2) %>% 
  group_by(country) %>% 
  mutate(coef = c("Intercept", "lagr", "pi", "y_gap")) %>% 
  ungroup(),
by = c("country", "coef")
)  %>% 
  full_join(
data_taylor %>% 
  mutate(
         coef3 = map(mod3, ~.x$coefficients)) %>% 
  select(coef3) %>% 
  unnest(coef3) %>% 
  group_by(country) %>% 
  mutate(coef = c("Intercept", "lagr", "pi", "y_gap", "e_gap")) %>% 
  ungroup(),
by = c("country", "coef")
) %>% 
  rename(mod1 = coef1,
         mod2 = coef2,
         mod3 = coef3) %>% 
  select(country, coef, mod1, mod2, mod3) %>%
  mutate(across(c(mod1, mod2, mod3) , ~round(.x, digits = 4))) %>% 
  gather(key = "model", value = "value", mod1, mod2, mod3) %>% 
  spread(country, value) %>% 
  mutate(coef = factor(coef, levels = c("Intercept", "lagr", "pi", "y_gap", "e_gap"),
                       labels = c("Intercept", "lagr", "pi", "y_gap", "e_gap"))) %>% 
  arrange(model, coef) 

indiv_plot <- data_taylor %>% 
  mutate(r1 = map(mod1, ~.x$fitted.values),
         r2 = map(mod2, ~.x$fitted.values),
         r3 = map(mod3, ~.x$fitted.values)) %>% 
  mutate(data = map(.x = data, .y = r1, ~bind_cols(.x, .y) %>% 
           slice(2:n()) ),
         data = map(.x = data, .y = r2, ~bind_cols(.x, .y)),
         data = map(.x = data, .y = r3, ~bind_cols(.x, .y))
         ) %>% 
  select(country, data) %>% 
  unnest(data) %>% 
  rename(r1 = ...9,
         r2 = ...10,
         r3 = ...11) %>% 
  mutate(r1 = r1 + 0.5*pi_targ,
         r2 = r2 + 0.8*0.5*pi_targ,
         r3 = r3 + 0.8*0.5*pi_targ,
         r4 = 2 + pi + 0.5*pi_gap + 0.5*y_gap) %>% 
  gather(key = "key", value = "value", r, r1, r2, r3, r4) %>% 
  mutate(key = case_when (key == "r" ~ "Interest",
                          key == "r4" ~ "Simple",
                          key == "r1" ~ "Simple bayes",
                          key == "r2" ~ "Smooth",
                          key == "r3" ~ "Smooth + exchange")) %>% 
  ggplot(aes(x = date, y = value, color = key)) +
  geom_line(size = 1) +
  facet_wrap(~country, scales = "free") +
  theme_minimal() +
  scale_color_manual(breaks = c("Interest", "Simple", 
                                "Simple bayes", "Smooth", 
                                "Smooth + exchange"),
                     values = c("black", "red",
                                "blue", "green",
                                "purple")) + 
  ggtitle("Interest rates and Bayesian Taylor rules - Individual estimation") +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))


indiv_averages <- data_taylor %>% 
  mutate(r1 = map(mod1, ~.x$fitted.values),
         r2 = map(mod2, ~.x$fitted.values),
         r3 = map(mod3, ~.x$fitted.values)) %>% 
  mutate(data = map(.x = data, .y = r1, ~bind_cols(.x, .y) %>% 
                      slice(2:n()) ),
         data = map(.x = data, .y = r2, ~bind_cols(.x, .y)),
         data = map(.x = data, .y = r3, ~bind_cols(.x, .y))
  ) %>% 
  select(country, data) %>% 
  unnest(data) %>% 
  rename(r1 = ...9,
         r2 = ...10,
         r3 = ...11) %>% 
  mutate(r1 = r1 + 0.5*pi_targ,
         r2 = r2 + 0.8*0.5*pi_targ,
         r3 = r3 + 0.8*0.5*pi_targ,
         r4 = 2 + pi + 0.5*pi_gap + 0.5*y_gap) %>% 
  group_by(country) %>% 
  summarize(across(c(r, r1, r2, r3, r4), ~mean(.x))) %>% 
  gather(key = "key", value = "value", r, r1, r2, r3, r4) %>% 
  mutate(key = case_when (key == "r" ~ "Interest",
                          key == "r4" ~ "Simple",
                          key == "r1" ~ "Simple empirical",
                          key == "r2" ~ "Smooth",
                          key == "r3" ~ "Smooth + exchange")) %>% 
  ggplot(aes(x = key, y = value)) +
  geom_col() +
  facet_wrap(~country, scales = "free") +
  theme_minimal()

indiv_dev <- data_taylor %>% 
  mutate(r1 = map(mod1, ~.x$fitted.values),
         r2 = map(mod2, ~.x$fitted.values),
         r3 = map(mod3, ~.x$fitted.values)) %>% 
  mutate(data = map(.x = data, .y = r1, ~bind_cols(.x, .y) %>% 
                      slice(2:n()) ),
         data = map(.x = data, .y = r2, ~bind_cols(.x, .y)),
         data = map(.x = data, .y = r3, ~bind_cols(.x, .y))
  ) %>% 
  select(country, data) %>% 
  unnest(data) %>% 
  rename(r1 = ...9,
         r2 = ...10,
         r3 = ...11) %>% 
  mutate(r1 = r1 + 0.5*pi_targ,
         r2 = r2 + 0.8*0.5*pi_targ,
         r3 = r3 + 0.8*0.5*pi_targ,
         r4 = 2 + pi + 0.5*pi_gap + 0.5*y_gap) %>% 
  mutate(r1 = r-r1,
         r2 = r-r2,
         r3 = r-r3,
         r4 = r-r4) %>% 
  gather(key = "key", value = "value",r1, r2, r3, r4) %>% 
  mutate(key = case_when (
    key == "r4" ~ "Simple",
    key == "r1" ~ "Simple empirical",
    key == "r2" ~ "Smooth",
    key == "r3" ~ "Smooth + exchange")) %>% 
  ggplot(aes(x = date, y = value, color = key)) +
  geom_line(size = 1) +
  facet_wrap(~country, scales = "free") +
  theme_minimal()+
  scale_color_manual(breaks = c( "Simple", 
                                 "Simple empirical", "Smooth", 
                                 "Smooth + exchange"),
                     values = c("red",
                                "blue", "green",
                                "purple")) +
  ggtitle("Deviation from Taylor rules - Indivitual estimation") +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))


