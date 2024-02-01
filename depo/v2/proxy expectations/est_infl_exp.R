library(tidyverse)
library(lubridate)
library(readr)
library(stringr)
library(forecast)

#Load data series           
ds <- lapply(paste(file.path('data', 'input'), list.files(file.path('data','input')), sep = '/') , read_rds)
names(ds) <- str_remove_all(list.files(file.path('data','input')), '.rds')

data <- ds[[1]]
for(i in 2:length(ds)){
  data <- data %>% 
    full_join(ds[[i]])
}
rm(ds, i)


#Measure inflation expectations as realized core inflation from best fitting univiariate model
#Coefs are retrieved in the code just in case we want to further use them

data <- data %>% 
  select(country, date, core_infl) %>% 
  drop_na() %>% 
  arrange(country, date) %>% 
  group_by(country) %>% 
  nest() %>% 
  mutate(date_min = map(data, ~.x %>% pull(date) %>% min()),
         date_min = date_min[[1]],
         date_max = map(data, ~.x %>% pull(date) %>% max()),
         date_max = date_max[[1]],
         date = paste(seq(from = date_max+months(1), to = date_max+years(1), by = 'months'), collapse = ',')) %>% 
  separate_rows(date, sep = ',') %>% 
  nest(fcdates = date) %>% 
  mutate(fcdates = map(fcdates, ~.x %>% mutate(date = as_date(date))),
         date = map(data, ~.x %>% select(date)), 
         data = map(data, ~.x %>% select(-date) %>% 
                      ts(start = as_date(date_min), freq = 12)),
         mod = map(data, ~auto.arima(.x, max.d = 1, max.D = 0, 
                                     max.p = 12, max.q = 0,
                                     max.P = 0, max.Q = 0)),
         exp_infl = map(mod, ~.x[['fitted']]),
         coefs = map(mod, ~.x[['coef']]),
         fc = map(mod, ~as_tibble(forecast(.x, 12)) %>% select(x = `Point Forecast`) )) %>% 
  mutate(data = map(data, ~as_tibble(.x)),
         exp_infl = map(exp_infl, ~as_tibble(.x)),
         data2 = map2(data, exp_infl, ~bind_cols(.x, .y)),
         data2 = map2(data2, date, ~bind_cols(.y, .x)),
         fcdata = map2(fcdates, fc, ~bind_cols(.x, .y)),
         data2 = map2(data2, fcdata, ~bind_rows(.x, .y))) %>% 
  select(country, data = data2, coefs) %>% 
  unnest(data) %>% 
  mutate(coefs = map(coefs, ~tibble(names(.x), .x))) %>% 
  unnest(coefs) %>% 
  rename(exp_infl = x,
         coefname = `names(.x)`,
         coef = .x) %>% 
  spread(coefname, coef) %>% 
  mutate(across(.cols = c(-date, -core_infl, -exp_infl), ~replace_na(.x, 0))) %>% 
  ungroup() 


saveRDS(data %>% 
          select(country, date, exp_infl), 'data/output/exp_infl.rds')
rm(data)