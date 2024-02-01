infl <- get_eurostat('prc_hicp_midx', select_time = 'M')

infl <- infl %>% 
  filter(unit == 'I15') %>% 
  select(ccode2 = geo,
         date = time,
         coicop,
         value = values) %>% 
  inner_join(countries) %>% 
  select(country, date, coicop, value) %>%
  arrange(country, coicop, date) %>% 
  filter(coicop  %in%  c('CP00', 'TOT_X_NRG_FOOD', 'FOOD','ELC_GAS', 'FUEL')) 

mod <- infl %>% 
  group_by(country, coicop) %>% 
  mutate(value = value - dplyr::lag(value, 12)) %>% 
  spread(coicop, value) %>% 
  drop_na() %>% 
  ungroup() %>% 
  lm(CP00 ~ -1 + country + as.factor((year(date))) + ELC_GAS + FOOD + FUEL + TOT_X_NRG_FOOD, 
     data = .) 

summary(mod)

infl %>% 
  group_by(country, coicop) %>% 
  mutate(value = value - dplyr::lag(value, 12)) %>% 
  spread(coicop, value) %>% 
  drop_na() %>% 
  ungroup() %>% 
  left_join(
    tibble(country = names(mod$coefficients),
           cfe = mod$coefficients) %>% 
      filter(str_detect(country, 'country')) %>% 
      mutate(country = str_remove_all(country, 'country'))
  ) %>% 
  mutate(year = year(date)) %>% 
  left_join(
    tibble(year = names(mod$coefficients),
           yfe = mod$coefficients) %>% 
      filter(str_detect(year, DGT)) %>% 
      mutate(year = str_remove_all(year, 'as\\.factor\\(\\(year\\(date\\)\\)\\)') %>% as.numeric())
  ) %>% 
  mutate(yfe = replace_na(yfe, 0)) %>% 
  select(-year) %>% 
  gather(var, value, -country, -date, -CP00, -cfe, -yfe) %>% 
  left_join(
    tibble(var = names(mod$coefficients),
           coef = mod$coefficients) %>% 
      filter(!str_detect(var, or('country', 'year')))
  ) %>% 
  mutate(value = value*coef) %>% 
  select(-coef) %>% 
  spread(var, value) %>% 
  group_by(country) %>% 
  #mutate(across(c('CP00', 'TOT_X_NRG_FOOD', 'FOOD', 'NRG', 'ELC_GAS', 'FUEL', 'SERV', 'AP'), 
  #              ~.x - dplyr::lag(.x, 12))) %>% 
  #drop_na() %>% 
  #ungroup() %>% 
  mutate(cpi_fit = cfe + yfe + ELC_GAS + FOOD + FUEL + TOT_X_NRG_FOOD,
         Other = CP00 - cpi_fit - cfe - yfe)  %>% 
  select(-cpi_fit, -yfe, -cfe) %>% 
  gather(var, contrib, -country, -date, -CP00) %>% 
  mutate(var = case_when(var == 'FUEL' ~ 'Transportation Fuel',
                         var == 'ELC_GAS' ~ 'Electricity, Gas, Heating Energy',
                         var == 'FOOD' ~ 'Food',
                         var == 'TOT_X_NRG_FOOD' ~ 'Core Inflation',
                         var == 'Other' ~ 'Other factors')) %>% 
  mutate(var = factor(var, 
                      levels = c('Core Inflation', 'Food', 'Electricity, Gas, Heating Energy',
                                      'Transportation Fuel', 'Other factors'),
                      labels = c('Core Inflation', 'Food', 'Electricity, Gas, Heating Energy',
                                 'Transportation Fuel', 'Other factors')
                      ),
         var = fct_rev(var)) %>% 
  filter(year(date) >=2019) %>% 
  ggplot(aes(x = date)) +
  geom_col(aes(y = contrib, fill = var), color = 'white') +
  geom_line(aes(y = CP00), linewidth = .75) + 
  facet_wrap(~country, scales = 'free') +
  theme_minimal() +
  scale_fill_brewer(palette = 'Dark2', 'Contribution to\nheadline inflation', direction = -1)

