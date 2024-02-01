#First stage - IS curve
data_is <- data %>% 
  select(country, date, gdp, cpi_l, exp_infl, r, reer) %>% 
  group_by(country) %>% 
  mutate(gdp_l = dplyr::lag(gdp),
         exp_infl_l = dplyr::lag(exp_infl),
         year = as.factor(year(date))) %>% 
  ungroup() %>% 
  drop_na() 

mod_is <- plm(gdp ~ gdp_l + cpi_l + exp_infl_l + r + reer + year, 
    data = data_is, 
    index = c('country', 'date'),
    model = 'within',
    effect = 'individual') 


fe <- fixef(mod_is)
fe <- tibble(fe, country = names(fe))

data_is <-data_is %>% 
  left_join(fe) %>% 
  mutate(gdp_fit = fitted(mod_is) + fe,
         gdp_fit = as.numeric(gdp_fit))

data_is %>% 
  ggplot() +
  geom_line(aes(x = date, y = gdp), color = 'black') +
  geom_line(aes(x = date, y = gdp_fit), color = 'red', linetype = 'dashed') +
  facet_wrap(~country, scales = 'free') + 
  theme_minimal() +
  labs(x = '',
       y = '',
       caption = 'The solid line is the actual quarterly GDP, the dashed line is the fitted one from the First Stage - IS Curve regression')



