library(tidyverse)
library(readr)
library(stringr)
library(broom)
library(lubridate)
library(plm)
library(dplyr)

codes <- file.path('codes', 'estimations', 'determinants of inflation')
output <- file.path('data', 'output', 'MÉGNEMTUDOM')

#Load data
source(file.path(codes, 'prep_data.R'))




#Second stage - PC estimation
data_pc <- data %>% 
  #left_join(data_is %>% 
  #            select(country, date, gdp)) %>% 
  select(country, date, cpi, cpi_l, gdp, enprice, wage, reer, exp_infl) %>% 
  mutate(year = year(date) %>% as.factor()) %>% 
  drop_na()


mod_pc <- plm(cpi ~ cpi_l + exp_infl + gdp + wage + reer + enprice + year, 
              data = data_pc, 
              index = c('country', 'date'),
              model = 'within',
              effect = 'individual') 

summary(mod_pc)

fixef(mod_pc)
fixef(mod_pc, 'time')
fixef(mod_pc, 'twoways')

mod_pc <- lm(cpi ~ -1 + cpi_l +  gdp_hat + wage + reer + enprice + country + year + country*year , data =data_pc)
summary(mod_pc)

data_pc <- data_pc %>% 
  mutate(cpi_fit = predict(mod_pc))


data_pc %>% 
  ggplot() +
  geom_line(aes(x = date, y = cpi), color = 'black') +
  geom_line(aes(x = date, y = cpi_fit), color = 'red', linetype = 'dashed') +
  facet_wrap(~country, scales = 'free') + 
  theme_minimal() +
  labs(x = '',
       y = '',
       caption = 'The solid line is the actual quarterly Inflation, the dashed line is the fitted one from the Second Stage - Hybrid PC Curve regression')


coef <- tibble(
  var = names(mod_pc$coefficients),
  coef = mod_pc$coefficients) 




%>% 
  mutate(var = str_remove_all(var, or('country', 'year')) ) %>% 
  filter(!str_detect(var, or1(unique(data_pc$country))),
         !str_detect(var, DGT)) 

coefname <- coef$var
coef <- t(coef[,2]) %>% as.numeric()
names(coef) <- coefname







data_pc %>%
  #Add country FE
  left_join(
    tibble(
      var = names(mod_pc$coefficients),
      coef = mod_pc$coefficients) %>% 
      mutate(var = str_remove_all(var, or('country', 'year'))),
    by = c('country' = 'var')) %>% 
  rename(cfe = coef) %>% 
  #Add year FE
  left_join(
    tibble(
      var = names(mod_pc$coefficients),
      coef = mod_pc$coefficients) %>% 
      mutate(var = str_remove_all(var, or('country', 'year'))),
    by = c('year' = 'var')) %>% 
  rename(yfe = coef) %>% 
  #Add country-reer interaction
  left_join(tibble(
    var = names(mod_pc$coefficients),
    coef = mod_pc$coefficients) %>% 
      mutate(var = str_remove_all(var, or('country', 'year'))
      ) %>% 
      filter(str_detect(var, ':reer')) %>% 
      mutate(var = str_remove_all(var, ':reer')),
  by = c('country' = 'var')) %>% 
  rename(creer = coef) %>% 
  mutate(creer = replace_na(creer, 0)) %>% 
  mutate(across(names(coef), ~.*coef[as.character(substitute(.))]),
         reer = reer + creer*reer,
         other = cfe + yfe) %>% 
  select(-year, -cpi_fit, -creer, -cfe, -yfe) %>% 
  gather(key = 'variable', value = 'contrib', -country, -date, -cpi) %>% 
  mutate(variable = case_when(variable == 'cpi_l' ~ 'Past Inflation',
                              variable == 'wage' ~ 'Wage Dynamics',
                              variable == 'gdp_hat' ~ 'Output',
                              variable == 'enprice' ~ 'Energy Prices',
                              variable == 'reer' ~ 'Currency depreciation',
                              variable == 'other' ~ 'Other factors')) %>% 
  filter(year(date) >= 2019) %>% 
  ggplot(aes(x = date)) +
  geom_col(aes(y = contrib, fill = variable)) +
  geom_line(aes(y = cpi)) +
  facet_wrap(~country, scales = 'free') +
  theme_minimal()
  
  



#Modelling Headline Inflation

##Create dataset
data %>% 
  select(-core_infl, -indpro, -r, -gpr) %>% 
  mutate(year = as.factor(year)) %>% 
  ungroup() %>% 
  select(-quarter) %>% 
  group_by(country) %>% 
  
  

mod <- lm(cpi ~ -1 + country + year + cpi_l + enprice + gpr + gdp  + reer + wage + reer*country, 
          data = data) 
coefs <- mod$coefficients

regtable <- tidy(mod) %>% 
  mutate(term = str_remove_all(term, 'country'),
         term = str_remove_all(term, 'year'),
         term = str_replace_all(term, ':', ' x '),
         term = case_when(term == 'cpi_l' ~ 'Past Inflation',
                          term == 'enprice' ~ 'Global Energy Inflation',
                          term == 'gpr' ~ 'Geopolitical Risk',
                          term == 'gdp' ~ 'Output',
                          term == 'reer' ~ 'Exchange Rates',
                          term == 'wage' ~ 'Wage Dynamics',
                          TRUE ~ term),
         term = str_replace_all(term, 'reer', 'Exchange Rates')) %>% 
  rename(Variable = term,
         Coefficient = estimate,
         `S.E.` = std.error,
         `t-statistic` = statistic,
         `P-value` = p.value)

#Retrieve FE
countryfe <- tibble(coef = names(coefs), value = coefs) %>% 
  filter(str_detect(coef, 'country')) %>% 
  mutate(coef = str_remove_all(coef, 'country')) %>% 
  rename(country = coef,
         fe = value)

#Retrieve coefs
variables <- tibble(coef = names(coefs), value = coefs) %>% 
  filter(!str_detect(coef, 'country')) 

coefs <- pull(variables, value)
coefs  

data %>% 
  mutate(cpi_est = predict(mod, data = .))

#Estimate fitted inflation rate 
cpi_est <- data %>% 
  full_join(countryfe) %>% 
  mutate(across(names(coefs), ~.*coefs[as.character(substitute(.))])) %>% 
  mutate(cpi_est = rowSums(pick(-country, -date, -cpi))) %>% 
  select(country, date, cpi, cpi_est)

data %>% 
  full_join(countryfe) %>% 
  mutate(across(names(coefs), ~.*coefs[as.character(substitute(.))])) %>% 
  gather('factor', 'contrib', -country, -date, -cpi) %>% 
  mutate(factor = case_when(factor == 'cpi_l' ~ 'Past Inflation',
                            factor == 'enprice' ~ 'Global Energy Inflation',
                            factor == 'gpr' ~ 'Geopolitical Risk',
                            factor == 'indpro' ~ 'Economic Growth',
                            factor == 'reer' ~ 'Exchange Rates',
                            factor == 'wage' ~ 'Wage Dynamics',
                            factor == 'fe' ~ 'Other factors')) %>% 
  filter(year(date) >= 2020) %>% 
  ggplot() +
  geom_col(aes(x = date, y = contrib, fill = factor)) +
  geom_line(aes(x = date, y = cpi)) +
  facet_wrap(~country, scales = 'free')




