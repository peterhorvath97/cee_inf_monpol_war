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
  filter(coicop  %in%  c('CP00', 'TOT_X_NRG_FOOD', 'FOOD', 'NRG')) 

saveRDS(infl, file.path(fold_data, 'infls_eurostat.rds'))
rm(infl)