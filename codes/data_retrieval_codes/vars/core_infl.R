core_infl <- get_eurostat('prc_hicp_midx', select_time = 'M') %>% 
  filter(coicop == 'TOT_X_NRG_FOOD',
         unit == 'I15') %>% 
  select(ccode2 = geo,
         core_infl = values,
         date = TIME_PERIOD) %>% 
  inner_join(countries, by = 'ccode2') %>% 
  select(country, date, core_infl) 

saveRDS(core_infl, file.path(fold_data, 'core_infl.rds')) 
rm(core_infl)