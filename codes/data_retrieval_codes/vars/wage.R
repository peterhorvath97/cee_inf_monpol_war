wage <- get_eurostat('lc_lci_r2_q', select_time = 'Q')

wage <- wage %>% 
  filter(s_adj == 'SCA',
         nace_r2 == 'B-S',
         lcstruct == 'D11',
         unit == 'I20') %>%
  select(wage = values,
         date = time,
         ccode2 = geo) %>% 
  inner_join(countries, by = 'ccode2') %>% 
  select(country, date, wage) 

saveRDS(wage, file.path(fold_data, 'wage.rds')) 
rm(wage)
