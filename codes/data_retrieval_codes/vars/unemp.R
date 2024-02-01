unemp <- imfr::imf_dataset(database_id = "IFS", 
                               indicator = "LUR_PT",
                               start_year  = 1900,
                               ref_area = countries$ccode2,
                               freq = 'M') %>% 
  as_tibble() %>% 
  filter(freq == 'M') %>% 
  select(date, unemp = value, ccode2 = ref_area) %>% 
  inner_join(countries, by = 'ccode2') %>% 
  select(country, date, unemp) %>% 
  mutate(unemp = as.numeric(unemp),
         date = paste(date, '01', sep = '-') %>% as_date())

saveRDS(unemp, file.path(fold_data, 'unemp.rds'))
rm(unemp)