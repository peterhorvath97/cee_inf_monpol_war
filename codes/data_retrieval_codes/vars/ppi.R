ppi <- imfr::imf_dataset(database_id = "IFS", 
                               indicator = "PPPI_IX",
                               start_year  = 1900,
                               ref_area = countries$ccode2,
                               freq = 'M') %>% 
  as_tibble() %>% 
  filter(freq == 'M') %>% 
  select(date, ppi = value, ccode2 = ref_area) %>% 
  inner_join(countries, by = 'ccode2') %>% 
  select(country, date, ppi) %>% 
  mutate(ppi = as.numeric(ppi),
         date = paste(date, '01', sep = '-') %>% as_date())

saveRDS(ppi, file.path(fold_data, 'ppi.rds'))
rm(ppi)