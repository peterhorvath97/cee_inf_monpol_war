inflation <- imfr::imf_dataset(database_id = "IFS", 
                               indicator = "PCPI_IX",
                               start_year  = 1900,
                               ref_area = countries$ccode2,
                               freq = 'M') %>% 
  as_tibble() %>% 
  filter(freq == 'M') %>% 
  select(date, cpi = value, ccode2 = ref_area) %>% 
  inner_join(countries, by = 'ccode2') %>% 
  select(country, date, cpi) %>% 
  mutate(cpi = as.numeric(cpi),
         date = paste(date, '01', sep = '-') %>% as_date())

saveRDS(inflation, file.path(fold_data, 'inflation.rds'))
rm(inflation)