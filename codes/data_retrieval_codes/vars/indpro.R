indpro <- imfr::imf_dataset(database_id = "IFS", 
                            indicator = "AIP_IX", 
                            start_year  = 1900,
                            ref_area = countries$ccode2,
                            freq = 'M') %>% 
  as_tibble() %>% 
  filter(freq == 'M') %>% 
  select(date, indpro = value, ccode2 = ref_area) %>% 
  inner_join(countries, by = 'ccode2') %>% 
  select(country, date, indpro) %>% 
  mutate(indpro = as.numeric(indpro),
         date = paste(date, '01', sep = '-') %>% as_date()) %>% 
  group_by(country) %>% 
  drop_na() %>% 
  mutate(indpro = x13adj(indpro, 12, date))

saveRDS(indpro, file.path(fold_data, 'indpro.rds'))
rm(indpro)