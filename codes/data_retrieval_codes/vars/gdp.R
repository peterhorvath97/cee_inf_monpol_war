gdp <- imfr::imf_dataset(database_id = "IFS", 
                         indicator = "NGDP_R_SA_XDC",
                         start_year  = 1900,
                         ref_area = countries$ccode2,
                         freq = 'Q') %>% 
  as_tibble() %>% 
  filter(freq == 'Q') %>% 
  select(date, gdp = value, ccode2 = ref_area) %>% 
  inner_join(countries, by = 'ccode2') %>% 
  select(country, date, gdp) %>% 
  mutate(gdp = as.numeric(gdp),
         date = paste(date, '01', sep = '-') %>% as_date())

saveRDS(gdp, file.path(fold_data, 'gdp.rds'))
rm(gdp)