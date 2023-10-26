polrate <- imfr::imf_dataset(database_id = "IFS", 
                             indicator = "FPOLM_PA", 
                             start_year  = 1900,
                             ref_area = countries$ccode2,
                             freq = 'M') %>% 
  as_tibble() %>% 
  filter(freq == 'M') %>% 
  select(date, r = value, ccode2 = ref_area) %>% 
  inner_join(countries, by = 'ccode2') %>% 
  select(country, date, r) %>% 
  mutate(r = as.numeric(r),
         date = paste(date, '01', sep = '-') %>% as_date())


intbank <- OECD::get_dataset("MEI_FIN", filter = list("IR3TIB", c("HUN", "CZE", "EST", 
                                                       "LVA", "LTU", "POL", 
                                                       "SVK", "SVN", "HRV"), "M"),
                  start_time = "1950-01", end_time = substr(Sys.Date(), 1,7)) %>% 
  select(ccode3 = LOCATION,
         r = ObsValue,
         date = Time) %>%
  inner_join(countries, by = 'ccode3') %>% 
  select(country, date, r) %>% 
  mutate(r = as.numeric(r),
         date = paste(date, '01', sep = '-') %>% as_date()) %>% 
  filter(!(country %in% polrate$country))

intrate <- bind_rows(polrate, intbank)

saveRDS(intrate, file.path(fold_data, 'intrate.rds'))
rm(polrate, intbank, intrate)