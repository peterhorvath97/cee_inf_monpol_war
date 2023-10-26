enprice <- imfr::imf_dataset(database_id = 'PCPS',
                  commodity = 'PNRG',
                  start_year = 1900,
                  ref_area = 'W00',
                  freq = 'M',
                  unit_measure = 'IX') %>% 
  as_tibble() %>% 
  select(date, 
         enprice = value) %>% 
  mutate(enprice = as.numeric(enprice),
         date = paste(date, '01', sep = '-') %>% as_date())

saveRDS(enprice, file.path(fold_data, 'enprice.rds'))
rm(enprice)