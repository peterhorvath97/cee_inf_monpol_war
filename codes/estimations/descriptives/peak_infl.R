peak_infls <- read_rds(file.path(folder_input, 'infls_eurostat.rds')) %>% 
  filter(coicop == 'CP00',
         year(date) > 2020) %>% 
  group_by(country) %>% 
  mutate(value = 100*(value/lag(value, 12)-1)) %>% 
  drop_na() %>% 
  filter(value == max(value)) %>% 
  select(country, date, cpi = value) %>% 
  mutate(date = date %>% 
           as.character() %>% 
           str_remove_all('-01' %R% END)) 

saveRDS(peak_infls, file.path(folder_output, 'descriptives', 'peak_infls.rds'))

write_csv2(peak_infls, file.path(tabout, 'peak_infls.csv'))
rm(peak_infls)