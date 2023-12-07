gdp <- get_eurostat('namq_10_gdp', select_time = 'Q') %>% 
  filter(geo %in% ccodes,
         s_adj == 'SCA',
         unit  =='CLV15_MNAC',
         na_item == 'B1GQ') %>% 
  select(ccode2 = geo, date = time, gdp = values) %>% 
  inner_join(countries, by = 'ccode2') %>% 
  select(country, date, gdp) %>% 
  arrange(country, date)

gdp <- gdp %>% 
  mutate(year = year(date)) %>% 
  group_by(country, year) %>% 
  mutate(mgdp = mean(gdp),
         is2015 = ifelse(year == 2015, 1, NA),
         mgdp = mgdp*is2015) %>% 
  ungroup(year) %>% 
  mutate(mgdp = mean(mgdp, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(gdp = 100*gdp/mgdp) %>% 
  select(country, date, gdp)


saveRDS(gdp, file.path(fold_data, 'gdp.rds'))
rm(gdp)


