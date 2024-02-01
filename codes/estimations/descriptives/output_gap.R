gdptab  <- read_rds(file.path(folder_input, 'gdp.rds')) %>% 
  mutate(year = year(date)) %>% 
  group_by(country, year) %>% 
  summarize(gdp = mean(gdp)) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  mutate(gdp = 100*(gdp/lag(gdp)-1)) %>% 
  filter(year >= 2020) %>% 
  mutate(gdp = round(gdp, 1)) %>% 
  mutate(gdp = paste(gdp, '%', sep = '')) %>% 
  spread(year, gdp)

saveRDS(gdptab, file.path(folder_output, 'descriptives', 'gdptab.rds'))

write_csv2(gdptab, file.path(tabout, 'gdptab.csv'))
rm(gdptab)