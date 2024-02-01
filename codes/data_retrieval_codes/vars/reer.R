url <- 'https://www.bis.org/statistics/eer/broad.xlsx'
GET(url, write_disk(file.path(fold_datasource, 'broad.xlsx'), overwrite = T))

reer <- read_excel(file.path(fold_datasource, 'broad.xlsx'),
                   sheet = 'Real',
                   range = 'A5:BM1000') %>% 
  drop_na() %>% 
  rename(date = ...1) %>% 
  mutate(date = as_date(date) %>% 
           as.character(),
         date = substr(date, start = 1, stop = nchar(date)-3) %>% 
           paste('01', sep = '-') %>% 
           as_date()) %>% 
  gather(key = ccode2, value = reer, -date) %>% 
  mutate(ccode2 = substr(ccode2, 3, 4)) %>% 
  inner_join(countries, by = 'ccode2') %>% 
  select(country, date, reer)


saveRDS(reer, file.path(fold_data, 'reer.rds'))
rm(url, reer)

