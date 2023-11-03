wage <- get_eurostat('lc_lci_r2_q', select_time = 'Q')

wage <- wage %>% 
  filter(s_adj == 'SCA',
         nace_r2 == 'B-S',
         lcstruct == 'D11',
         unit == 'I20') %>%
  select(wage = values,
         date = time,
         ccode2 = geo) %>% 
  inner_join(countries, by = 'ccode2') %>% 
  select(country, date, wage) 


gdp <- get_eurostat('namq_10_gdp', select_time = 'Q') %>% 
  filter(geo %in% ccodes,
         s_adj == 'SCA',
         unit  %in%  c('CP_MEUR', 'CLV15_MEUR'),
         na_item == 'B1GQ') %>% 
  select(ccode2 = geo, date = time, values, unit) %>% 
  mutate(unit = ifelse(unit == 'CP_MEUR', 'Nominal', 'Real')) %>% 
  spread(unit, values) %>% 
  mutate(gdp_defl = Nominal/Real) %>% 
  inner_join(countries) %>% 
  select(country, date, gdp_defl)

wage <- wage %>% 
  mutate(year = year(date),
         is2015 = ifelse(year == 2015, 1, NA)) %>% 
  group_by(country, year) %>% 
  mutate(mwage = mean(wage)*is2015) %>% 
  ungroup(year) %>% 
  mutate(mwage = mean(mwage, na.rm = T),
         wage = 100*wage/mwage) %>% 
  ungroup() %>% 
  select(country, date, wage) %>% 
  inner_join(gdp) %>% 
  mutate(wage = wage/gdp_defl) %>% 
  select(-gdp_defl)
 
wage <- wage %>% 
  group_by(country) %>% 
  mutate(min = min(date),
         max = max(date)) %>% 
  distinct(country, .keep_all = T) %>% 
  select(min, max) %>% 
  mutate(range = seq(min, max, by = '1 month') %>% 
           as.character() %>% 
           paste(collapse = ',')) %>% 
  separate_rows(range, sep = ',') %>% 
  select(country, date = range) %>% 
  mutate(date = as_date(date)) %>% 
  left_join(wage) %>% 
  mutate(year = year(date),
         quarter = quarter(date)) %>% 
  group_by(country, year, quarter) %>% 
  mutate(wage_q = mean(wage, na.rm = T)) %>% 
  ungroup(year, quarter) %>% 
  mutate(wage = zoo::na.spline(wage, na.rm = TRUE)) %>% 
  select(country, date, wage) %>% 
  ungroup()
 

saveRDS(wage, file.path(fold_data, 'wage.rds')) 
rm(wage, gdp)
