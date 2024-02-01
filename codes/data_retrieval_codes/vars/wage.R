wage <- eurostat::get_eurostat("NAMQ_10_A10" %>% tolower(), select_time = "Q") %>% 
  filter(geo %in% ccodes,
         unit == 'CP_MEUR',
         s_adj == 'SCA',
         na_item == 'D11',
         nace_r2 %in% c('B-E', 'F', 'G-I', 'J', 'K', 'L', 'M_N')) %>% 
  select(ccode2 = geo,
         date = TIME_PERIOD,
         values) %>%
  group_by(ccode2, date) %>% 
  summarize(values = sum(values)) %>%
  ungroup() %>% 
  mutate(values = values * 1000000) %>% 
  rename(wage = values)

empl <- eurostat::get_eurostat("NAMQ_10_A10_E" %>% tolower(), select_time = "Q") %>% 
  filter(geo %in% ccodes,
         s_adj == 'SCA',
         na_item == 'EMP_DC',
         unit == 'THS_HW',
         nace_r2 %in% c('B-E', 'F', 'G-I', 'J', 'K', 'L', 'M_N')) %>% 
  select(ccode2 = geo,
         date = TIME_PERIOD,
         values) %>%
  group_by(ccode2, date) %>% 
  summarize(values = sum(values)) %>%
  ungroup() %>% 
  mutate(values = values * 1000) %>% 
  rename(hw = values)

wage <- wage %>% 
  inner_join(empl) %>% 
  mutate(wage = wage/hw) %>% 
  inner_join(countries, by = 'ccode2') %>% 
  select(country, date, wage)


gdp <- get_eurostat('namq_10_gdp', select_time = 'Q') %>% 
  filter(geo %in% ccodes,
         s_adj == 'SCA',
         unit  %in%  c('CP_MNAC', 'CLV15_MNAC'),
         na_item == 'B1GQ') %>% 
  select(ccode2 = geo, date = TIME_PERIOD, values, unit) %>% 
  mutate(unit = ifelse(unit == 'CP_MNAC', 'Nominal', 'Real')) %>% 
  spread(unit, values) %>% 
  mutate(gdp_defl = Nominal/Real) %>% 
  inner_join(countries) %>% 
  select(country, date, gdp_defl)

wage <- wage %>% 
  inner_join(gdp) %>% 
  mutate(wage = wage / gdp_defl) %>% 
  mutate(year = year(date),
         is2015 = ifelse(year == 2015, 1, NA)) %>% 
  group_by(country, year) %>% 
  mutate(mwage = mean(wage)*is2015) %>% 
  ungroup(year) %>% 
  mutate(mwage = mean(mwage, na.rm = T),
         wage = 100*wage/mwage) %>% 
  ungroup() %>% 
  select(country, date, wage) %>% 
  filter(year(date) >= 2001)  
  
saveRDS(wage, file.path(fold_data, 'wage.rds')) 
rm(wage, empl, gdp)
  
