totw <- eurostat::get_eurostat("NAMQ_10_A10" %>% tolower(), select_time = "Q") %>% 
  filter(geo %in%  ccodes,
         s_adj == "SCA",
         na_item == "D11",
         nace_r2 == "TOTAL",
         unit == "CP_MEUR") %>% 
  select(-s_adj, - na_item, - nace_r2, -unit) %>% 
  mutate(values = values * 1000000)

totw %>% 
  filter(geo == 'RS') %>% drop_na() %>% 
  janitor::tabyl(na_item)
         s_adj == "SCA",
         na_item == "D11",
         nace_r2 == "TOTAL",
         unit == "CP_MEUR") %>% drop_na()
  janitor::tabyl(nace_r2) %>% 
  drop_na()

totemp <- eurostat::get_eurostat("NAMQ_10_A10_E" %>% tolower(), select_time = "Q") %>% 
  filter(geo %in% ccodes,
         s_adj %in% c("SA", "SCA"),
         nace_r2 == "TOTAL",
         na_item == "SAL_DC",
         unit == "THS_PER"
  ) %>% 
  select(-s_adj, -nace_r2, -na_item, -unit) %>% 
  mutate(values = values * 1000) %>% 
  distinct(geo, time, .keep_all = TRUE)

wages <- totw %>% 
  inner_join(totemp, by = c('geo', 'time')) %>% 
  mutate(wage = values.x / values.y) %>% 
  select(ccode2 = geo,
         date = time, 
         wage) %>% 
  drop_na() %>% 
  inner_join(countries) %>% 
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
  


rwage <- wages %>% 
  inner_join(gdp) %>% 
  mutate(rwage = wage / gdp_defl) %>% 
  select(country, date, rwage)


rwage %>% 
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
  left_join(rwage) %>% 
  mutate(year = year(date),
         quarter = quarter(date)) %>% 
  group_by(country, year, quarter) %>% 
  mutate(rwage_q = mean(rwage, na.rm = T)) %>% 
  ungroup(year, quarter) %>% 
  mutate(rwage = lag(rwage/3),
         rwage = zoo::na.spline(rwage, na.rm = TRUE)) %>% 
  ggplot() +
  geom_line(aes(x = date, y = rwage), color = 'red')+
  #geom_line(aes(x = date, y = rwage_q/3), color = 'blue') +
  facet_wrap(~country, scales = 'free')

totw %>% 
  filter(geo == 'RS') %>% 
  drop_na()
gdp  %>% 
  filter(country == 'Serbia')

countries$country 
unique(rwage$country)