#Load data series           
ds <- lapply(paste(file.path('data', 'input'), list.files(file.path('data','input')), sep = '/') , read_rds)
names(ds) <- str_remove_all(list.files(file.path('data','input')), '.rds')

data <- ds[[1]]
for(i in 2:length(ds)){
  data <- data %>% 
    full_join(ds[[i]])
}

data <- data %>% 
  full_join(read_rds('data/output/exp_infl.rds'))


#Quarterly Freq
data <- data %>% 
  arrange(country, date) %>%  
  mutate(year = year(date),
         quarter = quarter(date)) %>% 
  group_by(country, year, quarter) %>% 
  mutate(across(-date, ~mean(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(month = month(date)) %>% 
  filter(month %in%  c(1, 4, 7, 10)) %>% 
  select(-month, -quarter, -year) %>% 
  mutate(#enprice = enprice - dplyr::lag(enprice, 4),
         gdp = gdp - dplyr::lag(gdp, 4),
         cpi = cpi - dplyr::lag(cpi, 4),
         cpi_l = dplyr::lag(cpi),
         core_infl = core_infl - dplyr::lag(core_infl, 4),
         core_infl_l = dplyr::lag(core_infl),
         exp_infl = exp_infl - dplyr::lag(exp_infl, 4),
         exp_infl = dplyr::lead(exp_infl, 12),
         reer = reer - dplyr::lag(reer, 4),
         wage = wage - dplyr::lag(wage, 4)
  ) 


rm(ds)
print(data)