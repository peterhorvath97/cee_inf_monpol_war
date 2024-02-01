infl <- read_rds(file.path(folder_input, 'infls_eurostat.rds'))

mod <- infl %>% 
  group_by(country, coicop) %>% 
  mutate(value = 100*(value/dplyr::lag(value, 12)-1)) %>% 
  spread(coicop, value) %>% 
  drop_na() %>% 
  ungroup() %>% 
  lm(CP00 ~ -1 + country + as.factor((year(date))) + FOOD +  NRG + TOT_X_NRG_FOOD, 
     data = .) 

summary(mod)
  
infl <- infl %>% 
  group_by(country, coicop) %>% 
  mutate(value = 100*(value/dplyr::lag(value, 12)-1)) %>% 
  spread(coicop, value) %>% 
  drop_na() %>% 
  ungroup() %>% 
  left_join(
tibble(country = names(mod$coefficients),
cfe = mod$coefficients) %>% 
  filter(str_detect(country, 'country')) %>% 
  mutate(country = str_remove_all(country, 'country'))
  ) %>% 
  mutate(year = year(date)) %>% 
  left_join(
tibble(year = names(mod$coefficients),
       yfe = mod$coefficients) %>% 
  filter(str_detect(year, DGT)) %>% 
  mutate(year = str_remove_all(year, 'as\\.factor\\(\\(year\\(date\\)\\)\\)') %>% as.numeric())
) %>% 
  mutate(yfe = replace_na(yfe, 0)) %>% 
  select(-year) %>% 
  gather(var, value, -country, -date, -CP00, -cfe, -yfe) %>% 
  left_join(
tibble(var = names(mod$coefficients),
       coef = mod$coefficients) %>% 
  filter(!str_detect(var, or('country', 'year')))
) %>% 
  mutate(value = value*coef) %>% 
  select(-coef) %>% 
  spread(var, value) %>% 
  group_by(country) %>% 
  #mutate(across(c('CP00', 'TOT_X_NRG_FOOD', 'FOOD', 'NRG', 'ELC_GAS', 'FUEL', 'SERV', 'AP'), 
  #              ~.x - dplyr::lag(.x, 12))) %>% 
  #drop_na() %>% 
  #ungroup() %>% 
  mutate(cpi_fit = cfe + yfe + FOOD + NRG + TOT_X_NRG_FOOD,
         Other = CP00 - cpi_fit - cfe - yfe)  %>% 
  select(-cpi_fit, -yfe, -cfe) %>% 
  gather(var, contrib, -country, -date, -CP00) %>% 
  mutate(var = case_when(var == 'NRG' ~ 'Energy',
                         var == 'FOOD' ~ 'Food',
                         var == 'TOT_X_NRG_FOOD' ~ 'Core Inflation',
                         var == 'Other' ~ 'Other factors')) %>% 
  mutate(var = factor(var, 
                      levels = c('Core Inflation', 'Food', 'Energy', 'Other factors'),
                      labels = c('Core Inflation', 'Food', 'Energy', 'Other factors')),
  var = fct_rev(var)) %>% 
  filter(year(date) >=2019) 

saveRDS(infl, file.path(folder_output, 'determinants of inflation', 'infl_decomp.rds'))


infl %>% 
  filter(country %in% c('Bulgaria', 'Croatia', 'Czechia', 'Estonia')) %>% 
  left_join(countrycode::codelist %>% 
              select(country = country.name.en, ccode2 = iso2c)) %>% 
  mutate(country = ccode2) %>% 
  ggplot(aes(x = date)) +
  geom_col(aes(y = contrib, fill = var), color = 'white', size = .1) +
  geom_line(aes(y = CP00), linewidth = .75) + 
  facet_wrap(~country, scales = 'free_x'
  ) +
  theme_minimal(base_size = 16, base_family = 'Times New Roman') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(breaks = c('Core Inflation', 'Food', 'Energy', 'Other factors'),
                    values = c('#1f78b4', '#33a02c', '#e31a1c', 'grey80')) +
  theme(legend.position = 'bottom',
        legend.title = element_blank()) +
  labs(x = '', y = '') +
  scale_y_continuous(limits = c(-2, 28)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = .75))
  

ggsave(file.path(chartout, 'decomp1.png'), dpi = 1000)


infl %>% 
  filter(country %in% c('Hungary', 'Latvia', 'Lithuania', 'Poland')) %>% 
  left_join(countrycode::codelist %>% 
              select(country = country.name.en, ccode2 = iso2c)) %>% 
  mutate(country = ccode2) %>%  
  ggplot(aes(x = date)) +
  geom_col(aes(y = contrib, fill = var), color = 'white', size = .1) +
  geom_line(aes(y = CP00), linewidth = .75) + 
  facet_wrap(~country, scales = 'free_x'
  ) +
  theme_minimal(base_size = 16, base_family = 'Times New Roman') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(breaks = c('Core Inflation', 'Food', 'Energy', 'Other factors'),
                    values = c('#1f78b4', '#33a02c', '#e31a1c', 'grey80'))  +
  theme(legend.position = 'bottom',
        legend.title = element_blank()) +
  labs(x = '', y = '') +
  scale_y_continuous(limits = c(-2, 28)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = .75))

ggsave(file.path(chartout, 'decomp2.png'), dpi = 1000)

infl %>% 
  filter(country %in% c('Romania', 'Serbia', 'Slovakia', 'Slovenia')) %>% 
  left_join(countrycode::codelist %>% 
              select(country = country.name.en, ccode2 = iso2c)) %>% 
  mutate(country = ccode2) %>%  
  ggplot(aes(x = date)) +
  geom_col(aes(y = contrib, fill = var), color = 'white', size = .1) +
  geom_line(aes(y = CP00), linewidth = .75) + 
  facet_wrap(~country, scales = 'free_x'
  ) +
  theme_minimal(base_size = 16, base_family = 'Times New Roman') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(breaks = c('Core Inflation', 'Food', 'Energy', 'Other factors'),
                    values = c('#1f78b4', '#33a02c', '#e31a1c', 'grey80'))  +
  theme(legend.position = 'bottom',
        legend.title = element_blank()) +
  labs(x = '', y = '') +
  scale_y_continuous(limits = c(-2, 28)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = .75))

ggsave(file.path(chartout, 'decomp3.png'), dpi = 1000)

rm(mod, infl)
