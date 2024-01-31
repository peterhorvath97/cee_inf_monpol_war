intrate <- read_rds(file.path(folder_input, 'intrate.rds'))
inflation <- read_rds(file.path(folder_input, 'infls_eurostat.rds')) %>% 
  spread(coicop, value) %>% 
  select(country, date, cpi = CP00)


real_rate <- intrate %>% 
  left_join(inflation) %>% 
  drop_na() %>% 
  mutate(year = year(date),
         quarter = quarter(date)) %>% 
  group_by(country, year, quarter) %>% 
  mutate(r = mean(r),
         cpi = mean(cpi)) %>% 
  mutate(month = month(date)) %>% 
  filter(month %in% c(1, 4, 7, 10)) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  mutate(cpi = 100*(cpi/lag(cpi, 4)-1)) %>% 
  drop_na() %>% 
  mutate(r = r - lead(cpi)) %>% 
  select(country, date, r) %>% 
  filter(country != 'Romania' | (country == 'Romania' & year(date) >= 1999))

source(file.path(folder_estcodes, 'real rate', 'hp_one_sided.R'))

real_rate <- real_rate %>% 
  drop_na() %>% 
  group_by(country) %>% 
  mutate(r_trend1 = hp_one_sided(ts(r), 12, 12)$trend,
         r_cycle1 = hp_one_sided(ts(r), 12, 12)$cycle,
         r_trend2 = mFilter::hpfilter(ts(r), 12)$trend %>% as.numeric(),
         r_cycle2 = mFilter::hpfilter(ts(r), 12)$cycle %>% as.numeric())

saveRDS(real_rate, file.path(folder_output, 'real rate', 'real_rate.rds'))


real_rate %>% 
  select(country, date, req = r_trend1) %>% 
  left_join(countrycode::codelist %>% 
              select(country = country.name.en, ccode2 = iso2c)) %>% 
  mutate(country = ccode2) %>%   
  ggplot(aes(x = date, y = req)) +
  geom_line(linewidth = .75) +
  facet_wrap(~country, ncol = 2, scales = 'free_x') +
  theme_minimal(base_size = 16, base_family = 'Times New Roman') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  labs(x = '',
       y = '') +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = .75))  

ggsave(file.path(chartout, 'eqrates.png'), dpi = 1000)



real_rate %>% 
  select(country, date, rgap = r_cycle1) %>% 
  filter(year(date) >= 2021) %>% 
  left_join(countrycode::codelist %>% 
              select(country = country.name.en, ccode2 = iso2c)) %>% 
  mutate(country = ccode2) %>%  
  ggplot(aes(x = date, y = rgap)) +
  geom_line(linewidth = .75) +
  geom_hline(aes(yintercept = 0), color = 'red') +
  facet_wrap(~country, ncol = 2, scales = 'free_x') +
  theme_minimal(base_size = 16, base_family = 'Times New Roman') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  labs(x = '',
       y = '') +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = .75))

ggsave(file.path(chartout, 'rgap.png'), dpi = 1000)

rm(intrate, inflation, real_rate)