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

real_rate %>% 
  ggplot(aes(x = date, y = r)) +
  geom_line(linewidth = .75)+
  facet_wrap(~country, scales = 'free_x') +
  theme_minimal() +
  labs(x = '', 
       y = '')

ggsave(file.path(folder_output, 'real rate', 'real_rates.png'), dpi = 1000)

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
  select(country, date, r_trend1, r_trend2) %>% 
  gather(filter, value, starts_with('r')) %>% 
  mutate(filter = case_when(filter == 'r_trend1' ~ 'One-sided HP',
                            filter == 'r_trend2' ~ 'Two-sided HP')) %>% 
  ggplot(aes(x = date, y = value, color = filter)) +
  geom_line(linewidth = .75) +
  facet_wrap(~country, scales = 'free') +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  labs(x = '',
       y = '') +
  scale_color_manual(breaks = c('One-sided HP', 'Two-sided HP'),
                     values = c('red', 'darkblue'))

ggsave(file.path(folder_output, 'real rate', 'real_rate_trends.png'), dpi = 1000)


real_rate %>% 
  select(country, date, r_cycle1, r_cycle2) %>% 
  gather(filter, value, starts_with('r')) %>% 
  mutate(filter = case_when(filter == 'r_cycle1' ~ 'One-sided HP',
                            filter == 'r_cycle2' ~ 'Two-sided HP')) %>% 
  ggplot(aes(x = date, y = value, color = filter)) +
  geom_line(linewidth = .75) +
  facet_wrap(~country, scales = 'free') +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  labs(x = '',
       y = '') +
  scale_color_manual(breaks = c('One-sided HP', 'Two-sided HP'),
                     values = c('red', 'darkblue'))

ggsave(file.path(folder_output, 'real rate', 'real_rate_deviations.png'), dpi = 1000)

