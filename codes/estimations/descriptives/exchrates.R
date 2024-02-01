exchrates <- read_rds(file.path(folder_input, 'reer.rds')) %>% 
  arrange(country, date) %>% 
  full_join(read_rds(file.path(folder_input, 'neer.rds')))  %>% 
  gather(var, value, contains('eer')) %>% 
  mutate(year = year(date)) %>% 
  group_by(country, year, var) %>% 
  mutate(value = mean(value)) %>% 
  distinct(country, year, var, .keep_all = T) %>% 
  select(-date) %>% 
  ungroup() %>% 
  group_by(country, var) %>% 
  mutate(base = ifelse(year == 2021, value, NA),
         base = mean(base, na.rm = T)) %>%  
  mutate(value = 100*(value/base-1)) %>% 
  filter(year > 2021) %>%
  mutate(var = ifelse(var == 'neer', 'Nominal', 'Real')) 

saveRDS(exchrates, file.path(folder_output, 'descriptives', 'exchrates.rds'))
  
exchrates %>%  
  left_join(countrycode::codelist %>% 
              select(country = country.name.en, ccode2 = iso2c)) %>% 
  mutate(country = ccode2) %>% 
  ggplot(aes(x = country, y = value, fill = var)) +
  geom_col(position = 'dodge') +
  facet_wrap(~year, ncol = 1, scales = 'free_x') +
  theme_minimal(base_size = 16, base_family = 'Times New Roman') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(breaks = c('Nominal', 'Real'),
                    values = c('darkred', 'darkblue')) +
  theme(legend.position = 'bottom',
        legend.title = element_blank()) +
  labs(x = '', y = 'Percent') +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = .75))


ggsave(file.path(chartout, 'exchrates.pdf'), dpi = 1000, device = cairo_pdf)
rm(exchrates)
