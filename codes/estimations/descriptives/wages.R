wages <- read_rds(file.path(folder_input, 'wage.rds')) %>%
  mutate(year = year(date)) %>% 
  group_by(country, year) %>%
  mutate(wage = mean(wage)) %>% 
  distinct(country, year, .keep_all = T) %>% 
  select(-date) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  mutate(base = ifelse(year == 2021, wage, NA),
         base = mean(base, na.rm = T)) %>%  
  mutate(wage = 100*(wage/base-1)) %>% 
  filter(year > 2021) 
  
wages %>% 
  left_join(countrycode::codelist %>% 
              select(country = country.name.en, ccode2 = iso2c)) %>% 
  mutate(country = ccode2) %>% 
  ggplot(aes(x = country, y = wage, fill = factor(year)) ) +
  geom_col(position = 'dodge') +
  theme_minimal(base_size = 16, base_family = 'Times New Roman') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(breaks = c(2022, 2023),
                    values = c('darkred', 'darkblue'))  +
  theme(legend.position = 'bottom',
        legend.title = element_blank()) +
  labs(x = '', y = '') +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = .75))

saveRDS(wages, file.path(folder_output, 'descriptives', 'wages.rds'))

ggsave(file.path(chartout, 'wages.png'), dpi = 1000)
rm(wages)