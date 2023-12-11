source(file.path(folder_estcodes, 'real rate', 'hp_one_sided.R'))

read_rds(file.path(folder_input, 'gdp.rds')) %>% 
  group_by(country) %>% 
  mutate(gap2 = mFilter::hpfilter(ts(gdp), freq = 4 )$cycle,
         gap1 = hp_one_sided(ts(gdp), 4, 4)$cycle) %>% 
  mutate(gdp = gdp-lag(gdp, 4)) %>% 
  filter(year(date) >= 2020) %>% 
  gather(side, value, starts_with('gap'), gdp) %>% 
  mutate(side = case_when(side == 'gdp' ~ 'GDP growth',
                          side == 'gap1' ~ 'Output gap (One-Sided)',
                          side == 'gap2' ~ 'Output gap (Two-Sided)')) %>% 
  saveRDS(file.path(folder_output, 'descriptives', 'output_gap.rds'))

read_rds(file.path(folder_output, 'descriptives', 'output_gap.rds')) %>%    
  ggplot(aes(x = date, y = value, color = side)) +
  geom_line(linewidth = .75) + 
  facet_wrap(~country, scales = 'free') +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  labs(x = '',
       y= '')

ggsave(file.path(folder_output, 'descriptives', 'output_gap.png'), dpi = 1000)
