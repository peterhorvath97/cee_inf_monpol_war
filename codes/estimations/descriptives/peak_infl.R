read_rds(file.path(folder_input, 'infls_eurostat.rds')) %>% 
  filter(year(date) > 2020) %>% 
  group_by(country, coicop) %>% 
  mutate(value = value - lag(value, 12)) %>% 
  drop_na() %>% 
  ungroup(coicop) %>% 
  spread(coicop, value) %>% 
  filter(CP00 == max(CP00)) %>% 
  gather(key = coicop, value = value, -country, -date) %>% 
  mutate(date = date %>% 
           as.character() %>% 
           str_remove_all('-01' %R% END)) %>%  
  mutate(coicop = case_when(coicop == 'CP00' ~ 'Headline',
                            coicop == 'TOT_X_NRG_FOOD' ~ 'Core',
                            coicop == 'NRG' ~ 'Energy',
                            coicop == 'FOOD' ~ 'Food'),
         coicop = factor(coicop, levels = c('Headline', 'Core', 'Food', 'Energy'))) %>% 
  saveRDS(file.path(folder_output, 'descriptives', 'peak_inflation.rds'))
  
read_rds(file.path(folder_output, 'descriptives', 'peak_inflation.rds')) %>%    
  ggplot(aes(x = country, y = value, fill = coicop, group = coicop)) +
  geom_col(position = 'dodge', color = 'black') +
  geom_text(aes(y = -5, label = date , angle = 30)) +
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.title = element_blank()) +
  labs(x = '',
       y = '')


ggsave(file.path(folder_output, 'descriptives', 'peak_inflation.png'), dpi = 1000)
