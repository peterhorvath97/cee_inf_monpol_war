read_rds(file.path(folder_input, 'wage.rds')) %>% 
  arrange(country, date) %>% 
  drop_na() %>% 
  mutate(year = year(date),
         month = month(date),
         is2022 = ifelse(year == 2022, 1, NA),
         isjan = ifelse(month == 1, 1, NA),
         baseind = is2022*isjan,
         base = baseind*wage) %>% 
  group_by(country) %>% 
  mutate(base = mean(base, na.rm = T),
         wage = 100*wage/base) %>% 
  filter(date == max(date)) %>% 
  saveRDS(file.path(folder_output, 'descriptives', 'wages.rds'))

read_rds(file.path(folder_output, 'descriptives', 'wages.rds')) %>%     
  ggplot(aes(x = fct_reorder(country, -(wage-100)), y = wage-100, fill = wage-100)) + 
  geom_col() +
  theme_minimal() +
  theme(legend.position = 'none') +
  labs(x = '',
       y = '') +
  scale_fill_distiller(direction = -1)
  

ggsave(file.path(folder_output, 'descriptives', 'wages.png'), dpi = 1000)

  
  
  
  

