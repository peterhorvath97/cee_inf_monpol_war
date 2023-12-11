read_rds(file.path(folder_input, 'intrate.rds')) %>% 
  filter(year(date) >= 2022) %>% 
  mutate(r = r-lag(r)) %>% 
  drop_na() %>% 
  mutate(r = accumulate(r, sum)) %>% 
  group_by(country) %>% 
  filter(date == max(date))  %>% 
  arrange(desc(r)) %>% 
  saveRDS(file.path(folder_output, 'descriptives', 'ratehikes.rds'))

read_rds(file.path(folder_output, 'descriptives', 'ratehikes.rds')) %>%     
  ggplot(aes(x = fct_reorder(country, r), y = r, fill = r)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(x = '', 
       y = 'Accumulated change in interest rates since 2022') +
  scale_fill_distiller(direction = 1) + 
  theme(legend.position = 'none')

ggsave(file.path(folder_output, 'descriptives', 'ratehikes.png'), dpi = 1000)

  

