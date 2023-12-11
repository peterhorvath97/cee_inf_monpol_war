read_rds(file.path(folder_input, 'reer.rds')) %>% 
  arrange(country, date) %>% 
  full_join(read_rds(file.path(folder_input, 'neer.rds')))  %>% 
  gather(var, value, contains('eer')) %>% 
  mutate(year = year(date),
         month = month(date),
         is.2022 = ifelse(year == 2022, 1, NA),
         is.jan = ifelse(month == 1, 1, NA),
         baseind = is.2022*is.jan,
         base = value*baseind) %>% 
  group_by(country, var) %>% 
  mutate(base = mean(base, na.rm = TRUE),
         value = 100*value/base) %>% 
  ungroup() %>% 
  select(country, date, var, value) %>% 
  filter(year(date) >= 2022) %>% 
  mutate(var = ifelse(var == 'reer', 'Real', 'Nominal')) %>% 
  saveRDS(file.path(folder_output, 'descriptives', 'exchrates.rds'))

read_rds(file.path(folder_output, 'descriptives', 'exchrates.rds')) %>%     
  ggplot(aes(x = date, y = value, color = var)) +
  geom_line(linewidth = .75) + 
  facet_wrap(~country) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  labs(x = '',
       y = '') +
  scale_color_manual(values = c('Red', 'Blue'))

ggsave(file.path(folder_output, 'descriptives', 'exchrates.png'), dpi = 1000)


