ratehikes <- read_rds(file.path(folder_input, 'intrate.rds')) %>% 
  filter(year(date) >= 2022) %>%
  group_by(country) %>% 
  mutate(r = r-lag(r)) %>% 
  drop_na() %>% 
  mutate(r = accumulate(r, sum)) %>% 
  group_by(country) %>% 
  filter(date == max(date))  %>% 
  arrange(desc(r)) 

ratehikes  %>%     
  left_join(countrycode::codelist %>% 
              select(country = country.name.en, ccode2 = iso2c)) %>% 
  mutate(country = ccode2) %>% 
  ggplot(aes(x = fct_reorder(country, r), y = r, fill = r)) +
  geom_col() +
  coord_flip() +
  theme_minimal(base_size = 16, base_family = 'Times New Roman') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x = '', 
       y = 'Percentage points') +
  scale_fill_distiller(direction = 1) + 
  theme(legend.position = 'none') +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = .75))

saveRDS(ratehikes, file.path(folder_output, 'descriptives', 'ratehikes.rds'))

ggsave(file.path(chartout, 'ratehikes.pdf'), dpi = 1000, device = cairo_pdf)
rm(ratehikes)
