peak_infls <- read_rds(file.path(folder_input, 'infls_eurostat.rds')) %>% 
  filter(coicop == 'CP00',
         year(date) > 2020) %>% 
  group_by(country) %>% 
  mutate(value = 100*(value/lag(value, 12)-1)) %>% 
  drop_na() %>% 
  filter(value == max(value)) %>% 
  select(country, date, cpi = value) %>% 
  arrange(desc(cpi)) %>% 
  mutate(date = date %>% 
           as.character() %>% 
           str_remove_all('-01' %R% END),
         cpi = round(cpi, 1)) 

saveRDS(peak_infls, file.path(folder_output, 'descriptives', 'peak_infls.rds'))

write_csv2(peak_infls, file.path(tabout, 'peak_infls.csv'))
rm(peak_infls)

read_rds(file.path(folder_input, 'infls_eurostat.rds')) %>% 
  filter(coicop == 'CP00') %>% 
  select(country, date, cpi = value) %>%
  group_by(country) %>% 
  mutate(cpi = 100*(cpi/lag(cpi,12)-1)) %>% 
  filter(year(date) >= 2019) %>%  
  left_join(countrycode::codelist %>% 
              select(country = country.name.en, ccode2 = iso2c)) %>% 
  mutate(country = ccode2) %>%
  ggplot(aes(x = date, y = cpi, color = country)) +
  geom_line(linewidth = .75) +
  scale_color_manual(values = c("#FF5733", 
                                "#2E7D32", 
                                "#3498DB", 
                                "#FFC107", 
                                "#9C27B0", 
                                "#E91E63", 
                                "#795548", 
                                "#00BCD4", 
                                "#FF9800", 
                                "#8BC34A", 
                                "#607D8B", 
                                "#9E9E9E")) +
  theme_minimal(base_size = 16, base_family = 'Times New Roman') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(#legend.position = 'bottom',
    legend.title = element_blank()) +
  labs(x = '', y = 'Percent') +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = .75))  

ggsave(file.path(chartout, 'infls.pdf'), dpi = 1000, device = cairo_pdf)
