sheet <- excel_sheets(file.path(folder_output, 'svar shocks', 'OUTPUT.xlsx'))

suppressMessages(
  svar_shocks <- lapply(setNames(sheet, sheet),
                       function(x) read_excel(file.path(folder_output, 'svar shocks', 'OUTPUT.xlsx'), sheet = x) %>% mutate(country = x)) %>% 
    bind_rows() %>% 
    mutate(SERIES12 = ifelse(is.na(SERIES12) & !is.na(SER12), SER12, SERIES12),
           SERIES12_1 = ifelse(is.na(SERIES12_1) & !is.na(SER12_1), SER12_1, SERIES12_1),
           SERIES12_2 = ifelse(is.na(SERIES12_2) & !is.na(SER12_2), SER12_2, SERIES12_2)) %>% 
    rename(date = ...1,
           Shock = SERIES12,
           Demand = SERIES12_1,
           Supply = SERIES12_2) %>%
    select(country, date, Shock, Demand, Supply) %>% 
    mutate(date = case_when(substr(date, nchar(date), nchar(date)) == '1' ~ paste(substr(date, 1,4), '01', '01', sep = '-'),
                            substr(date, nchar(date), nchar(date)) == '2' ~ paste(substr(date, 1,4), '04', '01', sep = '-'),
                            substr(date, nchar(date), nchar(date)) == '3' ~ paste(substr(date, 1,4), '07', '01', sep = '-'),
                            substr(date, nchar(date), nchar(date)) == '4' ~ paste(substr(date, 1,4), '10', '01', sep = '-')) %>% 
             as_date()) %>% 
    drop_na() %>% 
    filter(year(date) >= 2015) %>% 
    gather(key = contrib, value = value, Supply, Demand) %>% 
    mutate(contrib = factor(contrib, levels = c('Supply', 'Demand'), labels = c('Supply', 'Demand')))
  
  
)
       

svar_shocks %>% 
  filter(country %in% c('BG', 'HR', 'CZ', 'EE')) %>% 
  ggplot(aes(x = date)) +
  geom_col(aes(y = value, fill = contrib), color = 'white', size = .1) + 
  geom_line(aes(y = Shock), color = 'darkblue', linewidth = .75) +
  geom_hline(aes(yintercept = 0), color = 'black') +
  facet_wrap(~country, scales = 'free_x') +
  scale_fill_manual(breaks = c('Supply', 'Demand'),
                    values = c('#FF5A5A', '#94BEFF')) +
  theme_minimal(base_size = 16, base_family = 'Times New Roman') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = 'bottom',
        legend.title = element_blank()) +
  labs(x = '', y = '') +
  scale_x_date(limits = as.Date(c("2015-01-01","2023-12-01")))+
  scale_y_continuous(limits = c(-0.03, 0.08)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = .75))

ggsave(file.path(chartout, 'svar1.png'), dpi = 1000)

svar_shocks %>% 
  filter(country %in% c('HU', 'LV', 'LT', 'PL')) %>% 
  ggplot(aes(x = date)) +
  geom_col(aes(y = value, fill = contrib), color = 'white', size = .1) + 
  geom_line(aes(y = Shock), color = 'darkblue', linewidth = .75) +
  geom_hline(aes(yintercept = 0), color = 'black') +
  facet_wrap(~country, scales = 'free_x') +
  scale_fill_manual(breaks = c('Supply', 'Demand'),
                    values = c('#FF5A5A', '#94BEFF')) +
  theme_minimal(base_size = 16, base_family = 'Times New Roman') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = 'bottom',
        legend.title = element_blank()) +
  labs(x = '', y = '') +
  scale_x_date(limits = as.Date(c("2015-01-01","2023-12-01")))+
  scale_y_continuous(limits = c(-0.03, 0.08)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = .75))

ggsave(file.path(chartout, 'svar2.png'), dpi = 1000)

   
svar_shocks %>% 
  filter(country %in% c('RO', 'RS', 'SK', 'SI')) %>% 
  ggplot(aes(x = date)) +
  geom_col(aes(y = value, fill = contrib), color = 'white', size = .1) + 
  geom_line(aes(y = Shock), color = 'darkblue', linewidth = .75) +
  geom_hline(aes(yintercept = 0), color = 'black') +
  facet_wrap(~country, scales = 'free_x') +
  scale_fill_manual(breaks = c('Supply', 'Demand'),
                    values = c('#FF5A5A', '#94BEFF')) +
  theme_minimal(base_size = 16, base_family = 'Times New Roman') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = 'bottom',
        legend.title = element_blank()) +
  labs(x = '', y = '') +
  scale_x_date(limits = as.Date(c("2015-01-01","2023-12-01")))+
  scale_y_continuous(limits = c(-0.03, 0.08)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = .75))

ggsave(file.path(chartout, 'svar3.png'), dpi = 1000)

rm(sheet, svar_shocks)