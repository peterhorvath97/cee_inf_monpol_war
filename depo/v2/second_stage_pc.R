#Second stage - Augmented PC curve
data_pc <- data %>% 
  left_join(data_is %>% 
              select(country, date, gdp_hat = gdp_fit)) %>% 
  select(country, date, cpi, cpi_l, gdp_hat, enprice, wage, reer) %>% 
  mutate(year = year(date) %>% as.factor()) %>% 
  drop_na()

data_pc %>% ggplot(aes(x = date, y = cpi)) + geom_line() + facet_wrap(~country, scales = 'free')

#Country FE
pc_cfe <- data_pc %>% 
  group_by(country) %>% 
  summarize(cfe = mean(cpi))

data_pc <- data_pc %>%
  group_by(country) %>% 
  mutate(cpi = cpi - mean(cpi)) %>% 
  ungroup()

#Year FE
pc_yfe <- data_pc %>% 
  group_by(year) %>% 
  summarize(yfe = mean(cpi))

data_pc <- data_pc %>% 
  group_by(year) %>% 
  mutate(cpi = cpi - mean(cpi)) %>% 
  ungroup()

#Country Trend
pc_ctrend <- data_pc %>% 
  group_by(country, year) %>% 
  summarize(ctrend = mean(cpi)) 

data_pc <- data_pc %>% 
  group_by(country, year) %>% 
  mutate(cpi = cpi - mean(cpi)) %>% 
  ungroup()

#Estimate PC curve
mod_pc <- lm(cpi ~ -1 + cpi_l + gdp_hat + enprice + wage + reer, data =data_pc)
tidy(mod_pc)



pc_coef <- tibble(var = names(mod_pc$coefficients),
                  coef = mod_pc$coefficients) 


data_pc <- data_pc %>% 
  left_join(pc_cfe) %>% 
  left_join(pc_yfe) %>% 
 # left_join(pc_ctrend) %>% 
  mutate(cpi_fit = predict(mod_pc) + cfe + yfe,
         cpi = cpi + cfe + yfe )

data_pc %>% 
  ggplot() +
  geom_line(aes(x = date, y = cpi), color = 'black') +
  geom_line(aes(x = date, y = cpi_fit), color = 'red', linetype = 'dashed') +
  facet_wrap(~country, scales = 'free') + 
  theme_minimal() +
  labs(x = '',
       y = '',
       caption = 'The solid line pc the actual quarterly Inflation rate, the dashed line is the fitted one from the Second Stage - Phillips Curve regression')



