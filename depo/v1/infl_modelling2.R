library(tidyverse)
library(fredr)
library(lubridate)

#data issues
df_all <- read_rds("df_all.rds")
month_quarter <- tibble(month = 1:12) %>% 
  mutate(quarter = case_when(month %in% 1:3 ~ 1,
                             month %in% 4:6 ~ 2,
                             month %in% 7:9 ~ 3,
                             month %in% 10:12 ~ 4))

data <-df_all %>% group_by(iso2c) %>% nest() %>% 
  mutate(data = case_when(iso2c %in% c("BG", "CZ", "HU", "PL", "RO") ~
                            map(data, ~filter(.x, description %in% c("CPI", "GDP", "Policy rate", 
                                                                    "Real Effective Exchange Rate", "Wages and salaries, Index, B-S"))),
                          iso2c %in% c("EE", "LV", "SI", "SK") ~ 
                            map(data, ~filter(.x, description %in% c("CPI", "GDP", "Short term interest rate", 
                                                                    "Real Effective Exchange Rate", "Wages and salaries, Index, B-S"))),
                          iso2c == "LT" ~
                            map(data, ~filter(.x, description %in% c("CPI", "GDP", "Treasury rate", 
                                                                    "Real Effective Exchange Rate", "Wages and salaries, Index, B-S"))),
                          iso2c == "Global" ~
                            map(data, ~filter(.x, description %in% c("Energy Price Index", "Geopolitical Risk Index")) %>% mutate(freq = "M"))
                            ) 
         ) %>% 
  mutate(data = map(data, ~mutate(.x, year = year(date),
                                  month = month(date)) %>% 
                      left_join(month_quarter, by = "month") %>% 
                      group_by(indicator, quarter, year) %>% 
                      mutate(val = mean(value),
                             name = case_when(description == "CPI" ~ "pi", 
                                              description == "GDP" ~ "y",
                                              description == "Policy rate" ~ "r",
                                              description == "Short term interest rate" ~ "r",
                                              description == "Treasury rate" ~ "r",
                                              description == "Real Effective Exchange Rate" ~ "e",
                                              description == "Wages and salaries, Index, B-S" ~ "w",
                                              description == "Energy Price Index" ~ "gep",
                                              description == "Geopolitical Risk Index" ~  "gpr")) %>% 
                      ungroup() %>% 
                      distinct(indicator, quarter, year, .keep_all = TRUE) %>% 
                      dplyr::select(date, val, name) %>% 
                      spread(name, val) %>% 
                      drop_na()
                    ))

global <- data %>% filter(iso2c == "Global") %>% unnest(data) %>% ungroup %>% dplyr::select(-iso2c)

data <- data %>% filter(iso2c != "Global") %>% 
  mutate(data = map(data, ~inner_join(.x, global, by = "date"))) %>% 
  unnest(data)


data <- data %>% mutate(year = year(date)) %>% 
  left_join(data %>% mutate(year = year(date)) %>% 
                              group_by(iso2c, year) %>% 
                              summarize(mean_y = mean(y)) %>% 
                              filter(year == 2010) %>% ungroup(),
                            by = "iso2c") %>% 
  mutate(y = 100*y/mean_y) %>% 
  dplyr::select(-starts_with('year'), -mean_y) %>% 
  rename(country = iso2c) %>% ungroup()

 




#country-wise var attempt
library(vars)
amat <- matrix(nrow = 7, ncol = 7, 0)
amat[lower.tri(amat)] <- NA
diag(amat) <- 1
amat

irf_tib <- function(irf, length){
  for(i in 1:length(irf$irf %>% names())) {
    irf$irf[[i]] <- irf$irf[[i]] %>% as_tibble() %>% 
      mutate(shock = names(irf$irf)[i]) %>% 
      bind_cols(t = 1:(length+1)) %>% 
      gather(key = "var", value = "irf", -shock, -t)
    
    irf$Upper[[i]] <- irf$Upper[[i]] %>% as_tibble() %>% 
      mutate(shock = names(irf$irf)[i]) %>% 
      bind_cols(t = 1:(length+1)) %>% 
      gather(key = "var", value = "upper", -shock, -t)
    
    irf$Lower[[i]] <- irf$Lower[[i]] %>% as_tibble() %>% 
      mutate(shock = names(irf$irf)[i]) %>% 
      bind_cols(t = 1:(length+1)) %>% 
      gather(key = "var", value = "lower", -shock, -t)
  }
  
  
  bind_rows(irf$irf) %>% 
    left_join(bind_rows(irf$Upper), by = c("shock", "t", "var")) %>% 
    left_join(bind_rows(irf$Lower), by = c("shock", "t", "var"))
  
}


x <- data %>% 
  group_by(country) %>%
  mutate(across(.cols = c(y, pi, e, w, gep, gpr), .fns = ~log(.x))) %>% 
  mutate(across(.cols = c(y, pi), .fns = ~.x - lag(.x))) %>% 
  drop_na() %>% 
  nest() %>% 
  mutate(data = map(data, ~dplyr::select(.x, -date) %>% 
                      dplyr::select(gpr, gep, e, pi, y, w, r) %>% 
                      ts()),
         var = map(data, ~VAR(.x, p = 1, type = "const")),
         svar = map(var, ~SVAR(.x, estmethod = "scoring", Amat = amat)),
         sum = map(var, ~summary(.x)),
         cov_norm = map(sum, ~.x$covres/diag(.x$covres)),
         shock = map(sum, ~t(chol(.x$covres))/diag(t(chol(.x$covres)))),
         resid = map(var, ~resid(.x)),
         fitted = map(var, ~fitted(.x)),
         irf = map(svar, ~irf(.x, 
                             impulse = c("gpr", "gep", "e", "pi", "y", "w", "r"),
                             response = c("gpr", "gep", "e", "pi", "y", "w", "r"),
                             n.ahead = 20,
                             boot = TRUE) %>% irf_tib(length = 20))) 



x %>% 
  dplyr::select(country, irf) %>% 
  unnest(irf) %>% 
  filter(shock == "gpr") %>% 
  ggplot(aes(x = t, y = irf, #ymin = lower, ymax = upper, 
             color = country, fill = country)) +
  geom_line(linewidth = .75) +
#  geom_ribbon(linetype = "dashed", alpha = 0) +
  facet_wrap(~var, scales = "free") +
  theme_minimal() +
  scale_color_manual(breaks = c("BG", "CZ", "EE", "HU", "LT", "PL", "RO", "SI", "SK"),
                     values = RColorBrewer::brewer.pal(9, name = "Spectral")) +
  ggtitle("Geopolitical risk shock")


x %>% 
  dplyr::select(country, irf) %>% 
  unnest(irf) %>% 
  filter(shock == "gep") %>% 
  ggplot(aes(x = t, y = irf, #ymin = lower, ymax = upper, 
             color = country, fill = country)) +
  geom_line(linewidth = .75) +
  #  geom_ribbon(linetype = "dashed", alpha = 0) +
  facet_wrap(~var, scales = "free") +
  theme_minimal() +
  scale_color_manual(breaks = c("BG", "CZ", "EE", "HU", "LT", "PL", "RO", "SI", "SK"),
                     values = RColorBrewer::brewer.pal(9, name = "Spectral")) +
  ggtitle("Energy price shock")




x %>% 
  dplyr::select(country, irf) %>% 
  unnest(irf) %>% 
  filter(shock == "y") %>% 
  ggplot(aes(x = t, y = irf, #ymin = lower, ymax = upper, 
             color = country, fill = country)) +
  geom_line(linewidth = .75) +
  #  geom_ribbon(linetype = "dashed", alpha = 0) +
  facet_wrap(~var, scales = "free") +
  theme_minimal() +
  scale_color_manual(breaks = c("BG", "CZ", "EE", "HU", "LT", "PL", "RO", "SI", "SK"),
                     values = RColorBrewer::brewer.pal(9, name = "Spectral")) +
  ggtitle("GDP shock (demand)")

x %>% 
  dplyr::select(country, irf) %>% 
  unnest(irf) %>% 
  filter(shock == "pi") %>% 
  ggplot(aes(x = t, y = irf, #ymin = lower, ymax = upper, 
             color = country, fill = country)) +
  geom_line(linewidth = .75) +
  #  geom_ribbon(linetype = "dashed", alpha = 0) +
  facet_wrap(~var, scales = "free") +
  theme_minimal() +
  scale_color_manual(breaks = c("BG", "CZ", "EE", "HU", "LT", "PL", "RO", "SI", "SK"),
                     values = RColorBrewer::brewer.pal(9, name = "Spectral")) +
  ggtitle("Inflation shock (supply)")

x %>% 
  dplyr::select(country, irf) %>% 
  unnest(irf) %>% 
  filter(shock == "e") %>% 
  ggplot(aes(x = t, y = irf, #ymin = lower, ymax = upper, 
             color = country, fill = country)) +
  geom_line(linewidth = .75) +
  #  geom_ribbon(linetype = "dashed", alpha = 0) +
  facet_wrap(~var, scales = "free") +
  theme_minimal() +
  scale_color_manual(breaks = c("BG", "CZ", "EE", "HU", "LT", "PL", "RO", "SI", "SK"),
                     values = RColorBrewer::brewer.pal(9, name = "Spectral")) +
  ggtitle("Exchange rate shock")

x %>% 
  dplyr::select(country, irf) %>% 
  unnest(irf) %>% 
  filter(shock == "r") %>% 
  ggplot(aes(x = t, y = irf, #ymin = lower, ymax = upper, 
             color = country, fill = country)) +
  geom_line(linewidth = .75) +
  #  geom_ribbon(linetype = "dashed", alpha = 0) +
  facet_wrap(~var, scales = "free") +
  theme_minimal() +
  scale_color_manual(breaks = c("BG", "CZ", "EE", "HU", "LT", "PL", "RO", "SI", "SK"),
                     values = RColorBrewer::brewer.pal(9, name = "Spectral")) +
  ggtitle("Interest rate shock")

x %>% 
  dplyr::select(country, irf) %>% 
  unnest(irf) %>% 
  filter(shock == "w") %>% 
  ggplot(aes(x = t, y = irf, #ymin = lower, ymax = upper, 
             color = country, fill = country)) +
  geom_line(linewidth = .75) +
  #  geom_ribbon(linetype = "dashed", alpha = 0) +
  facet_wrap(~var, scales = "free") +
  theme_minimal() +
  scale_color_manual(breaks = c("BG", "CZ", "EE", "HU", "LT", "PL", "RO", "SI", "SK"),
                     values = RColorBrewer::brewer.pal(9, name = "Spectral")) +
  ggtitle("Wage shock")


#Panel var attempt
library(panelvar)
panel <- data %>% mutate(date = as.numeric(date)) %>% 
  as.data.frame()

mod_ols <- pvarfeols(dependent_vars = c("y", "pi", "r", "e", "w", "gep", "gpr"),
                     lags = 1,
                     exog_vars = c("gep", "gpr"),
                     transformation = "fd",
                     data = panel,
                     panel_identifier = c("country", "date")
)


panelvar::girf(mod_ols, n.ahead = 20, ma_approx_steps = 20) %>% plot()
#panelvar::oirf(mod_ols, n.ahead = 20) %>% plot()
