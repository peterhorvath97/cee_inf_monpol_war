#devtools::install_github("christophergandrud/imfr")
library(imfr)
library(tidyverse)
library(stringr)
library(stringr)
library(lubridate)
library(shiny)
library(shinydashboard)

df %>% filter(str_detect(description, "rate")) %>% 
  mutate(description = as.factor(description) %>% droplevels()) %>% 
  janitor::tabyl(description, iso2c)

ccodes <- c("HU", "BG", "CZ", "EE",
            "LT", "LV", "PL", "RO",
            "SI", "SK", "RS", "HR")

meta <- tibble(indicator = c("FPOLM_PA", 
                       "FITB_PA", 
                       "FIDR_PA", 
                       "FILR_PA", 
                       "FIGB_PA", 
                       "PCPI_IX", 
                       "PPPI_IX", 
                       "FIGB_PA", 
                       "AIP_IX", 
                       "NGDP_R_SA_XDC",
                       "ENEER_IX",
                       "EREER_IX",
                       "LUR_PT"),
       description = c("Policy rate",
                       "Treasury rate", 
                       "Deposit rate",
                       "Lending rate",
                       "Govt. bonds rate",
                       "CPI",
                       "PPI",
                       "Financial Market Price Indicator",
                       "Industrial production",
                       "GDP",
                       "Nominal Effective Exchange Rate",
                       "Real Effective Exchange Rate",
                       "Unemployment Rate"),
       freq = c("M",
                "M",
                "M",
                "M",
                "M",
                "M",
                "M",
                "M",
                "M",
                "Q",
                "M",
                "M",
                "M"))

vars <- NULL
for(i in 1:nrow(meta)){
  #ys.sleep(runif(1, 5, 6))
  vars[[i]] <- imf_data(database_id = "IFS", 
                        indicator = pull(meta[i, ], indicator),
                        freq = pull(meta[i, ], freq),
                        start = 1900) %>% 
    filter(iso2c %in% ccodes) %>% 
    gather(pull(meta[i, ], indicator), key = "indicator", value = "value") %>% 
    left_join(meta, by = "indicator")
}

library(fredr)
fredr_set_key("cda47ae66b38ed7988c0a9c2ec80c94f")

df <- vars %>% 
  bind_rows %>% as_tibble() %>% 
  mutate(year_month = as.character(year_month),
         year_quarter = as.character(year_quarter),
         year_month = case_when(str_detect(year_quarter, "Q1") ~ 
                                  paste(substr(year_quarter, 1, 4), "-01", sep = ""),
                                str_detect(year_quarter, "Q2") ~ 
                                  paste(substr(year_quarter, 1, 4), "-04", sep = ""),
                                str_detect(year_quarter, "Q3") ~ 
                                  paste(substr(year_quarter, 1, 4), "-07", sep = ""),
                                str_detect(year_quarter, "Q4") ~ 
                                  paste(substr(year_quarter, 1, 4), "-10", sep = ""),
                                is.na(year_quarter) ~ year_month)) %>% 
  select(-year_quarter) %>% 
  mutate(year_month = paste(year_month, "-01", sep = ""),
         year_month = as_date(year_month)) %>% 
  mutate(source = "IMF-IFS") %>% 
  bind_rows(
  fredr("RBSIBIS", 
        frequency = "m") %>% 
  select(year_month = date, value) %>% 
  mutate(iso2c = "SI",
         indicator = "EREER_IX",
         description = "Real Effective Exchange Rate",
         freq = "M",
         source = "FRED")
  ) %>% 
  bind_rows(
    fredr("RBLTBIS", 
          frequency = "m") %>% 
      select(year_month = date, value) %>% 
      mutate(iso2c = "LT",
             indicator = "EREER_IX",
             description = "Real Effective Exchange Rate",
             freq = "M",
             source = "FRED")
  ) %>% 
  bind_rows(
    fredr("RBEEBIS", 
          frequency = "m") %>% 
      select(year_month = date, value) %>% 
      mutate(iso2c = "EE",
             indicator = "EREER_IX",
             description = "Real Effective Exchange Rate",
             freq = "M", 
             source = "FRED")
  ) %>% 
  bind_rows(
    fredr("RBHRBIS", 
          frequency = "m") %>% 
      select(year_month = date, value) %>% 
      mutate(iso2c = "HR",
             indicator = "EREER_IX",
             description = "Real Effective Exchange Rate",
             freq = "M", 
             source = "FRED")
  ) %>%   
  bind_rows(
    fredr("CLVMNACSCAB1GQLT", 
          frequency = "q") %>% 
      select(year_month = date, value) %>% 
      mutate(iso2c = "LT",
             indicator = "NGDP_R_SA_XDC",
             description = "GDP",
             freq = "Q", 
             source = "FRED")
  ) %>% 
  bind_rows(
    fredr("RBHUBIS", 
          frequency = "m") %>% 
      select(year_month = date, value) %>% 
      mutate(iso2c = "HU",
             indicator = "EREER_IX",
             description = "Real Effective Exchange Rate",
             freq = "M", 
             source = "FRED")
    ) %>%
  bind_rows(
    fredr("RBPLBIS", 
          frequency = "m") %>% 
      select(year_month = date, value) %>% 
      mutate(iso2c = "PL",
             indicator = "EREER_IX",
             description = "Real Effective Exchange Rate",
             freq = "M", 
             source = "FRED")
  ) %>% 
  bind_rows(
    fredr("CLVMNACSCAB1GQHU", 
          frequency = "q") %>% 
      select(year_month = date, value) %>% 
      mutate(iso2c = "HU",
             indicator = "NGDP_R_SA_XDC",
             description = "GDP",
             freq = "Q", 
             source = "FRED")
  ) %>% 
  bind_rows(
    fredr("NGDPRSAXDCPLQ", 
          frequency = "q") %>% 
      select(year_month = date, value) %>% 
      mutate(iso2c = "PL",
             indicator = "NGDP_R_SA_XDC",
             description = "GDP",
             freq = "Q", 
             source = "FRED")
  ) %>% 
  mutate(iso2c = as.factor(iso2c),
         indicator = as.factor(indicator),
         description = as.factor(description))



library(eurostat)


#D11-et egyben és onnan - div by employment
df <- bind_rows(df,
inner_join(
eurostat::get_eurostat("NAMQ_10_A10" %>% tolower(), select_time = "Q") %>% 
  filter(geo %in%  ccodes,
         s_adj == "SCA",
         na_item == "D11",
         nace_r2 == "TOTAL",
         unit == "CP_MEUR") %>% 
  select(-s_adj, - na_item, - nace_r2, -unit) %>% 
  mutate(values = values * 1000000),
eurostat::get_eurostat("NAMQ_10_A10_E" %>% tolower(), select_time = "Q") %>% 
  filter(geo %in% ccodes,
         s_adj == "SCA",
         nace_r2 == "TOTAL",
         na_item == "SAL_DC",
         unit == "THS_PER"
         ) %>% 
  select(-s_adj, -nace_r2, -na_item, -unit) %>% 
  mutate(values = values * 1000),
by = c("geo", "time")) %>% 
  mutate(wage = values.x / values.y) %>% 
  select(-starts_with("value")) %>% 
  rename(year_month = time,
         iso2c = geo,
         value = wage) %>% 
  mutate(indicator = "D11_L",
         description = "Average wage, Euros",
         freq = "Q",
         source = "EUROSTAT")
)
#Labour cost index & wages
df <- bind_rows(df,
  eurostat::get_eurostat("LC_LCI_R2_Q" %>% tolower(), select_time = "Q") %>% 
  filter(geo %in% ccodes,
         unit == "I16",
         s_adj == "SCA",
         nace_r2 == "B-S") %>%
  select(-unit, -s_adj, -nace_r2) %>% 
  rename(year_month = time,
         iso2c = geo,
         value = values,
         indicator = lcstruct) %>% 
  mutate(description = case_when(indicator == "D11" ~ "Wages and salaries, Index, B-S",
                                 indicator == "D1_D4_MD5" ~ "Total labour cost, Index, B-s",
                                 indicator == "D1_D4_MD5_XB" ~ "Labour cost excluding bonuses, Index, B-S",
                                 indicator == "D12_D4_MD5" ~ "Labour cost other than wages and salaries, Index, B-S")) %>% 
  mutate(freq = "Q",
         source = "EUROSTAT")
  )



remotes::install_github("https://github.com/expersso/OECD")
library(OECD)

OECD::search_dataset("Main Economics Indicator")

oecd <- OECD::get_data_structure("MEI_FIN")

df <- bind_rows(df,
OECD::get_dataset("MEI_FIN", filter = list("IR3TIB", c("HUN", "CZE", "EST", 
                                            "LVA", "LTU", "POL", 
                                            "SVK", "SVN", "HRV"), "M"),
                  start_time = "1950-01", end_time = "2023-01") %>% 
  rename(freq = FREQUENCY,
         iso2c = LOCATION,
         value = ObsValue,
         indicator = SUBJECT,
         year_month = Time) %>% 
  select(year_month, iso2c, value, indicator, freq) %>% 
  mutate(description = "Short term interest rate",
         iso2c = case_when(iso2c == "HUN" ~ "HU",
                           iso2c == "CZE" ~ "CZ",
                           iso2c == "EST" ~ "EE",
                           iso2c == "LVA" ~ "LV",
                           iso2c == "LTU" ~ "LV",
                           iso2c == "SVK" ~ "SK",
                           iso2c == "SVN" ~ "SI",
                           iso2c == "POL" ~ "PL",
                           iso2c == "HRV" ~ "HR"),
         year_month = paste(year_month, "-01", sep = "") %>% as.Date(),
         value = as.numeric(value),
         source = "OECD")
)


mon <- df %>% filter(str_detect(description, "rate")) %>% 
  rename(date = year_month) %>% 
  mutate(iso2c = as.character(iso2c),
         indicator = as.character(indicator),
         description = as.factor(description),
         topic = "Interst rates")

prices <- df %>% filter(description %in% c("CPI", "PPI", "Financial Market Price Indicator"))%>% 
  rename(date = year_month) %>% 
  mutate(iso2c = as.character(iso2c),
         indicator = as.character(indicator),
         description = as.factor(description),
         topic = "Prices")

econ <- df %>% filter(description == "GDP" | 
                        description == "Industrial production") %>% 
  rename(date = year_month) %>% 
  mutate(iso2c = as.character(iso2c),
         indicator = as.character(indicator),
         description = as.factor(description),
         topic = "Economic activity")

exchange <- df %>% filter(str_detect(description, "Exchange Rate")) %>% 
  rename(date = year_month) %>% 
  mutate(iso2c = as.character(iso2c),
         indicator = as.character(indicator),
         description = as.factor(description),
         topic = "Exchange rate")

labor <- df %>% filter(str_detect(description %>% tolower(), "wage") | 
                         str_detect(description, "Unemp") |
                         str_detect(description %>% tolower(), "labour")) %>% 
  rename(date = year_month) %>% 
  mutate(iso2c = as.character(iso2c),
         indicator = as.character(indicator),
         description = as.factor(description),
         topic = "Labour market")
  

imfr::imf_ids() %>% as_tibble() %>% 
  filter(str_detect(description , "Price"))

imfr::imf_codelist("PCPS")
imfr::imf_codes("CL_INDICATOR_PCPS") %>% filter(str_detect(description, "Energy"))
imfr::imf_codes("CL_AREA_PCPS")
imfr::imf_codes("CL_UNIT_PCPS")

energy <- imf_data(database_id = "PCPS", indicator = "PNRG", country = "W00", freq = "M", start = 1900) %>% as_tibble() %>% 
  mutate(year_month = as.character(year_month),
         year_month = paste(year_month, "-01", sep = ""),
         year_month = as_date(year_month)) %>% 
  gather(PNRG, key = "indicator", value = "value") %>% 
  mutate(description = "Energy Price Index") %>% 
  mutate(iso2c = "Global",
         iso2c = as.factor(iso2c),
         indicator = as.factor(indicator),
         description = as.factor(description),
         topic = "Energy prices",
         source = "IMF-PCPS") %>% 
  rename(date = year_month)

energy <- energy[1:which(energy$date == max(energy$date))[1], ]


plot_data <- function(data){
data %>% 
  ggplot(aes(x = date, y = value, color = iso2c)) +
  geom_line(size = .75) +
  facet_wrap(~description, scales = "free") +
  theme_minimal() +
  labs(x = "",
       y = "") +
  theme(legend.title = element_blank()) +
  scale_color_viridis_d(begin = 0, end = .8, option = "turbo")
}

econ %>% filter(description == "GDP") %>% 
  mutate(year = year(date)) %>% 
  group_by(iso2c, year) %>% 
  mutate(mean = mean(value)) %>% 
  ungroup() %>%
  group_by(iso2c) %>% 
  mutate(mean_2010 = ifelse(year == 2010, mean, NA),
         mean_2010 = mean(mean_2010, na.rm = TRUE)) %>% 
  mutate(value = 100*value / mean_2010) %>% 
  select(-year, -mean, -mean_2010) %>% 
  bind_rows(econ %>% filter(description != "GDP")) %>% 
  plot_data() +
  labs(caption = "GDP volume index, 2010 average = 100%")

exchange %>% 
  filter(str_detect(description, "Nominal")) %>% 
  filter(year(date) > 2000) %>% 
  bind_rows(exchange %>% filter(!str_detect(description, "Nominal"))) %>% 
  plot_data() +
  caption("Nominal Effective Exchange Rates post 2000")


  

plot_data(mon)
plot_data(prices)
plot_data(energy)
plot_data(labor)

library(httr)
library(readxl)
gpr_dat <- GET("https://www.matteoiacoviello.com/gpr_files/data_gpr_export.xls", 
    write_disk(tf <- tempfile(fileext = ".xls")))[["content"]] %>% 
  read_excel(col_types = c("date", rep("numeric", 112), rep("text", 2))) %>% 
  mutate(month = as_date(month))



gpr_meta <- gpr_dat %>% 
  select(var_name, var_label) %>% 
  drop_na()
gpr_dat <- gpr_dat %>% 
  select(-var_name, -var_label) %>% 
  filter(!is.na(GPR))


gpr <- gpr_dat %>% 
  select(date = month, gpr = GPR, 
         gpr_hu = GPRC_HUN,
         gpr_pol = GPRC_POL) %>% 
  gather(-date, key = "index", value = "value") %>% 
  mutate(description = case_when(index == "gpr" ~ "Geopolitical Risk Index",
                                 index == "gpr_hu" ~ "Geopolitical Risk Index - Hungary",
                                 index == "gpr_pol" ~ "Geopolitical Risk Index - Poland"),
         iso2c = case_when(index == "gpr" ~ "Global",
                                 index == "gpr_hu" ~ "HU",
                                 index == "gpr_pol" ~ "PL"),
         topic = "Geopolitical risk",
         source = "Geopolitical risk") %>% 
  rename(indicator = index)



gpr %>%  
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~description, scales = "free") +
  theme_minimal() +
  labs(x = "",
       y = "")

bind_rows(mon, econ, prices, exchange, labor,
          gpr, energy) %>% saveRDS("df_all.rds")

df <- df %>% mutate(across(c(description, indicator, iso2c), ~as.factor(.x)))

saveRDS(df, "df.rds")
saveRDS(gpr, "gpr.rds")
saveRDS(energy, "energy.rds")
