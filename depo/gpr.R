library(httr)
library(readxl)
#gpr_dat <- GET("https://www.matteoiacoviello.com/gpr_files/data_gpr_export.xls", 
#    write_disk(tf <- tempfile(fileext = ".xls")))[["content"]] %>% 
#  read_excel(col_types = c("date", rep("numeric", 110), rep("text", 2))) %>% 
#  mutate(month = as_date(month))


gpr_dat <- read.csv2("data_gpr_export.csv") %>% 
  as_tibble()
colnames(gpr_dat)[1] <- "month"
gpr_dat <- gpr_dat %>% 
  mutate(month = str_replace_all(month, "\\.", "-"),
         month = as.Date(month))

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
                                 index == "gpr_pol" ~ "Geopolitical Risk Index - Poland"))

gpr %>% saveRDS("gpr.rds")


gpr %>%  
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~description, scales = "free") +
  theme_minimal() +
  labs(x = "",
       y = "")

png("gpr.png")
