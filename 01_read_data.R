#===============================================================================
# Script to read the data for the tapping date analysis
#-------------------------------------------------------------------------------


# Load dependencies ----
if(!existsFunction("%≥%")) library("tidyverse")

# Read NASS data for the individual states ----
file_name <- "Climate - USDA NASS - Maple Season Open and Close for New England New York - updated through 2023.xlsx"
d <- read_excel(path = paste0("./data/", file_name),
           sheet = "Data",
           col_names = c("y", "b_ME", "e_ME", "b_MA", "e_MA", "b_NH", "e_NH", 
                         "b_VT", "e_VT", "b_NY", "e_NY", "XXX", "y_1", "d_ME",
                         "d_MA", "d_NH", "d_VT", "d_NY", "YYY", "mean_d", 
                         "yield"),
           skip = 6,
           na = "NA") %>%
  select(-XXX, - YYY, -y_1) %>%
  pivot_longer(cols = -c(y, mean_d, yield),     
               names_to = c(".value", "state"),
               names_sep = "_") %>% 
  mutate(b_date = as_date(b - 1, origin = paste0(y, "-01-01")),
         e_date = as_date(e - 1, origin = paste0(y, "-01-01")))

# Read PA data ----
file_name <- "PA Season Duration - NASS.xlsx"
d_PA <- read_excel(path = paste0("./data/", file_name),
                   sheet = "Sheet1",
                   col_names = c("y", "d"),
                   skip = 3) %>%
  mutate(state = "PA")

# Combine PA and other NASS data ---
d <- bind_rows(d, d_PA)

# Read Central VT data ----
# TR - Column L is not clear to me. It seems to simply repeat column K.
# TR - Columns P & Q seem to repeat columns N & O, but with some adjustment for 
# leap years, which I do not understand.
# TR - I do not read the NASS data for Vermont because we already read it above.
file_name <- "Climate- Central Vermont - Historical sugaring data.xlsx"
d_VT <- read_excel(path = paste0("./data/", file_name),
                   sheet = "Sheet2",
                   col_names = c("y", "tap_date", "boil", "delay", "e", "type", 
                                 "XXX", "d", "YYY", "VTC", "VTC_boil", 
                                 "VTC_boil_adj"),
                   range = "A2:L115", 
                   na = "NA") %>%
  select(-XXX, -YYY) %>% 
  mutate(tap_date = as_date(tap_date),
         boil = as_date(boil),
         e = as_date(e),
         VTC_boil_adj = as_date(VTC_boil_adj)) %>%
  pivot_longer()

# Read St. John's data ----
file_name <- "Data_St_Johns_All.xlsx"
d_StJ1 <- read_excel(path = paste0("./data/", file_name),
                     sheet = "General Sum",
                     col_names = c("y", "logical_syrup", "n_taps", "t_sy_y", 
                                   "t_sa_y", "wood_used", "tapping_date", 
                                   "untapping", "first_boil", "last_boil", 
                                   "WWW", "d_boiling", "days_boiling", 
                                   "batches", "mean_batch_vol", "XXX", 
                                   "first_sap", "last_sap", "days_collecting", "YYY", "ZZZ", 
                                   "AAA", "d_collecting", "loads", 
                                   "max_daily_sap", "min_daily_sap", 
                                   "mean_daily_sap", "mean_daily_sap_per_tap",
                                   "sap_yield", "syrup_yield", "BBB", 
                                   "sap/syrup", "syrup/cord", "brix86", 
                                   "interval", "CCC", "DDD", "EEE", "y_1", 
                                   "t_season", "t_offseason", "p_offseason", 
                                   "brix+1", "yield+1"),
                     range = "A5:AR87", 
                     na = "NA") %>%
  select(-WWW, -XXX, -YYY, -ZZZ, -AAA, -BBB, -CCC, -DDD, -EEE)
dates <- format(seq(from = as_date("2024-02-15"), 
                    to = as_date("2024-04-30"), 
                    by = "day"), "%m-%d")
d_StJ_daily <- read_excel(path = paste0("./data/", file_name),
                          sheet = "Daily Sap Sum",
                          range = "A6:CC37",
                          col_names = c("y", "AAA", "BBB", dates, "t_sap", 
                                        "n_days")) %>% 
  select(-AAA, -BBB, -t_sap, -n_days) %>%
  pivot_longer(cols = starts_with("0"),
    names_to = "month_day", 
    values_to = "value") %>%
  mutate(value = if_else(is.na(value), 0, value)) %>% 
  slice(-c(91, 167, 243, 319, 471, 623, 775, 927, 1003, 1155, 1231, 1307, 1459, 
           1535, 1611, 1763, 1839, 1915, 2067, 2143, 2219, 2371)) %>% # Filter out 02-29 on non-leap years
  mutate(date = as_date(paste(y, month_day, sep = "-"))) %>%
  select(-month_day, -y)
  
#===============================================================================
