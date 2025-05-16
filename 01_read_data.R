#===============================================================================
# Script to read the data for the tapping date analysis
#-------------------------------------------------------------------------------

# To-do: ----
# TR - Add data from Quebec (PPAQ survey from 1999 to 2011)
#.    - TR Contact DAniel Houle and Louis Duchesne on season open and close

# Load dependencies ----
if(!existsFunction("%≥%")) library("tidyverse")
if(!existsFunction("read_excel")) library("readxl")

# Read NASS data for the individual states ----
file_name <- "Sugaring season data.xlsx"
d <- read_excel(path = paste0("./data/", file_name),
             sheet = "Data",
             col_names = c("yr", "o_ME", "c_ME", "o_MA", "c_MA", "o_NH", "c_NH", 
                           "o_VT", "c_VT", "o_NY", "c_NY", "y_VT", "b_VTC",
                           "o_VTH", "b_VTH", "c_VTH", "t_VTH", "o_ON"),
             skip = 6,
             na = "NA") %>%
    pivot_longer(cols = -yr,     
                 names_to = c(".value", "state"),
                 names_sep = "_") %>% 
  mutate(o_date = as_date(o - 1, origin = paste0(yr, "-01-01")),
         c_date = as_date(c - 1, origin = paste0(yr, "-01-01")),
         b_date = as_date(b - 1, origin = paste0(yr, "-01-01")),
         d = c-o)

# Read the Pennsylvania (PA) data for season duration ----
file_name <- "PA Season Duration - NASS.xlsx"
d_PA <- read_excel(path = paste0("./data/", file_name),
                   sheet = "Sheet1",
                   col_names = c("yr", "d"),
                   skip = 3) %>%
  mutate(state = "PA")

# Combine PA and other NASS data ---
d <- bind_rows(d, d_PA)
rm(d_PA)

# Add state's southern most, northern most and mean latitude ----
# N.B.: Before 1999 only southern Maine was included in the NASS data and since 
# 1999 all of Maine is included.
d <- d %>% mutate(m_lat = case_when(
    state == "ME" & yr <= 1999 ~ 45.25, # Before 1999 they only included southern Maine # TR - Needs to be adjusted
    state == "ME" & yr >  1999 ~ 45.25, # TR - Needs to change
    state == "MA" ~ 42.34,
    state == "NH" ~ 43.68,
    state == "NY" ~ 42.97,
    state == "PA" ~ 40.87,
    state == "VT" ~ 43.93,
    state == "VTC" ~ 43.94,
    state == "VTH" ~ 43.94,
    state == "ON" ~ 51.25),
  n_lat = case_when(
    state == "ME" & yr <= 1999 ~ 45.00, # Before 1999 they only included southern Maine # TR - Needs to be adjusted
    state == "ME" & yr >  1999 ~ 47.46,
    state == "MA" ~ 42.88,
    state == "NH" ~ 45.30,
    state == "NY" ~ 45.02,
    state == "PA" ~ 42.27,
    state == "VT" ~ 45.02,
    state == "VTC" ~ 43.94,
    state == "VTH" ~ 43.94,
    state == "ON" ~ 56.85),
  s_lat = case_when(
    state == "ME" & yr <= 1999 ~ 45.25, # Before 1999 they only included southern Maine # TR - Needs to be adjusted
    state == "ME" & yr >  1999 ~ 42.96,
    state == "MA" ~ 41.23,
    state == "NH" ~ 42.70,
    state == "NY" ~ 40.30,
    state == "PA" ~ 39.72,
    state == "VT" ~ 42.73,
    state == "VTC" ~ 43.94,
    state == "VTH" ~ 43.94,
    state == "ON" ~ 41.69)) 

# Add a weights column to give more importance to NASS state-averages ----
d <- d %>% mutate(w = case_when(
  state == "VTC" ~ 1, 
  state == "VTH" ~ 1,
  state == "ON" ~ 9,
  state != "VTC" & state != "VTH" ~ 100, # TR - Ought to change this to number of responses per state. I assume that this is based on an n = 100 per state.
))

# Add column for sites within states and source of the data ----
d <- d %>% mutate(site = case_when(
  state == "VTC" ~ "VTC",
  state == "VTH" ~ "VTH",
  state != "VTC" & state != "VTH" ~ "NA",
  state %in% c("MA", "ME", "NH", "NY", "ON", "PA") ~ "NA"),
  source = case_when(
    state %in% c("MA", "ME", "NH", "NY", "PA") ~ "NASS",
    state == "ON" ~ "CFS",
    state == "VT" & site == "NA" ~ "NASS",
    state %in% c("VTH", "VTC") ~ "IND"))
d$state[d$site %in% c("VTC", "VTH")] <- "VT"

# Read St. John's data ----
file_name <- "Data_St_Johns_All.xlsx"
d_StJ <- read_excel(path = paste0("./data/", file_name),
                    sheet = "General Sum",
                    col_names = c("y", "logical_syrup", "n_taps", "t_sy_y", 
                                  "t_sa_y", "wood_used", "tapping_date", 
                                  "untapping", "b", "last_b", 
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
  dplyr::select(-c(WWW, XXX, YYY, ZZZ, AAA, BBB, CCC, DDD, EEE))

# Convert StJ data into the d format ----
d1 <- d_StJ %>% 
  mutate(yr = y,
         state = "MN",
         o = yday(tapping_date),
         c = yday(untapping),
         y = t_sy_y / n_taps,
         b = yday(b),
         t = "syrup",
         o_date = as_date(tapping_date),
         c_date = as_date(untapping),
         b_date = as_date(b),
         d = c - o,
         m_lat = 45.57,
         n_lat = 45.58,
         s_lat = 45.56,
         w = 1,
         site = "STJ",
         source = "IND") %>%
  dplyr::select(yr, state, o, c, y, b, t, o_date, c_date, b_date, d, m_lat,
                n_lat, s_lat, w, site, source)

# Add the Saint John's and Saint Benedict data to the overall data ----
d <- rbind(d, d1); rm(d1)

# Additional daily data ----
dates <- format(seq(from = as_date("2024-02-15"), 
                    to = as_date("2024-04-30"), 
                    by = "day"), "%m-%d")
d_StJ_daily <- read_excel(path = paste0("./data/", file_name),
                          sheet = "Daily Sap Sum",
                          range = "A6:CC37",
                          col_names = c("y", "AAA", "BBB", dates, "t_sap", 
                                        "n_days")) %>% 
  dplyr::select(-AAA, -BBB, -t_sap, -n_days) %>%
  pivot_longer(cols = starts_with("0"),
    names_to = "month_day", 
    values_to = "value") %>%
  mutate(value = if_else(is.na(value), 0, value)) %>% 
  slice(-c(91, 167, 243, 319, 471, 623, 775, 927, 1003, 1155, 1231, 1307, 1459, 
           1535, 1611, 1763, 1839, 1915, 2067, 2143, 2219, 2371)) %>% # Filter out 02-29 on non-leap years
  mutate(date = as_date(paste(y, month_day, sep = "-"))) %>%
  dplyr::select(-month_day, -y)
  
#===============================================================================
