#===============================================================================
# Script to read the data for the tapping date analysis
#-------------------------------------------------------------------------------

# To-do: ----
# TR - Add data from Quebec (PPGB survey from 1999 to 2011)
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
                           "o_VTH", "b_VTH", "c_VTH", "t_VTH", "b_CFS", "b_OMSPA", 
                           "c_OMSPA", "ssc_OMSPA", "y_OMSPA", "ntaps_OMSPA", "w_OMSPA", 
                           "b_AL", "c_AL", "ssc_AL", "y_AL", "ntaps_AL", "w_AL", 
                           "b_AQ", "c_AQ", "ssc_AQ", "y_AQ", "ntaps_AQ", "w_AQ", 
                           "b_EA", "c_EA", "ssc_EA", "y_EA", "ntaps_EA", "w_EA", 
                           "b_GB", "c_GB", "ssc_GB", "y_GB", "ntaps_GB", "w_GB", 
                           "b_HK", "c_HK", "ssc_HK", "y_HK", "ntaps_HK", "w_HK", 
                           "b_LA", "c_LA", "ssc_LA", "y_LA", "ntaps_LA", "w_LA", 
                           "b_OV", "c_OV", "ssc_OV", "y_OV", "ntaps_OV", "w_OV", 
                           "b_QI", "c_QI", "ssc_QI", "y_QI", "ntaps_QI", "w_QI", 
                           "b_SI", "c_SI", "ssc_SI", "y_SI", "ntaps_SI", "w_SI", 
                           "b_SW", "c_SW", "ssc_SW", "y_SW", "ntaps_SW", "w_SW", 
                           "b_WW", "c_WW", "ssc_WW", "y_WW", "ntaps_WW", "w_WW"),
             skip = 6,
             na = "NA") %>%
    pivot_longer(cols = -yr,     
                 names_to = c(".value", "region"),
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
  mutate(region = "PA")

# Combine PA and other NASS data ---
d <- bind_rows(d, d_PA)
rm(d_PA)

# Add region's southern most, northern most and mean latitude ----
# N.B.: Before 1999 only southern Maine was included in the NASS data and since 
# 1999 all of Maine is included.
d <- d %>% mutate(m_lat = case_when(
    region == "ME" & yr <= 1999 ~ 44.64, # Before 1999 they only included southern Maine # TR - This is just a guess
    region == "ME" & yr >  1999 ~ 45.25, # TR - Needs to change
    region == "MA" ~ 42.34,
    region == "NH" ~ 43.68,
    region == "NY" ~ 42.97,
    region == "PA" ~ 40.87,
    region == "VT" ~ 43.93,
    region == "VTC" ~ 43.94,
    region == "VTH" ~ 43.94,
    region == "CFS" ~ 46.00, # TR - They were all in southern Ontario
    region == "OMSPA" ~ 46.00, # TR - Rough guess, based on the fact that they tend to be in Southern Ontario
    region == "ON" ~ 46.00, # TR - Rough guess, based on the fact that they tend to be in Southern Ontario
    region == "AL" ~ 47.65,
    region == "AQ" ~ 45.94,
    region == "EA" ~ 45.24,
    region == "GB" ~ 44.14,
    region == "HK" ~ 44.59,
    region == "LA" ~ 44.82,
    region == "OV" ~ 45.61,
    region == "QI" ~ 44.53,
    region == "SI" ~ 44.18,
    region == "SW" ~ 42.86,
    region == "WW" ~ 43.43),
  n_lat = case_when(
    region == "ME" & yr <= 1999 ~ 45.10, # Before 1999 they only included southern Maine # TR - This is just a guess
    region == "ME" & yr >  1999 ~ 47.46,
    region == "MA" ~ 42.88,
    region == "NH" ~ 45.30,
    region == "NY" ~ 45.02,
    region == "PA" ~ 42.27,
    region == "VT" ~ 45.02,
    region == "VTC" ~ 43.94,
    region == "VTH" ~ 43.94,
    region == "ON" ~ 56.85,
    region == "CFS" ~ 51.00, # TR - They were all in southern Ontario, so I just limited it at 51°.
    region == "OMSPA" ~ 51.00, # TR - They tend to be further south and this is probably too far north. I just put 51°N
    region == "ON" ~ 56.85,
    region == "AL" ~ 49.71,
    region == "AQ" ~ 46.15,
    region == "EA" ~ 45.65,
    region == "GB" ~ 45.32,
    region == "HK" ~ 45.36,
    region == "LA" ~ 45.45,
    region == "OV" ~ 46.17,
    region == "QI" ~ 45.06,
    region == "SI" ~ 44.93,
    region == "SW" ~ 43.80,
    region == "WW" ~ 44.04),
  s_lat = case_when(
    region == "ME" & yr <= 1999 ~ 42.96, # Before 1999 they only included southern Maine 
    region == "ME" & yr >  1999 ~ 42.96,
    region == "MA" ~ 41.23,
    region == "NH" ~ 42.70,
    region == "NY" ~ 40.30,
    region == "PA" ~ 39.72,
    region == "VT" ~ 42.73,
    region == "VTC" ~ 43.94,
    region == "VTH" ~ 43.94,
    region == "ON" ~ 41.69,
    region == "CFS" ~ 41.69, # TR - They were all in southern Ontario
    region == "OMSPA" ~ 41.69, # TR - They tend to be further south and this.
    region == "AL" ~ 46.05,
    region == "AQ" ~ 45.72,
    region == "EA" ~ 44.76,
    region == "GB" ~ 43.85,
    region == "HK" ~ 43.83,
    region == "LA" ~ 44.21,
    region == "OV" ~ 45.20,
    region == "QI" ~ 43.83,
    region == "SI" ~ 43.44,
    region == "SW" ~ 41.89,
    region == "WW" ~ 42.82)) 

# Add a weights column to give more importance to NASS state-averages ----
d <- d %>% mutate(w = case_when(
  region == "VTC" ~ 1, 
  region == "VTH" ~ 1,
  region == "ON" ~ 9,
  region != "VTC" & region != "VTH" ~ 100, # TR - Ought to change this to number of responses per state. I assume that this is based on an n = 100 per state.
))

# Add column for sites within regions and source of the data ----
d <- d %>% mutate(site = case_when(
  region == "VTC" ~ "VTC",
  region == "VTH" ~ "VTH",
  region != "VTC" & region != "VTH" ~ "NA",
  region %in% c("MA", "ME", "NH", "NY", "ON", "PA") ~ "NA"),
  source = case_when(
    region %in% c("MA", "ME", "NH", "NY", "PA") ~ "NASS",
    region == "CFS" ~ "CFS",
    region %in% c("OMSPA", "AL", "AQ", "EA", "GB", "HK", "LA", "OV", "QI", "SI", 
                 "SW", "WW") ~ "OMSPA",
    region == "VT" & site == "NA" ~ "NASS",
    region %in% c("VTH", "VTC") ~ "IND"))
d$region[d$site %in% c("VTC", "VTH")] <- "VT" # Set region for VTH & VTS to VT
d$region[d$site %in% c("CFS", "OMSPA")] <- "ON" # Set region for CFS & OMSPA to ON

# Read in NASS census data ----
d_census <- read_csv(file = "./data/D25B89FB-A611-3258-B8AD-AB089064EE13.csv",
                     col_names = c("source", "yr", "Period", "week", "Geo Level", 
                                   "region", "State ANSI", "Ag District", 
                                   "Ag District Code", "county", "County ANSI", 
                                   "Zip Code", "Region", "watershed_code", 
                                   "Watershed", "Commodity", "Data Item", 
                                   "Domain", "Domain Category", "Value", 
                                   "CV"),
                     col_types = cols(), skip = 1) %>% 
  dplyr::select(-c(Period, week, `State ANSI`, `Ag District`, 
                   `Ag District Code`, `County ANSI`, Region, 
                   watershed_code, Watershed, Commodity)) %>%
  dplyr::filter(region %in% c("MAINE", "MASSACHUSETTS", "MINNESOTA", 
                             "NEW HAMPSHIRE", "NEW YORK", "PENNSYLVANIA", 
                             "VERMONT")) %>%
  mutate(Value = ifelse(Value == "(D)" | Value == "(L)" | Value == "(Z)", NA, Value)) %>% 
  mutate(value = parse_number(Value)) %>% 
  select(-Value)

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
         region = "MN",
         o = yday(tapping_date),
         c = yday(untapping),
         y = t_sy_y / n_taps,
         b = yday(b),
         t = "syrup",
         ssc = brix86, # TR - This is just calculated from rule of 86 and not measured
         ntaps = n_taps,
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
  dplyr::select(yr, region, o, c, y, b, t, ssc, ntaps, w, o_date, c_date, b_date, d, m_lat,
                n_lat, s_lat, site, source)

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
