#===============================================================================
# Script to read the data for the tapping date analysis
#-------------------------------------------------------------------------------

# TR - Make sure the census data is read correctly and integrated in the d tibble
# TR - Check coordinates with Louis and make sure that the Quebec data is read in correctly

# VTC is the Vermont Technical College in Randolph
# VTU is a local sugarmaker from Northern Vermont.
# VTF is Fleury's Hill Maple Farm in Richford Vermont.

# Load dependencies ----
if(!existsFunction("%≥%")) library("tidyverse")
if(!existsFunction("read_excel")) library("readxl")

# Read NASS data for the individual states ----
file_name <- "Sugaring season data.xlsx"
d <- read_excel(path = paste0("./data/", file_name),
             sheet = "Data",
             col_names = c("yr", "o_ME", "c_ME", "o_MA", "c_MA", "o_NH", "c_NH", 
                           "o_VT", "c_VT", "o_NY", "c_NY", "y_VT", "b_VTC",
                           "o_VTH", "b_VTH", "c_VTH", "typ_VTH", "t_VTU", 
                           "b_VTU", "c_VTU", "y_VTU","o_VTF", "c_VTF", "y_VTF", 
                           "b_CFS", "b_OMSPA", "c_OMSPA", "ssc_OMSPA", 
                           "y_OMSPA", "ntaps_OMSPA", "w_OMSPA", "b_AL", 
                           "c_AL", "ssc_AL", "y_AL", 
                           "ntaps_AL", "w_AL", "b_AQ", "c_AQ", "ssc_AQ", 
                           "y_AQ", "ntaps_AQ", "w_AQ", "b_EA", "c_EA", "ssc_EA", 
                           "y_EA", "ntaps_EA", "w_EA", "b_GB", "c_GB", "ssc_GB", 
                           "y_GB", "ntaps_GB", "w_GB", "b_HK", "c_HK", "ssc_HK", 
                           "y_HK", "ntaps_HK", "w_HK", "b_LA", "c_LA", "ssc_LA", 
                           "y_LA", "ntaps_LA", "w_LA", "b_OV", "c_OV", "ssc_OV", 
                           "y_OV", "ntaps_OV", "w_OV", "b_QI", "c_QI", "ssc_QI", 
                           "y_QI", "ntaps_QI", "w_QI", "b_SI", "c_SI", "ssc_SI", 
                           "y_SI", "ntaps_SI", "w_SI", "b_SW", "c_SW", "ssc_SW", 
                           "y_SW", "ntaps_SW", "w_SW", "b_WW", "c_WW", "ssc_WW", 
                           "y_WW", "ntaps_WW", "w_WW"),
             skip = 6,
             na = "NA") %>%
    pivot_longer(cols = -yr,     
                 names_to = c(".value", "region"),
                 names_sep = "_") %>% 
  mutate(o_date = as_date(o - 1, origin = paste0(yr, "-01-01")),
         c_date = as_date(c - 1, origin = paste0(yr, "-01-01")),
         b_date = as_date(b - 1, origin = paste0(yr, "-01-01")),
         d_o = c - o,
         d_b = c - b)

# Read the Pennsylvania (PA) data for season duration ----
file_name <- "PA Season Duration - NASS.xlsx"
d_PA <- read_excel(path = paste0("./data/", file_name),
                   sheet = "Sheet1",
                   col_names = c("yr", "d_o"),
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
    region == "VTU" ~ 44.63180408635008, # TR - Coordinates were estimated for this producer. 
    region == "VTF" ~ 44.98867889648642, # Rough coordinates for Fleury's Hill Maple Farm in Richford, Vermont
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
    region == "VTU" ~ 44.63422647268263, # TR - Coordinates were estimated for this producer. 
    region == "VTF" ~ 44.99132860860256, # Rough coordinates for Fleury's Hill Maple Farm in Richford, Vermont
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
    region == "VTU" ~ 44.62869868000306, # TR - Coordinates were estimated for this producer. 
    region == "VTF" ~ 44.986850175837134, # Rough coordinates for Fleury's Hill Maple Farm in Richford, Vermont
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

# Add column for sites within regions and source of the data ----
d <- d %>% mutate(site = case_when(
  region == "VTC" ~ "VTC",
  region == "VTH" ~ "VTH",
  region == "VTU" ~ "VTU",
  region == "VTF" ~ "VTF",
  region != "VTC" & region != "VTH" & region != "VTU" & region != "VTF" ~ "NA",
  region %in% c("MA", "ME", "NH", "NY", "ON", "PA") ~ "NA"),
  source = case_when(
    region %in% c("MA", "ME", "NH", "NY", "PA") ~ "NASS",
    region == "CFS" ~ "CFS",
    region %in% c("OMSPA", "AL", "AQ", "EA", "GB", "HK", "LA", "OV", "QI", "SI", 
                  "SW", "WW") ~ "OMSPA",
    region == "VT" & site == "NA" ~ "NASS",
    region %in% c("VTH", "VTC", "VTF") ~ "IND"))
d$region[d$site %in% c("VTC", "VTH", "VTU", "VTF")] <- "VT" # Set region for VTC, VTH, VTU, and VTF to VT
d$region[d$site %in% c("CFS", "OMSPA")] <- "ON" # Set region for CFS & OMSPA to ON

# Add a weights column to give more importance to NASS state-averages ----
d <- d %>% mutate(w = case_when(
  source == "IND" ~ 1, 
  source == "CFS" ~ 9,
  source %in% c("NASS", "PPAQ") ~ NA,
  source == "OMSPA" ~ w,
))

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
         t = yday(tapping_date),
         o = NA,
         c = yday(last_b),
         y = t_sy_y / n_taps,
         b = yday(b),
         typ = "syrup",
         ssc = brix86, # This is just calculated from rule of 86 and not measured
         ntaps = n_taps,
         t_date = as_date(tapping_date),
         o_date = as_date(o),
         c_date = as_date(untapping),
         b_date = as_date(b),
         d_o = c - t,
         d_b = c - b,
         m_lat = 45.57,
         n_lat = 45.58,
         s_lat = 45.56,
         w = 1,
         site = "STJ",
         source = "IND") %>%
  dplyr::select(yr, region, t, o, c, y, b, typ, ssc, ntaps, w, o_date, c_date, 
                b_date, d_o, d_b, m_lat, n_lat, s_lat, site, source)

# Add the Saint John's and Saint Benedict data to the overall data ----
d <- rbind(d, d1); rm(d1, d_StJ)

# Start reading the data from Quebec ----
file_name <- "DébutSaisonCouléeRegions.xlsx"
d2 <- read_excel(path = paste0("./data/Quebec/", file_name),                      
                 sheet = "DatePcomplete_Regions", skip = 1,
                 col_names = c("region", "yr", "o_date", "o", "i_o", "c_date", 
                               "c", "d_o")) %>%
  mutate(yr = yr,
         region = region,
         t = NA,
         o = o,
         c = c,
         y = NA,
         b = NA,
         typ = "syrup",
         ssc = NA,
         ntaps = 0, # TR - Need to fill this in from the separate spreadsheet
         o_date = as_date(o_date),
         c_date = as_date(c_date),
         b_date = NA,
         d_o = d_o,
         d_b = NA,
         m_lat = NA, # Filled in below
         n_lat = NA, # Filled in below
         s_lat = NA, # Filled in below
         w = 0,  # TR - To be filled in from  number of companies from separate file
         site = "STJ",
         source = "PPAQ") %>%
  relocate(yr, region, o, c, y, b, typ, t, ssc, ntaps, w, o_date, c_date, b_date, 
          d_o, d_b, m_lat, n_lat, s_lat, site, source, i_o)

# Simplify regional names for the administrative regions ----
d2 <- d2 %>% mutate(region = case_when(
  region == "Bas-Saint-Laurent–Gaspésie" ~ "BSL",
  region == "Beauce" ~ "BEA",
  region == "Centre-du-Québec" ~ "CDQ",
  region == "Cote-du-Sud" ~ "CDS",
  region == "Estrie" ~ "EST",
  region == "Lanaudière" ~ "LAN",
  region == "Mauricie" ~ "MAU",
  region == "Montérégie-Est" ~ "MOE",
  region == "Outaouais-Laurentides" ~ "LAU",
  region == "Montérégie-Ouest" ~ "MOO",
  region == "Province" ~ "PRO",
  region == "Québec" ~ "QUE"
))

# Add latitudes estimated from Fig. 1 in Houle et al. (2015) ----
d2 <- d2 %>% mutate(m_lat = case_when(
  region == "BSL" ~ 47.99,
  region == "BEA" ~ 46.08,
  region == "CDQ" ~ 45.98,
  region == "CDS" ~ 47.10,
  region == "EST" ~ 45.46,
  region == "LAN" ~ 46.21,
  region == "LAU" ~ 45.53,
  region == "MAU" ~ 46.54,
  region == "PRO" ~ 46.29,
  region == "QUE" ~ 46.94,
  region == "MOE" ~ 45.51, # TR - Assuming that Monteregie-Est is what was previously called St-Hyacinthe
  region == "MOO" ~ 45.21, # TR - Assuming that Monteregie-Ouest is what was previously called Valleyfield
), n_lat = case_when(
  region == "BSL" ~ 48.53,
  region == "BEA" ~ 46.51,
  region == "CDQ" ~ 46.56,
  region == "CDS" ~ 47.43,
  region == "EST" ~ 45.93,
  region == "LAN" ~ 46.70,
  region == "LAU" ~ 45.58,
  region == "MAU" ~ 46.85,
  region == "PRO" ~ 48.53,
  region == "QUE" ~ 47.35,
  region == "MOE" ~ 46.02,
  region == "MOO" ~ 45.41,
), s_lat = case_when(
  region == "BSL" ~ 47.45,
  region == "BEA" ~ 45.64,
  region == "CDQ" ~ 45.59,
  region == "CDS" ~ 46.77,
  region == "EST" ~ 45.00,
  region == "LAN" ~ 45.72,
  region == "LAU" ~ 45.48,
  region == "MAU" ~ 46.23,
  region == "PRO" ~ 45.00,
  region == "QUE" ~ 46.53,
  region == "MOE" ~ 45.00,
  region == "MOO" ~ 45.00,
))

# Add number of taps (ntaps) and companies (w) from a different spreadsheet ----
file_name <- "NbEntreprises_NbEntailles.xlsx"
d3 <- read_excel(path = paste0("./data/Quebec/", file_name),                      
                 sheet = "data", skip = 1, 
                 col_names = c("yr", "ntaps_PRO", "w_PRO", "ntaps_BEA", "w_BEA", 
                               "ntaps_BSL", "w_BSL", "ntaps_EST", "w_EST", 
                               "ntaps_CDQ", "w_CDQ", "ntaps_MOE", "w_MOE", 
                               "ntaps_QUE", "w_QUE", "ntaps_LAU", "w_LAU",
                               "ntaps_LAN", "w_LAN")) %>% 
  pivot_longer (cols = -yr,  # All columns except 'yr'
                names_to = c("variable", "region"),
                names_sep = "_",           # Split column names at underscore
                values_to = "value") %>% 
  pivot_wider(names_from = variable,
              values_from = value) %>% 
  mutate(ntaps = ntaps * 1000 / w) %>% # Calculate the mean number of taps per company from the total number of taps
  filter(yr >= 1999) # We do not have data for open and close before 1999
d2 <- rows_update(x = d2, y = d3, by = c("yr", "region"))

# Add i_o for the data that is not from Quebec ----
d <- d %>% mutate(i_o = NA)
d <- rbind(d, d2); rm(d2, d3)

# Read in NASS census data ----
d_cens <- read_csv(file = "./data/D25B89FB-A611-3258-B8AD-AB089064EE13.csv",
                   col_names = c("source", "yr", "Period", "week", "geo_level", 
                                 "region", "State ANSI", "Ag District", 
                                 "Ag District Code", "county", "County ANSI", 
                                 "zip_code", "Region", "watershed_code", 
                                 "Watershed", "Commodity", "data_item", 
                                 "domain", "Domain Category", "Value", 
                                 "CV"),
                   col_types = cols(), skip = 1) %>% 
  dplyr::select(-c(Period, week, `State ANSI`, `Ag District`, 
                   `Ag District Code`, `County ANSI`, Region, 
                   watershed_code, Watershed, Commodity)) %>%
  dplyr::filter(region %in% c("MAINE", "MASSACHUSETTS", "MINNESOTA", 
                              "NEW HAMPSHIRE", "NEW YORK", "PENNSYLVANIA", 
                              "VERMONT")) %>%
  mutate(Value = ifelse(Value == "(D)" | Value == "(L)" | Value == "(Z)", NA, Value),
         CV = ifelse(CV == "(D)" | CV == "(L)" | CV == "(Z)" | CV == "(H)", NA, CV)) %>% 
  mutate(value = parse_number(Value),
         cv = parse_number(CV)) %>% 
  dplyr::select(-Value, - CV) %>% 
  mutate(region = case_when(
    region == "MAINE" ~ "ME",
    region == "MASSACHUSETTS" ~ "MA",
    region == "MINNESOTA" ~ "MN",
    region == "NEW HAMPSHIRE" ~ "NH",
    region == "NEW YORK" ~ "NY",
    region == "PENNSYLVANIA" ~ "PA",
    region == "VERMONT" ~ "VT"
  )) %>% filter(geo_level == "STATE", # Currently focusing on the state data.
                domain == "TOTAL" &
                  `Domain Category` == "NOT SPECIFIED" & 
                  data_item %in% c("MAPLE SYRUP - NUMBER OF TAPS",
                                   "MAPLE SYRUP - OPERATIONS WITH PRODUCTION",
                                   "MAPLE SYRUP - OPERATIONS WITH SALES",
                                   "MAPLE SYRUP - OPERATIONS WITH TAPS",
                                   "MAPLE SYRUP - PRODUCTION, MEASURED IN GALLONS",
                                   "MAPLE SYRUP - SALES, MEASURED IN $")) %>% 
  mutate(item = case_when(
    grepl("NUMBER OF TAPS", data_item) ~ "taps",
    grepl("OPERATIONS WITH PRODUCTION", data_item) ~ "ops_prod",
    grepl("OPERATIONS WITH SALES", data_item) ~ "ops_sales",
    grepl("OPERATIONS WITH TAPS", data_item) ~ "ops_taps",
    grepl("PRODUCTION, MEASURED", data_item) ~ "prod",
    grepl("SALES, MEASURED", data_item) ~ "sales"
  )) %>%  
  dplyr::select(-c(geo_level, county, zip_code)) %>%
  pivot_wider(id_cols = c(source, yr, region), 
              names_from = item,
              values_from = c(cv, value),
              names_sep = "_") %>% 
  dplyr::select(-c(value_ops_prod, value_ops_sales, cv_ops_prod, cv_ops_sales)) %>%
  rename(taps = "value_taps", prod = "value_prod", sales = "value_sales", 
         w = "value_ops_taps", cv_w = "cv_ops_taps") %>%
  mutate(ntaps = taps / w,
         y = prod / taps)

# Additional daily data ----
# dates <- format(seq(from = as_date("2024-02-15"), 
#                     to = as_date("2024-04-30"), 
#                     by = "day"), "%m-%d")
# d_StJ_daily <- read_excel(path = paste0("./data/", file_name),
#                           sheet = "Daily Sap Sum",
#                           range = "A6:CC37",
#                           col_names = c("y", "AAA", "BBB", dates, "t_sap", 
#                                         "n_days")) %>% 
#   dplyr::select(-AAA, -BBB, -t_sap, -n_days) %>%
#   pivot_longer(cols = starts_with("0"),
#     names_to = "month_day", 
#     values_to = "value") %>%
#   mutate(value = if_else(is.na(value), 0, value)) %>% 
#   slice(-c(91, 167, 243, 319, 471, 623, 775, 927, 1003, 1155, 1231, 1307, 1459, 
#            1535, 1611, 1763, 1839, 1915, 2067, 2143, 2219, 2371)) %>% # Filter out 02-29 on non-leap years
#   mutate(date = as_date(paste(y, month_day, sep = "-"))) %>%
#   dplyr::select(-month_day, -y)
#   
#===============================================================================
