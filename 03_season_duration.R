#===============================================================================
# Script to plot the beginning and end of season dates
#-------------------------------------------------------------------------------

# Load dependencies ----
if(!existsFunction("brms")) library("brms") 

# Load data ----
if(!exists("d")) source("01_read_data.R")

# Plot d_o vs d_b for the sites for which we have them ----
par(mfrow = c(1, 1))
plot(x = d$d_b,
     y = d$d_o, xlim = c(0, 60), ylim = c(0, 60), pch = 19,
     xlab = "Duration from first boil (days)", 
     ylab = "Duration from season open (days)", axes = FALSE)
abline(a = 0, b = 1, lty = 2, col = "gray")
axis(side = 1)
axis(side = 2, las = 1)

# Define the two response models for the duration since season open and since 
# first boil, both model need to impute missing values, as we do not have this 
# data for all regions and sites ----
bf_do <- bf(d_o | mi() ~ mi(d_b) + (1 + mi(d_b) | region / site))
# bf_do <- bf(d_o | mi() ~ mi(d_b) + exp(delta), 
#             delta ~ 1 + (1 | region/site), 
#             nl = TRUE)
bf_db <- bf(d_b | mi() ~ yr + (1 + yr | region / site))

# priors <- c(
#   set_prior("normal(0, 1)", nlpar = "delta"),
#   set_prior("normal(0, 1)", class = "b", resp = "db"), # for d_b model
#   set_prior("normal(0, 1)", class = "sd")
# )

# Set the priors ----
priors <- c(
  # Truncated normal prior for the coefficient of mi(d_b) in the d_o model
  set_prior("normal(1, 0.5)", class = "b", coef = "mid_b", resp = "do", lb = 0),
  
  # Other priors
  set_prior("normal(0, 1)", class = "b"), # fixed effects for the trend over time
  set_prior("normal(0, 1)", class = "sd") # group-level effects of region and site
) 


# Fit the multivariate model with correlated residuals for the two variables of 
# season duration ----
mod_d <- brm(
  formula = bf_do + bf_db + set_rescor(TRUE), # set_rescor allows the model to estimate the residual correlation between d_o and d_b.
  data = d %>% filter(!is.na(d_o) | !is.na(d_b)) %>% 
    dplyr::select(yr, region, d_o, d_b, site),
  family = gaussian(),
  chains = 4, cores = 4, iter = 6000,
  control = list(adapt_delta = 0.99, max_treedepth = 13)
)

# Check model summary ----
summary(mod_d)
plot(mod_d)

# Extract fixed effects and random effects ----
d_fixed_effects <- fixef(mod_d)  # Fixed effects (intercept and slope)
d_random_effects <- ranef(mod_d)  # Random effects by group (region)


d_model_with_flags <- d %>%
  mutate(
    missing_d_o = is.na(d_o),
    missing_d_b = is.na(d_b)
  ) %>%
  filter(!is.na(d_o) | !is.na(d_b)) %>%
  dplyr::select(yr, region, d_o, d_b, site, missing_d_o, missing_d_b)

# Plot data of season duration for Massachusetts, Maine, New Hampshire, Vermont, 
# New York, and Pennsylvania from the NASS survey as well as individual site 
# data from Minnesota, OMPSA and PPAQ data for regions in Ontario and Quebec, 
# respectively ----
par(mfrow = c(3, 3), mar = c(5, 5, 1, 1))
for (region in c("ME", "MA", "NH", "NY", "PA", "VT", "MN", "OMSPA", "PRO")){
  
  # Determine if there are individual sites in a region (NA for region-wide averages)
  if (region == "MN") {
    sites <- "STJ"
  } else if (region != "VT") {
    sites <- "NA"
  } else {
    sites <- c("NA", "VTH")
  }
  
  # Loop over individual sites in each region ----
  for (site in sites) {
    if(region != "VT" | (region == "VT" & site == "NA")) {
      plot(x = d$yr[d$region == region & d$site == site], 
           y = d$d_o[d$region == region & d$site == site], 
           pch = ifelse(site == "NA", 19, 23), 
           lwd = 1.5, 
           col = ifelse(site == "NA", "black", "darkgray"),
           xlim = c(ifelse(region != "VT", 1960, 1870), 2025), 
           ylim = c(0, 70),
           axes = FALSE, xlab = "Year", ylab = "Duration (sdays)")
      points(x = d$yr[d$region == region & d$site == site], 
             y = d$d_b[d$region == region & d$site == site], 
             pch = 23, lwd = 1.5, 
             col = ifelse(site == "NA", "black", "darkgray"))
      text(x = 2020, y = 69, adj = 1, pos = 2,
           label = case_when(
             region == "MA" ~ "Massachusetts",
             region == "ME" ~ "Maine",
             region == "NH" ~ "New Hampshire",
             region == "NY" ~ "New York",
             region == "VT" ~ "Vermont",
             region == "PA" ~ "Pensylvannia",
             region == "MN" ~ "Minnesota",
             region == "OMSPA" ~ "Ontario",
             region == "PRO" ~ "Quebec"))
      if (region != "VT") {
        x_ats <- seq(1960, 2020, by = 10)
      } else {
        x_ats <- seq(1880, 2020, by = 20)
      }
      axis(side = 1, at = x_ats)
      axis(side = 2, las = 1)
    } else if (region == "VT" & site == "VTH") {
      points(x = d$yr[d$region == region & d$site == site], 
             y = d$d_o[d$region == region & d$site == site], 
             pch = 23, lwd = 1.5, col = "darkgray")
    }
  
    # Calculate the intercept and slope for the chosen region
    # d_region_intercept <- d_fixed_effects["do_Intercept", "Estimate"] + 
    #   d_random_effects$region[region, "Estimate", "do_Intercept"] +
    #   d_random_effects$`region:site`[paste0(region, "_", site), "Estimate", "do_Intercept"]
    # d_region_slope <- d_fixed_effects["db_yr", "Estimate"] + 
    #   d_random_effects$region[region, "Estimate", "db_yr"] +
    #   d_random_effects$`region:site`[paste0(region, "_", site), "Estimate", "db_yr"]
    #   
    # # Add the linear trends for beginning and end of the season ----
    # abline(a = d_region_intercept, b = d_region_slope, 
    #        col = ifelse(site == "NA", "black", "darkgray"), 
    #        lwd = 2, lty = 1)
    
  } # End site loop
} # End loop over regions

# Plot change in season duration versus latitude ----
par(mar = c (5, 5, 1, 1), mfrow = c(1, 1))
plot(x = d %>% group_by(region) %>% summarise(m_lat = mean(m_lat)) %>% 
       filter(region %in% c("AL", "AQ", "BEA", "BSL", "CDQ", "CDS", "EA", 
                            "EST", "GB", "HK", "LA", "LAN", "LAU", "MA", "MAU",
                            "ME", "NH", "NY", "OV", "PA", "QI", "QUE", 
                            "SI", "STH", "SW", "VAL", "VT", "WW")) %>% 
       select(m_lat) %>% unlist(),
     y = d_random_effects$region[-c(17, 20, 23), "Estimate", "db_yr"],
     pch = 19, axes = FALSE,
     xlab = expression(paste("Latitude (",degree,")")),
     ylab = expression(beta[duration]))
axis(side = 1)
axis(side = 2, las = 1)

# Plot change in season duration versus latitude ----
par(mar = c (5, 5, 1, 1), mfrow = c(1, 1))
plot(x = d %>% group_by(region) %>% summarise(m_lat = mean(m_lat)) %>% 
       filter(region %in% c("MA", "ME", "NH", "NY", "OMSPA", "PA", "PRO", 
                            "VT")) %>% 
       select(m_lat) %>% unlist(),
     y = d_random_effects$region[c(14, 16, 18:20, 22, 23, 30), "Estimate", "db_yr"],
     pch = 19, axes = FALSE,
     xlab = expression(paste("Latitude (",degree,")")),
     ylab = expression(beta[duration]))
axis(side = 1)
axis(side = 2, las = 1)

#===============================================================================
