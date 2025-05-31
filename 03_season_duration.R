#===============================================================================
# Script to plot the beginning and end of season dates
#-------------------------------------------------------------------------------

# TR - Need to include the duration since first boil either in the same model 
# or a separate model

# Load dependencies ----
if(!existsFunction("brms")) library("brms") 

# Load data ----
if(!exists("d")) source("01_read_data.R")


# Define the two response models for the duration since season open and since 
# first boil ----
formula_d_o <- bf(d_o ~ yr + (yr | region / site))
formula_d_b <- bf(d_b ~ yr + (yr | region / site))

# Fit the multivariate model with correlated residuals for the two variables of 
# season duration ----
mod_d <- brm(
  formula = formula_d_o + formula_d_b + set_rescor(TRUE), # set_rescor allows the model to estimate the residual correlation between d_o and d_b.
  data = d %>% filter(!is.na(d_o)),
  family = gaussian(),
  chains = 4, cores = 4, iter = 4000
)

# Model a linear trend for the season duration since its open ----
mod_d_o <- brm(formula = d_o ~ yr + (yr | region / site),
             data = d %>% filter(!is.na(d_o)), 
             family = gaussian(),
             chains = 4, cores = 4, iter = 6000,
             control = list(adapt_delta = 0.99, max_treedepth = 12))
summary(mod_d)
plot(mod_d)

# Extract fixed effects and random effects ----
d_fixed_effects <- fixef(mod_d)  # Fixed effects (intercept and slope)
d_random_effects <- ranef(mod_d)  # Random effects by group (region)

# Plot data of season duration for Massachusetts, Maine, New Hampshire, Vermont, 
# New York, and Pennsylvania from the NASS survey as well as individual site 
# data from Minnesota, OMPSA and PPAQ data for regions in Ontraio and Quebec, 
# respectively ----
par(mfrow = c(3, 3), mar = c(5, 5, 1, 1))
for (region in c("ME", "MA", "NH", "NY", "PA", "VT", "MN")){
  
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
           ylim = c(10, 60),
           axes = FALSE, xlab = "Year", ylab = "Duration (sdays)")
      text(x = 2020, y = 60, adj = 1, pos = 2,
           label = case_when(
             region == "MA" ~ "Massachusetts",
             region == "ME" ~ "Maine",
             region == "NH" ~ "New Hampshire",
             region == "NY" ~ "New York",
             region == "VT" ~ "Vermont",
             region == "PA" ~ "Pensylvannia",
             region == "MN" ~ "Minnesota"))
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
    d_region_intercept <- d_fixed_effects["Intercept", "Estimate"] + 
      d_random_effects$region[region, "Estimate", "Intercept"] +
      d_random_effects$`region:site`[paste0(region, "_", site), "Estimate", "Intercept"]
    d_region_slope <- d_fixed_effects["yr", "Estimate"] + 
      d_random_effects$region[region, "Estimate", "yr"] +
      d_random_effects$`region:site`[paste0(region, "_", site), "Estimate", "yr"]
      
    # Add the linear trends for beginning and end of the season ----
    abline(a = d_region_intercept, b = d_region_slope, 
           col = ifelse(site == "NA", "black", "darkgray"), 
           lwd = 2, lty = 1)
    
  } # End site loop
} # End loop over regions

# Plot change in season duration versus latitude ----
par(mar = c (5, 5, 1, 1), mfrow = c(1, 1))
plot(x = d %>% group_by(region) %>% summarise(m_lat = mean(m_lat)) %>% 
       filter(region %in% c("MA" ,"ME" ,"MN" ,"NH" ,"NY" ,"PA" ,"VT")) %>% 
       select(m_lat) %>% unlist(),
     y = d_random_effects$region[, "Estimate", "yr"],
     pch = 19, axes = FALSE,
     xlab = expression(paste("Latitude (",degree,")")),
     ylab = expression(beta[duration]))
axis(side = 1)
axis(side = 2, las = 1)

#===============================================================================
