#===============================================================================
# Script to analyze and plot the beginning and end of season dates
#-------------------------------------------------------------------------------

# To-do: ---- 
# - TR Check what is going on with the season close dates. VT_VTH has crazy values in the mod_c
# - TR Add Saint Benedict data and integrate it into the model

# Load dependencies ----
if(!existsFunction("brms")) library("brms") 

# Load data ----
if(!exists("d")) source("01_read_data.R")

# Model a linear trend for the onset and end of season ----
mod_o <- brm(formula = o ~ yr + (yr | state / site),
             data = d %>% filter(!is.na(o)), 
             family = gaussian(),
             #weights = d %>% filter(!is.na(o)) %>% select(w) %>% ungroup(), # TR - Need to look more into how to treat state-averages versus site values.
             chains = 4, cores = 4, iter = 4000,
             control = list(adapt_delta = 0.95))
summary(mod_o)
plot(mod_o)
mod_c <- brm(formula = c ~ yr + (yr | state / site),
             data = d %>% filter(!is.na(c)), 
             family = gaussian(),
             chains = 4, cores = 4, iter = 4000,
             control = list(adapt_delta = 0.95))
summary(mod_c)
plot(mod_c)

# Extract fixed effects and random effects ----
o_fixed_effects <- fixef(mod_o)  # Fixed effects (intercept and slope)
o_random_effects <- ranef(mod_o)  # Random effects by group (state)
c_fixed_effects <- fixef(mod_c)  # Fixed effects (intercept and slope)
c_random_effects <- ranef(mod_c)  # Random effects by group (state)

# Plot NASS data for Massachusetts, Maine, New Hampshire, Vermont, New York, 
# and Pennsylvania ----
par(mfrow = c(2, 3), mar = c(5, 5, 1, 1))
for (state in c("ME", "MA", "NH", "NY", "VT")){
  
  # Determine if there are individual sites in a state (NA for state-wide averages)
  if (state != "VT") {
    sites <- "NA"
  } else {
    sites <- c("NA", "VTH")
  }
  
  # Loop over individual sites in each state ----
  for (site in sites) {
    if(state != "VT" | (state == "VT" & site == "NA")) {
      plot(x = d$yr[d$state == state & d$site == site], 
           y = d$o[d$state == state & d$site == site], 
           pch = 21, lwd = 1.5,
           xlim = c(ifelse(state != "VT", 1960, 1870), 2023), 
           ylim = c(30, 150),
           axes = FALSE, 
           xlab = "Year", ylab = "Day of the year")
      text(x = 2025, y = 140, adj = 1, pos = 2,
           label = case_when(
             state == "MA" ~ "Massachusetts",
             state == "ME" ~ "Maine",
             state == "NH" ~ "New Hampshire",
             state == "NY" ~ "New York",
             state == "VT" ~ "Vermont",
             state == "PA" ~ "Pensylvannia"))
      if (state != "VT") {
        x_ats <- seq(1960, 2020, by = 10)
      } else {
        x_ats <- seq(1880, 2020, by = 20)
      }
      axis(side = 1, at = x_ats)
      axis(side = 2, las = 1)
      points(x = d$yr[d$state == state & d$site == site], 
             y = d$c[d$state == state & d$site == site], 
             pch = 19, lwd = 1.5)
    } else if (state == "VT" & site == "VTH") {
      points(x = d$yr[d$state == state & d$site == site], 
             y = d$o[d$state == state & d$site == site], 
             pch = 24, lwd = 1.5, col = "darkgray")
      points(x = d$yr[d$state == state & d$site == site], 
             y = d$c[d$state == state & d$site == site], 
             pch = 25, lwd = 1.5, col = "darkgray", bg = "darkgray")
    }
    
    # Calculate the intercept and slope for the chosen state
    o_state_intercept <- o_fixed_effects["Intercept", "Estimate"] + 
      o_random_effects$state[state, "Estimate", "Intercept"] +
      o_random_effects$`state:site`[paste0(state, "_", site), "Estimate", "Intercept"]
    o_state_slope <- o_fixed_effects["yr", "Estimate"] + 
      o_random_effects$state[state, "Estimate", "yr"] +
      o_random_effects$`state:site`[paste0(state, "_", site), "Estimate", "yr"]
    c_state_intercept <- c_fixed_effects["Intercept", "Estimate"] + 
      c_random_effects$state[state, "Estimate", "Intercept"] +
      c_random_effects$`state:site`[paste0(state, "_", site), "Estimate", "Intercept"]
    c_state_slope <- c_fixed_effects["yr", "Estimate"] + 
      c_random_effects$state[state, "Estimate", "yr"] +
      c_random_effects$`state:site`[paste0(state, "_", site), "Estimate", "yr"]

    # Add the linear trends for beginning and end of the season ----
    abline(a = o_state_intercept, b = o_state_slope, 
           col = ifelse(site == "NA", "black", "darkgray"),
           lwd = 2, lty = 2)
    abline(a = c_state_intercept, b = c_state_slope, 
           col = ifelse(site == "NA", "black", "darkgray"),
           lwd = 2, lty = 1)
    
  } # End site loop  
} # End loop over states

#===============================================================================