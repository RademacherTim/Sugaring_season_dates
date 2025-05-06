#===============================================================================
# Script to plot the beginning and end of season dates
#-------------------------------------------------------------------------------

# To-do: 

# Load dependencies ----
if(!existsFunction("brms")) library("brms") 

# Load data ----
if(!exists("d")) source("01_read_data.R")

# Model a linear trend for the onset and end of season ----
mod_d <- brm(formula = d ~ yr + (yr | state / site),
             data = d %>% filter(!is.na(d)), 
             family = gaussian(),
             chains = 4, cores = 4, iter = 4000,
             control = list(adapt_delta = 0.99))
summary(mod_d)
plot(mod_d)

# Extract fixed effects and random effects ----
d_fixed_effects <- fixef(mod_d)  # Fixed effects (intercept and slope)
d_random_effects <- ranef(mod_d)  # Random effects by group (state)

# Plot NASS data for Massachusetts, Maine, New Hampshire, Vermont, New York, 
# and Pennsylvania ----
par(mfrow = c(2, 3), mar = c(5, 5, 1, 1))
for (state in c("ME", "MA", "NH", "NY", "PA", "VT", "MN")){
  
  # Determine if there are individual sites in a state (NA for state-wide averages)
  if (state == "MN") {
    sites <- "STJ"
  } else if (state != "VT") {
    sites <- "NA"
  } else {
    sites <- c("NA", "VTH")
  }
  
  # Loop over individual sites in each state ----
  for (site in sites) {
    if(state != "VT" | (state == "VT" & site == "NA")) {
      plot(x = d$yr[d$state == state & d$site == site], 
           y = d$d[d$state == state & d$site == site], 
           pch = ifelse(site == "NA", 19, 23), 
           lwd = 1.5, 
           col = ifelse(site == "NA", "black", "darkgray"),
           xlim = c(ifelse(state != "VT", 1960, 1870), 2025), 
           ylim = c(10, 60),
           axes = FALSE, xlab = "Year", ylab = "Duration (sdays)")
      text(x = 2020, y = 60, adj = 1, pos = 2,
           label = case_when(
             state == "MA" ~ "Massachusetts",
             state == "ME" ~ "Maine",
             state == "NH" ~ "New Hampshire",
             state == "NY" ~ "New York",
             state == "VT" ~ "Vermont",
             state == "PA" ~ "Pensylvannia",
             state == "MN" ~ "Minnesota"))
      if (state != "VT") {
        x_ats <- seq(1960, 2020, by = 10)
      } else {
        x_ats <- seq(1880, 2020, by = 20)
      }
      axis(side = 1, at = x_ats)
      axis(side = 2, las = 1)
    } else if (state == "VT" & site == "VTH") {
      points(x = d$yr[d$state == state & d$site == site], 
             y = d$d[d$state == state & d$site == site], 
             pch = 23, lwd = 1.5, col = "darkgray")
    }
  
  
    # Calculate the intercept and slope for the chosen state
    d_state_intercept <- d_fixed_effects["Intercept", "Estimate"] + 
      d_random_effects$state[state, "Estimate", "Intercept"] +
      d_random_effects$`state:site`[paste0(state, "_", site), "Estimate", "Intercept"]
    d_state_slope <- d_fixed_effects["yr", "Estimate"] + 
      d_random_effects$state[state, "Estimate", "yr"] +
      d_random_effects$`state:site`[paste0(state, "_", site), "Estimate", "yr"]
      
    # Add the linear trends for beginning and end of the season ----
    abline(a = d_state_intercept, b = d_state_slope, 
           col = ifelse(site == "NA", "black", "darkgray"), 
           lwd = 2, lty = 1)
    
  } # End site loop
} # End loop over states

# Plot duration of the season versus the yield ----
par(mfrow = c(1, 1))
plot (x = d$d[d$state == "VT"],
      y = d$y[d$state == "VT"],
      pch = 19, col = "darkgreen",
      axes = FALSE, xlim = c(15, 65), ylim = c(0, 0.45),
      xlab = "Season duration (days)",
      ylab = "Average yield (gal / tap)")
axis(side = 1)
axis(side = 2, las = 1)
points(x = d$d[d$state == "MN"],
       y = d$y[d$state == "MN"],
       pch = 19, col = "#cd1041")
#===============================================================================
