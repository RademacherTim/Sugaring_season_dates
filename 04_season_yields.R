#===============================================================================
# Script to plot the beginning and end of season dates
#-------------------------------------------------------------------------------

# Load dependencies ----
if(!existsFunction("brms")) library("brms") 

# Load data ----
if(!exists("d")) source("01_read_data.R")

# Model yield as a function of start and end date or duration ----
mod_y <- brm(formula = yield ~ b + e,
             data = d %>% filter(!is.na(yield)), 
             family = gaussian(),
             chains = 4, cores = 4, iter = 4000)
summary(mod_y)
plot(mod_y)

mod_yd <- brm(formula = yield ~ d,
             data = d %>% filter(!is.na(yield)), 
             family = gaussian(),
             chains = 4, cores = 4, iter = 4000)
summary(mod_yd)
plot(mod_yd)

# Extract fixed effects and random effects ----
y_fixed_effects <- fixef(mod_y)  # Fixed effects (intercept and slope)
y_random_effects <- ranef(mod_y)  # Random effects by group (state)

# Plot NASS data for Massachusetts, Maine, New Hampshire, Vermont, New York, 
# and Pennsylvania ----
par(mfrow = c(2, 3), mar = c(5, 5, 1, 1))
for (state in c("ME", "MA", "NH", "NY", "PA", "VT")){
  plot(x = d$y[d$state == state], 
       y = d$b[d$state == state], pch = 21,
       xlim = c(1960, 2025), ylim = c(30, 150),
       axes = FALSE, xlab = "Year", ylab = "Day of the year")
  text(x = 2020, y = 140, adj = 1, pos = 2,
       label = case_when(
         state == "MA" ~ "Massachusetts",
         state == "ME" ~ "Maine",
         state == "NH" ~ "New Hampshire",
         state == "NY" ~ "New York",
         state == "VT" ~ "Vermont",
         state == "PA" ~ "Pensylvannia"))
  axis(side = 1)
  axis(side = 2, las = 1)
  points(x = d$y[d$state == state], 
         y = d$e[d$state == state], 
         pch = 19)
  
  # Calculate the intercept and slope for the chosen state
  if(state != "PA") {
    b_state_intercept <- b_fixed_effects["Intercept", "Estimate"] + 
      b_random_effects$state[state, "Estimate", "Intercept"]
    b_state_slope <- b_fixed_effects["y", "Estimate"] + 
      b_random_effects$state[state, "Estimate", "y"]
    e_state_intercept <- e_fixed_effects["Intercept", "Estimate"] + 
      e_random_effects$state[state, "Estimate", "Intercept"]
    e_state_slope <- e_fixed_effects["y", "Estimate"] + 
      e_random_effects$state[state, "Estimate", "y"]
    
    # Add the linear trends for beginning and end of the season ----
    abline(a = b_state_intercept, b = b_state_slope, col = "black", 
           lwd = 2, lty = 2)
    abline(a = e_state_intercept, b = e_state_slope, col = "black", 
           lwd = 2, lty = 1)
    
  } # End trend plotling condition
} # End loop over states


# Plot relation between early onset, late end and longer seasons with higher 
# yield ----
par(mfrow = c(1, 3))
plot(x = d$b[d$state == "VT"], y = d$yield[d$state == "VT"], pch = 25, axes = FALSE, bg = "darkgrey",
     xlim = c(45, 90), ylim = c(0.1, 0.45),
     xlab = "Season start (days)", ylab = "Syrup yield (gal/tap)")
axis(side = 1)
axis(side = 2, las = 1)
plot(x = d$e[d$state == "VT"], y = d$yield[d$state == "VT"], pch = 24, axes = FALSE, bg = "darkgrey",
     xlim = c(80, 130), ylim = c(0.1, 0.45),
     xlab = "Season end (day of year)", ylab = "Syrup yield (gal/tap)")
axis(side = 1)
axis(side = 2, las = 1)
plot(x = d$d[d$state == "VT"], y = d$yield[d$state == "VT"], pch = 23, axes = FALSE, bg = "darkgrey",
     xlim = c(0, 60), ylim = c(0.1, 0.45),
     xlab = "Season duration (days)", ylab = "Syrup yield (gal/tap)")
axis(side = 1)
axis(side = 2, las = 1)

#===============================================================================
