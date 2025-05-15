#===============================================================================
# Script to analyze and plot the beginning and end of season dates
#-------------------------------------------------------------------------------

# To-do: ------------ 
# - TR Something is wrong with the linear trend line for the VTH historic first boil data. The intercept is way to high.
#      The problem is with the slope. The intercept is pretty much the same as VTC, which looks good, but the slope is distinguishabel smaller and causes and issue
# - TR Add change in size of operation to analysis (requires appropriate data)
#         - TR Census of Ag should have this data
# - TR Add credible interval for the data
# - TR Add uncertainty for the statewide data, if I can get hold of standard deviation
# - TR Do a break-point analysis and fit linear trends to to the decline and increase
# - TR Add season open data from nine operations in Ontario (S Canadian Forest Service; Robert Sajan (Natural Resources Canada) and Brian Craig (Environment Canada))
#      N.B.: It might be first boil dates rather than season open

# Load dependencies ----
if(!existsFunction("brms")) library("brms") 

# Load data ----
if(!exists("d")) source("01_read_data.R")

# Set the priors ----
priors <- c(set_prior("normal(0, 1)", class = "b"),  # fixed effects for the trend over time
            set_prior("normal(0, 1)", class = "sd")) # group-level effects of state and site

# Model a linear trend for the onset (open), first boil, and end  (close) of 
# the season ----
mod_o <- brm(formula = o ~ yr + (yr | state / site),
             data = d %>% filter(!is.na(o)), 
             family = gaussian(),
             prior = priors,
             #weights = d %>% filter(!is.na(o)) %>% select(w) %>% ungroup(), # TR - Need to look more into how to treat state-averages versus site values.
             chains = 4, cores = 4, iter = 4000,
             control = list(adapt_delta = 0.95))
summary(mod_o)
plot(mod_o)
mod_c <- brm(formula = c ~ yr + (yr | state / site),
             data = d %>% filter(!is.na(c)), 
             family = gaussian(),
             prior = priors,
             chains = 4, cores = 4, iter = 4000,
             control = list(adapt_delta = 0.95))
summary(mod_c)
plot(mod_c)
mod_b <- brm(formula = b ~ yr + (yr | state / site), 
             data = d %>% filter(!is.na(b)), 
             family = gaussian(),
             prior = priors,
             chains = 4, cores = 4, iter = 6000,
             control = list(adapt_delta = 0.99, max_treedepth = 12))
summary(mod_b)
plot(mod_b)

# Extract fixed effects and random effects ----
o_fixed_effects <- fixef(mod_o)  # Fixed effects (intercept and slope)
o_random_effects <- ranef(mod_o)  # Random effects by group (state)
c_fixed_effects <- fixef(mod_c)  # Fixed effects (intercept and slope)
c_random_effects <- ranef(mod_c)  # Random effects by group (state)
b_fixed_effects <- fixef(mod_b)  # Fixed effects (intercept and slope)
b_random_effects <- ranef(mod_b)  # Random effects by group (state)

# Plot NASS data for Massachusetts, Maine, New Hampshire, Vermont, New York, 
# and Pennsylvania ----

# Define the layout matrix
layout_matrix <- matrix(c(1, 2, 3, 4, 5, 5, 6, 7, 8), nrow = 3, byrow = TRUE)

# Set the layout ----
layout(layout_matrix, widths = c(1, 1, 1, 1, 2, 1, 1, 1), heights = c(1, 1, 1))

# Set plot margins ----
par(mar = c(5, 5, 1, 1))

# Loop over states, as there is one plot per state ----
for (state in c("ME", "MA", "NH", "NY", "VT", "MN", "ON")){
  
  # Determine if there are individual sites in a state (NA for state-wide averages)
  if (state != "VT" & state != "MN") {
    sites <- "NA"
  } else if (state == "VT") {
    sites <- c("NA", "VTH", "VTC")
  } else if (state == "MN") {
    sites <- "STJ"
  }
  
  # Loop over individual sites in each state ----
  for (site in sites) {
    if(state != "VT" | (state == "VT" & site == "NA")) {
      plot(x = d$yr[d$state == state & d$site == site], 
           y = d$o[d$state == state & d$site == site], 
           pch = 21, lwd = 1.5, col = ifelse(site == "NA", "black", "darkgray"),
           xlim = c(ifelse(state != "VT", ifelse(state != "MN", 1960, 1940), 1870), 2023), 
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
             state == "PA" ~ "Pensylvannia",
             state == "MN" ~ "Minnesota",
             state == "ON" ~ "Ontario"))
      if (state == "MN") {
        x_ats <- seq(1940, 2020, by = 10)
      } else if (state != "VT") {
        x_ats <- seq(1960, 2020, by = 10)
      } else {
        x_ats <- seq(1880, 2020, by = 20)
      }
      axis(side = 1, at = x_ats)
      axis(side = 2, las = 1)
      points(x = d$yr[d$state == state & d$site == site], 
             y = d$c[d$state == state & d$site == site], 
             pch = 19, lwd = 1.5, col = ifelse(site == "NA", "black", "darkgray"))
      if (site == "STJ") {
        points(x = d$yr[d$state == state & d$site == site], 
               y = d$b[d$state == state & d$site == site], 
               pch = 4, lwd = 1.5, col = "darkgray")
      }
    } else if (state == "VT" & site == "VTH") {
      points(x = d$yr[d$state == state & d$site == site], 
             y = d$o[d$state == state & d$site == site], 
             pch = 24, lwd = 1.5, col = "darkgray")
      points(x = d$yr[d$state == state & d$site == site], 
             y = d$c[d$state == state & d$site == site], 
             pch = 25, lwd = 1.5, col = "darkgray", bg = "darkgray")
      points(x = d$yr[d$state == state & d$site == site], 
             y = d$b[d$state == state & d$site == site], 
             pch = 4, lwd = 1.5, col = "#FFD416")
    } else if (state == "VT" & site == "VTC") {
      points(x = d$yr[d$state == state & d$site == site], 
             y = d$b[d$state == state & d$site == site], 
             pch = 4, lwd = 1.5, col = "#154734")
    }
    
    # Plot open and close for all sites except for VTC ----
    if (site != "VTC") {
      
      # Calculate the intercept and slope for the chosen state
      o_intercept <- o_fixed_effects["Intercept", "Estimate"] + 
        o_random_effects$state[state, "Estimate", "Intercept"] +
        o_random_effects$`state:site`[paste0(state, "_", site), "Estimate", "Intercept"]
      o_slope <- o_fixed_effects["yr", "Estimate"] + 
        o_random_effects$state[state, "Estimate", "yr"] +
        o_random_effects$`state:site`[paste0(state, "_", site), "Estimate", "yr"]
      if (state != "ON") {
        c_intercept <- c_fixed_effects["Intercept", "Estimate"] + 
          c_random_effects$state[state, "Estimate", "Intercept"] +
          c_random_effects$`state:site`[paste0(state, "_", site), "Estimate", "Intercept"]
        c_slope <- c_fixed_effects["yr", "Estimate"] + 
          c_random_effects$state[state, "Estimate", "yr"] +
          c_random_effects$`state:site`[paste0(state, "_", site), "Estimate", "yr"]
      }
      # Add credibility interval for the trend lines ----
      #polygon()
      
      # Add the linear trends for beginning and end of the season ----
      abline(a = o_intercept, b = o_slope, 
             col = ifelse(site == "NA", "black", "darkgray"),
             lwd = 2, lty = 2)
      if (state != "ON") {
        abline(a = c_intercept, b = c_slope, 
               col = ifelse(site == "NA", "black", "darkgray"),
               lwd = 2, lty = 1)
      }
    } 
    
    # Plot first boil dates for VTH, VTC, and STJ ----
    if (site %in% c("VTH", "VTC", "STJ")) {
      b_intercept <- b_fixed_effects["Intercept", "Estimate"] + 
        b_random_effects$state[state, "Estimate", "Intercept"] + 
        b_random_effects$`state:site`[paste0(state,"_",site), "Estimate", "Intercept"]
      b_slope <- b_fixed_effects["yr", "Estimate"] + 
        b_random_effects$state[state, "Estimate", "yr"] + 
        b_random_effects$`state:site`[paste0(state,"_",site), "Estimate", "yr"]
      
      # Add the linear trends for first boil of the season ----
      abline(a = b_intercept, b = b_slope, 
             col = ifelse(site == "VTC", "#154734", ifelse(site == "VTH", "#FFD416", "darkgray")),
             lwd = 2, lty = 3)
    } 
    
  } # End site loop  
} # End loop over states


# Plot change in season close versus latitude ----
# TR - Add change for individual sites into the graphic
par(mar = c (5, 6, 1, 1), mfrow = c(2, 1))
plot(x = d %>% group_by(state) %>% summarise(m_lat = mean(m_lat)) %>% 
       filter (state %in% c("MA", "ME", "MN", "NH", "NY", "VT")) %>% 
       select(m_lat) %>% unlist(),
     y = c_random_effects$state[, "Estimate", "yr"],
     pch = 19, axes = FALSE, xlim = c(42, 52), ylim = c (-0.0015, 0.0025),
     xlab = expression(paste("Latitude (",degree,")")),
     ylab = "")
mtext(side = 2, line = 4, text = "Change in season close")
axis(side = 1, at = seq(42, 52, by = 2))
axis(side = 2, las = 1)
abline(lm(c_random_effects$state[, "Estimate", "yr"] ~ 
            d %>% group_by(state) %>% summarise(m_lat = mean(m_lat)) %>% 
            filter (state %in% c("MA", "ME", "MN", "NH", "NY", "VT")) %>% 
            select(m_lat) %>% unlist()))
# Plot change in season open versus latitude ----
plot(x = d %>% group_by(state) %>% summarise(m_lat = mean(m_lat)) %>% 
       filter (state != "PA") %>% select(m_lat) %>% unlist(),
     y = o_random_effects$state[, "Estimate", "yr"],
     pch = 19, axes = FALSE, xlim = c(42, 52), ylim = c (-0.0015, 0.0025),
     xlab = expression(paste("Latitude (",degree,")")),
     ylab = "")
mtext(side = 2, line = 4, text = "Change in season open")
axis(side = 1, at = seq(42, 52, by = 2))
axis(side = 2, las = 1)
abline(lm(o_random_effects$state[, "Estimate", "yr"] ~ 
            d %>% group_by(state) %>% 
            summarise(m_lat = mean(m_lat)) %>% 
            filter (state != "PA") %>% select(m_lat) %>% unlist()))



# TR - Started plotting the credibility intervals below ----

# Extract posterior samples
posterior_samples <- as_draws(mod_o)

# Create a new data frame for predictions
new_data <- tibble(
  yr = rep(1873:2023, 6),
  state = c(rep(c("ME", "MA", "NH", "NY"), each = 151), rep("VT", 2 * 151)),
  site = c(rep("NA", 5 * 151), rep("VTH", 151)))

# Generate posterior predictions
posterior_preds <- posterior_predict(mod_o, newdata = new_data)

# Calculate the 95% credibility intervals
cred_intervals <- apply(posterior_preds, 2, quantile, probs = c(0.025, 0.975))

# Plot the original data
plot(d$yr, d$o, pch = 16, xlab = "Year", ylab = "Outcome", main = "Linear Trend with 95% Credibility Intervals")

# Add the fitted line (mean prediction)
lines(new_data$yr, apply(posterior_preds, 2, mean), col = "blue")

# Add the 95% credibility intervals
lines(new_data$yr, cred_intervals[1, ], col = "red", lty = 2)
lines(new_data$yr, cred_intervals[2, ], col = "red", lty = 2)

#===============================================================================