#===============================================================================
# Script to plot the beginning and end of season dates
#-------------------------------------------------------------------------------

# Load dependencies ----
if(!existsFunction("brms")) library("brms") 

# Load data ----
if(!exists("d")) source("01_read_data.R")

# Set the priors ----
priors <- c(set_prior("normal(0, 1)", class = "b"),  # fixed effects for the trend over time
            set_prior("normal(0, 1)", class = "sd")) # group-level effects of state and site

# Fit a linear relationship to the yield versus season duration ----
mod_y <- brm(formula = y ~ d + (d | site),
             data = d %>% filter(!is.na(d)), 
             family = gaussian(),
             prior = priors, 
             chains = 4, cores = 4, iter = 6000,
             control = list(adapt_delta = 0.99, max_treedepth = 13))
summary(mod_y)
plot(mod_y)

# Fit a linear relationship to the yield versus season open and close ----
# mod_yd <- brm(formula = y ~ o + c + (1 | site),
#              data = d %>% filter(!is.na(d)), 
#              family = gaussian(),
#              prior = priors, 
#              chains = 4, cores = 4, iter = 6000,
#              control = list(adapt_delta = 0.99, max_treedepth = 13))
# summary(mod_yd)
# plot(mod_yd)

# Extract fixed effects and random effects ----
y_fixed_effects <- fixef(mod_y)  # Fixed effects (intercept and slope)
y_random_effects <- ranef(mod_y)  # Random effects by group (state)

# Plot duration of the season versus the yield ----
par(mfrow = c(1, 1))
plot (x = d$d[d$state == "VT"],
      y = d$y[d$state == "VT"],
      pch = 19, col = "#154734", cex = 1.2,
      axes = FALSE, xlim = c(0, 65), ylim = c(0, 0.45),
      xlab = "Season duration (days)",
      ylab = "Average yield (gal / tap)")
axis(side = 1)
axis(side = 2, las = 1)
points(x = d$d[d$state == "MN"],
       y = d$y[d$state == "MN"],
       pch = 19, col = "#cd1041")

# Add the linear relationship for VT ----
y_intercept <- y_fixed_effects["Intercept", "Estimate"] + 
  y_random_effects$site["NA", "Estimate", "Intercept"]
y_slope <- y_fixed_effects["d", "Estimate"] + 
  y_random_effects$site["NA", "Estimate", "d"]
abline(a = y_intercept, b = y_slope, 
       col = "#154734", lwd = 2, lty = 1)

# Add the linear relationship for STJ ----
y_intercept <- y_fixed_effects["Intercept", "Estimate"] + 
  y_random_effects$site["STJ", "Estimate", "Intercept"]
y_slope <- y_fixed_effects["d", "Estimate"] + 
  y_random_effects$site["STJ", "Estimate", "d"]
abline(a = y_intercept, b = y_slope, 
       col = "#cd1041", lwd = 2, lty = 1)

# Season open versus yield ----
par(mfrow = c(1, 1))
plot (x = d$o[d$state == "VT"],
      y = d$y[d$state == "VT"],
      pch = 15, col = "#154734", cex = 1.2,
      axes = FALSE, xlim = c(40, 90), ylim = c(0, 0.45),
      xlab = "Season open (days)",
      ylab = "Average yield (gal / tap)")
axis(side = 1)
axis(side = 2, las = 1)
points(x = d$o[d$state == "MN"],
       y = d$y[d$state == "MN"],
       pch = 18, cex = 1.3, col = "#cd1041")

# Season close versus yield ----
par(mfrow = c(1, 1))
plot (x = d$c[d$state == "VT"],
      y = d$y[d$state == "VT"],
      pch = 15, col = "#154734", cex = 1.2,
      axes = FALSE, xlim = c(70, 140), ylim = c(0, 0.45),
      xlab = "Season close (days)",
      ylab = "Average yield (gal / tap)")
axis(side = 1)
axis(side = 2, las = 1)
points(x = d$c[d$state == "MN"],
       y = d$y[d$state == "MN"],
       pch = 18, cex = 1.3, col = "#cd1041")
# There might be a positive relationship between season close and yield for STJ

#===============================================================================
