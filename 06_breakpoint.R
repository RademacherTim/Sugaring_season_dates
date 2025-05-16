#===============================================================================
# Script to analyze and plot the beginning and end of season dates with a single 
# global break-point
#-------------------------------------------------------------------------------

# Load dependencies ----
if(!existsFunction("brms")) library("brms") 
library ("segmented")

# Load data ----
if(!exists("d")) source("01_read_data.R")

# Frequentist break point analysis for all data ----
lm_fit_all <- lm(c ~ yr, data = d %>% filter(!is.na(c)))

# Add a breakpoint (you can specify initial guess with psi)
seg_fit_all <- segmented(lm_fit_all, seg.Z = ~ yr, psi = list(yr = 2000))

# Summary and plot
summary(seg_fit_all)
plot(seg_fit_all)

# Frequentist break point analysis for NASS data only ----
lm_fit_NASS <- lm(c ~ yr, data = d %>% filter(!is.na(c) & source == "NASS"))

# Add a breakpoint (you can specify initial guess with psi)
seg_fit_NASS <- segmented(lm_fit_NASS, seg.Z = ~ yr, psi = list(yr = 2000))

# Summary and plot
summary(seg_fit_NASS)
plot(seg_fit_NASS)

# Step 1: Create a new predictor for the post-breakpoint slope
d$after_1990 <- pmax(0, d$yr - 1990)

# Step 2: Fit the model
mod_cb <- brm(
  formula = c ~ yr + after_1990 + (yr | state / site),
  data = d %>% filter(!is.na(c)),
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "b"),         # priors for fixed effects
    prior(normal(0, 10), class = "Intercept"), # prior for intercept
    prior(exponential(1), class = "sd")        # prior for random effects
  ),
  control = list(adapt_delta = 0.96, max_treedepth = 12),
  cores = 4, chains = 4, iter = 6000
)

summary(mod_cb)
plot(mod_cb)


# Extract fitted values and credible intervals ----
fitted_values <- fitted(mod_cb, summary = TRUE)

# Extract observed data ----
observed_data <- mod_cb$data

# Plot observed data ----
plot(x = d$yr[d$source == "NASS"], 
     y = d$c[d$source == "NASS"], pch = 19, col = "black",
     xlab = "Year", ylab = "Season close", main = "", axes = FALSE)
points(x = d$yr[d$source == "IND"], 
       y = d$c[d$source == "IND"], pch = 19, col = "darkgray")
axis(side = 1, at = seq(1870, 2020, by = 30))
axis(side = 2, las = 1)

# Get coeffiecents ----
coef <- coef(seg_fit_all)

# Define start and end points 
x1 <- 1873
x2 <- 1990
x3 <- 2023 
y1 <- coef[1] + coef[2] * x1
y2 <- coef[1] + coef[2] * x2
y3 <- coef[1] + coef[2] * x2 + coef[3] * (x2-x2)
y4 <- coef[1] + coef[2] * x3 + coef[3] * (x3-x2) 
  
# Add the line segment
segments(x1, y1, x2, y2, col = "darkred", lwd = 3, lty = 2)
segments(x2, y3, x3, y4, col = "darkred", lwd = 3, lty = 2)

# Get coeffiecents ----
coef <- coef(seg_fit_NASS)

# Define start and end points 
x1 <- 1960
x2 <- 1984
x3 <- 2023 
y1 <- coef[1] + coef[2] * x1
y2 <- coef[1] + coef[2] * x2
y3 <- coef[1] + coef[2] * x2 + coef[3] * (x2-x2)
y4 <- coef[1] + coef[2] * x3 + coef[3] * (x3-x2) 

# Add the line segment
segments(x1, y1, x2, y2, col = "darkred", lwd = 3)
segments(x2, y3, x3, y4, col = "darkred", lwd = 3)

# Add fitted values
lines(observed_data$yr, fitted_values[, "Estimate"], col = "red", lwd = 2)

# Add 95% credible intervals
lines(observed_data$yr, fitted_values[, "Q2.5"], col = "red", lwd = 1, lty = 2)
lines(observed_data$yr, fitted_values[, "Q97.5"], col = "red", lwd = 1, lty = 2)

# Add vertical line at breakpoint (1990)
abline(v = 1990, col = "blue", lty = 2, lwd = 2)

