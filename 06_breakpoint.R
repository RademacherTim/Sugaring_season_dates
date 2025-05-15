#===============================================================================
# Script to analyze and plot the beginning and end of season dates with a single 
# global break-point
#-------------------------------------------------------------------------------

# Load dependencies ----
if(!existsFunction("brms")) library("brms") 

# Load data ----
if(!exists("d")) source("01_read_data.R")


# Define the nonlinear formula that include a global breakpoint (bp)
breakpoint_formula <- bf(
    c ~ a + b1 * yr + b2 * (yr - cp) * step(yr - cp),
    a + b1 + b2 + cp ~ 1,
    nl = TRUE
  )


# Model of season close with a breakpoint to allow for a single change ----
mod_cb <- brm(formula = c ~ s(yr) + (yr | state / site),
              data = d %>% filter(!is.na(c)), 
              family = gaussian(),
              prior = priors,
              chains = 4, cores = 4, iter = 4000,
              control = list(adapt_delta = 0.95))
summary(mod_cb)
plot(mod_cb)
