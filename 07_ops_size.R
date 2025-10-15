#===============================================================================
# Script to understand and plot the change in operation size over time 
#-------------------------------------------------------------------------------

# Take-away messages: 
# 1) The tap count has not increased significantly across all region for which 
#    we have data. 
# 2) Some regions, such as Vermont have seen clear increases in operation size.
# 3) More site-level data is necessary including tap counts to test whether 
#    increasing tap count do lead to earlier tapping. 

# Load dependencies ----
if(!existsFunction("brms")) library("brms") 

# Load data ----
if(!exists("d")) source("01_read_data.R")

# Set the priors ----
priors <- c(set_prior("normal(0, 10)", class = "b"),  # fixed effects for the trend over time
            set_prior("normal(0, 10)", class = "sd")) # group-level effects of region and site

# Create data frame with d_cens data and Ontario data on operation sizes ----
d_taps <- d %>% filter(!is.na(ntaps)) %>% filter(region != "OMSPA") %>% 
  select(yr, region, ntaps)
d_taps <- rbind(d_taps, 
                d_cens %>% 
                  filter(!is.na(ntaps)) %>% 
                  dplyr::select(yr, region, ntaps))


# Fit a model to the operation size ----
mod_taps <- brm(formula = ntaps ~ yr + (1 + yr | region),
             data = d_taps, 
             family = gaussian(),
             prior = priors,
             chains = 4, cores = 4, iter = 4000)
summary(mod_taps)
plot(mod_taps)

# Plot operation size over time ----
par(mar = c(5, 6, 1, 1), mfrow = c(1, 1))
plot(x = d_cens$yr[d_cens$region == "ME"],
     y = d_cens$ntaps[d_cens$region == "ME"],
     pch = 19, typ = "b", col = "white", axes = FALSE,
     xlim = c(1995, 2025), ylim = c(0, 6500),
     xlab = "Year",
     ylab = "")
mtext(side = 2, line = 4, text = "Number of tabs per operation (n)")
axis(side = 1)
axis(side = 2, las = 1)

# Loop over states in the USA and Ontario ----
for (region in c("ME", "MA", "NH", "NY", "OMSPA", "PA", "VT")) { 
  # Determine colour 
  colour <- case_when(
    region == "ME" ~ "#C60C30",
    region == "MA" ~ "#680A1D",
    region == "NH" ~ "#FDBB00",
    region == "NY" ~ "#FFD100",
    region == "PA" ~ "#002244",
    region == "VT" ~ "#78BE21",
    region == "OMSPA" ~ "#800080"
  )
  
  # Add data from census ----
  if(region != "OMSPA"){
    
    # Add a little bit of jitter to x ----
    x_jittered <- jitter(d_cens$yr[d_cens$region == region], amount = 0.3)
    
    points(x = x_jittered,
           y = d_cens$ntaps[d_cens$region == region],
           pch = 19, col = colour)
    arrows(x0 = x_jittered,
           x1 = x_jittered,
           y0 = d_cens$ntaps[d_cens$region == region] - 
             d_cens$cv_taps[d_cens$region == region] / 100 * d_cens$ntaps[d_cens$region == region],
           y1 = d_cens$ntaps[d_cens$region == region] + 
             d_cens$cv_taps[d_cens$region == region] / 100  * d_cens$ntaps[d_cens$region == region],
           col = colour, angle = 90, code = 3, length = 0.05, lwd = 2)
    lines(x = x_jittered,
          y = d_cens$ntaps[d_cens$region == region],
          col = colour)
  # Add data for Ontario ----
  } else if (region == "OMSPA") {
    lines(x = d$yr[d$region == region],
          y = d$ntaps[d$region == region],
          col = colour)
    points(x = d$yr[d$region == region],
           y = d$ntaps[d$region == region],
           pch = 19, col = colour)
  }
}
#===============================================================================
