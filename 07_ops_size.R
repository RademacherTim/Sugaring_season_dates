#===============================================================================
# Script to understand and plot the change in operation size over time 
#-------------------------------------------------------------------------------

# To-do list ----
# - TR Add Ontario numbers of taps over time to the graphic

# Load dependencies ----
if(!existsFunction("brms")) library("brms") 

# Load data ----
if(!exists("d")) source("01_read_data.R")

# Fit a model to the operation size ----
# mod_s <- brm(formula =  ~ yr + (1 + yr | region / site),
#              data = d %>% filter(!is.na(o)), 
#              family = gaussian(),
#              prior = priors,
#              #weights = d %>% filter(!is.na(o)) %>% select(w) %>% ungroup(), # TR - Need to look more into how to treat region-averages versus site values.
#              chains = 4, cores = 4, iter = 4000,
#              control = list(adapt_delta = 0.95))

# Plot operation size over time ----
par(mar = c(5, 6, 1, 1), mfrow = c(1, 1))
plot(x = d_cens$yr[d_cens$region == "ME"],
     y = d_cens$m_taps[d_cens$region == "ME"],
     pch = 19, typ = "b", col = "#C60C30", axes = FALSE,
     xlim = c(1995, 2025), ylim = c(0, 6000),
     xlab = "Year",
     ylab = "")
mtext(side = 2, line = 4, text = "Number of tabs per operation (n)")
# abline(lm(d_cens$m_taps[d_cens$region == "ME"] ~ 
#             d_cens$yr[d_cens$region == "ME"]), col = "#C60C30", lwd = 1)
axis(side = 1)
axis(side = 2, las = 1)
points(x = d_cens$yr[d_cens$region == "VT"],
       y = d_cens$m_taps[d_cens$region == "VT"],
       pch = 19, col = "#78BE21")
lines(x = d_cens$yr[d_cens$region == "VT"],
      y = d_cens$m_taps[d_cens$region == "VT"],
      col = "#78BE21")
# abline(lm(d_cens$m_taps[d_cens$region == "VT"] ~ 
#             d_cens$yr[d_cens$region == "VT"]), col = "#78BE21", lwd = 1)
points(x = d_cens$yr[d_cens$region == "NH"],
       y = d_cens$m_taps[d_cens$region == "NH"],
       pch = 19, col = "#FDBB00")
lines(x = d_cens$yr[d_cens$region == "NH"],
      y = d_cens$m_taps[d_cens$region == "NH"],
      col = "#FDBB00")
# abline(lm(d_cens$m_taps[d_cens$region == "NH"] ~ 
#             d_cens$yr[d_cens$region == "NH"]), col = "#FDBB00", lwd = 1)
points(x = d_cens$yr[d_cens$region == "NY"],
       y = d_cens$m_taps[d_cens$region == "NY"],
       pch = 19, col = "#ffd100")
lines(x = d_cens$yr[d_cens$region == "NY"],
      y = d_cens$m_taps[d_cens$region == "NY"],
      col = "#ffd100")
# abline(lm(d_cens$m_taps[d_cens$region == "NY"] ~ 
#             d_cens$yr[d_cens$region == "NY"]), col = "#ffd100", lwd = 1)
# points(x = d_cens$yr[d_cens$region == "MN"],
#        y = d_cens$m_taps[d_cens$region == "MN"],
#        pch = 19, col = "#003865")
# abline(lm(d_cens$m_taps[d_cens$region == "MN"] ~ 
#             d_cens$yr[d_cens$region == "MN"]), col = "#003865", lwd = 1)
points(x = d_cens$yr[d_cens$region == "PA"],
       y = d_cens$m_taps[d_cens$region == "PA"],
       pch = 19, col = "#002244")
lines(x = d_cens$yr[d_cens$region == "PA"],
      y = d_cens$m_taps[d_cens$region == "PA"],
      col = "#002244")
# abline(lm(d_cens$m_taps[d_cens$region == "PA"] ~ 
#             d_cens$yr[d_cens$region == "PA"]), col = "#002244", lwd = 1)
points(x = d_cens$yr[d_cens$region == "MA"],
       y = d_cens$m_taps[d_cens$region == "MA"],
       pch = 19, col = "#680A1D")
lines(x = d_cens$yr[d_cens$region == "MA"],
      y = d_cens$m_taps[d_cens$region == "MA"],
      col = "#680A1D")
# abline(lm(d_cens$m_taps[d_cens$region == "MA"] ~ 
#             d_cens$yr[d_cens$region == "MA"]), col = "#680A1D", lwd = 1)

#===============================================================================
