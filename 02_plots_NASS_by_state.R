#===============================================================================
# Script to plot the beginning and end of season dates
#-------------------------------------------------------------------------------


# Load data ----
if(!exists("d")) source("01_read_data.R")

# Plot NASS data for Maine ----
plot(x = d$y[d$state == "ME"], 
     y = d$b[d$state == "ME"], pch = 21,
     xlim = c(1960, 2025), ylim = c(40, 140),
     axes = FALSE, xlab = "Year", ylab ="Day of the year")
axis(side = 1)
axis(side = 2, las = 1)
points(x = d$y[d$state == "ME"], 
       y = d$e[d$state == "ME"], pch = 19)

# Plot NASS data for Massachusetts ----
plot(x = d$y, 
     y = d$b_MA, pch = 21,
     xlim = c(1960, 2025), ylim = c(40, 140),
     axes = FALSE, xlab = "Year", ylab ="Day of the year")
axis(side = 1)
axis(side = 2, las = 1)
points(x = d$y, 
       y = d$e_MA, pch = 19)

# Plot NASS data for New Hampshire ----
plot(x = d$y, 
     y = d$b_NH, pch = 21,
     xlim = c(1960, 2025), ylim = c(40, 140),
     axes = FALSE, xlab = "Year", ylab ="Day of the year")
axis(side = 1)
axis(side = 2, las = 1)
points(x = d$y, 
       y = d$e_NH, pch = 19)

# Plot NASS data for Vermont ----
plot(x = d$y, 
     y = d$b_VT, pch = 21,
     xlim = c(1960, 2025), ylim = c(40, 140),
     axes = FALSE, xlab = "Year", ylab ="Day of the year")
axis(side = 1)
axis(side = 2, las = 1)
points(x = d$y, 
       y = d$e_VT, pch = 19)

# Plot NASS data for New York ----
plot(x = d$y, 
     y = d$b_NY, pch = 21,
     xlim = c(1960, 2025), ylim = c(40, 140),
     axes = FALSE, xlab = "Year", ylab ="Day of the year")
axis(side = 1)
axis(side = 2, las = 1)
points(x = d$y, 
       y = d$e_NY, pch = 19)

# Plot relation between early onset, late end and longer seasons with higher 
# yield ----
plot(x = d$b_VT, y = d$yield, pch = 25, axes = FALSE, bg = "darkgrey",
     xlim = c(50, 82), ylim = c(0.1, 0.45),
     xlab = "Season start (days)", ylab = "Syrup yield (gal/tap)")
axis(side = 1)
axis(side = 2, las = 1)
plot(x = d$e_VT, y = d$yield, pch = 24, axes = FALSE, bg = "darkgrey",
     xlim = c(80, 120), ylim = c(0.1, 0.45),
     xlab = "Season end (day of year)", ylab = "Syrup yield (gal/tap)")
axis(side = 1)
axis(side = 2, las = 1)
plot(x = d$d_VT, y = d$yield, pch = 23, axes = FALSE, bg = "darkgrey",
     xlim = c(80, 120), ylim = c(0.1, 0.45),
     xlab = "Season duration (days)", ylab = "Syrup yield (gal/tap)")
axis(side = 1)
axis(side = 2, las = 1)

#===============================================================================
