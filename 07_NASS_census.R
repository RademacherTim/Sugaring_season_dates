#===============================================================================
# Script to understand and plot the change in operation size over time 
#-------------------------------------------------------------------------------

# Load dependencies ----
if(!existsFunction("brms")) library("brms") 

# Load data ----
if(!exists("d")) source("01_read_data.R")

# Extract number of taps and operations per state ----
n_taps <- d_census %>% 
  filter(`Data Item` == "MAPLE SYRUP - NUMBER OF TAPS",
         Domain == "TOTAL", 
         `Domain Category` == "NOT SPECIFIED",
         `Geo Level` == "STATE") %>% 
  select(-c(county, `Geo Level`, `Zip Code`, `Data Item`, Domain,
            `Domain Category`, CV))
n_ops_taps <- d_census %>% 
  filter(`Data Item` == "MAPLE SYRUP - OPERATIONS WITH TAPS",
         Domain == "TOTAL", 
         `Domain Category` == "NOT SPECIFIED",
         `Geo Level` == "STATE")  %>% 
  select(-c(county, `Geo Level`, `Zip Code`, `Data Item`, Domain,
            `Domain Category`, CV))
n_ops_sales <- d_census %>% 
  filter(`Data Item` == "MAPLE SYRUP - OPERATIONS WITH SALES",
         Domain == "TOTAL", 
         `Domain Category` == "NOT SPECIFIED",
         `Geo Level` == "STATE")  %>% 
  select(-c(county, `Geo Level`, `Zip Code`, `Data Item`, Domain,
            `Domain Category`, CV))


# Merge the number of taps and operations ----
ops_size <- inner_join(n_taps, n_ops_taps, by = c("source", "yr", "state")) %>% 
  rename(n_taps = "value.x",
         n_ops = "value.y") %>% 
  mutate(op_size = n_taps / n_ops)

# Plot operation size over time ----
par(mar = c(5, 6, 1, 1))
plot(x = ops_size$yr[ops_size$state == "MAINE"],
     y = ops_size$op_size[ops_size$state == "MAINE"],
     pch = 19, col = "#C60C30", axes = FALSE,
     xlim = c(1990, 2030), ylim = c(0, 6000),
     xlab = "Year",
     ylab = "")
mtext(side = 2, line = 4, text = "Average number of tabs per operation (n)")
abline(lm(ops_size$op_size[ops_size$state == "MAINE"] ~ 
            ops_size$yr[ops_size$state == "MAINE"]), col = "#C60C30", lwd = 1)
axis(side = 1)
axis(side = 2, las = 1)
points(x = ops_size$yr[ops_size$state == "VERMONT"],
       y = ops_size$op_size[ops_size$state == "VERMONT"],
       pch = 19, col = "#78BE21")
abline(lm(ops_size$op_size[ops_size$state == "VERMONT"] ~ 
            ops_size$yr[ops_size$state == "VERMONT"]), col = "#78BE21", lwd = 1)
points(x = ops_size$yr[ops_size$state == "NEW HAMPSHIRE"],
       y = ops_size$op_size[ops_size$state == "NEW HAMPSHIRE"],
       pch = 19, col = "#FDBB00")
abline(lm(ops_size$op_size[ops_size$state == "NEW HAMPSHIRE"] ~ 
            ops_size$yr[ops_size$state == "NEW HAMPSHIRE"]), col = "#FDBB00", lwd = 1)
points(x = ops_size$yr[ops_size$state == "NEW YORK"],
       y = ops_size$op_size[ops_size$state == "NEW YORK"],
       pch = 19, col = "#ffd100")
abline(lm(ops_size$op_size[ops_size$state == "NEW YORK"] ~ 
            ops_size$yr[ops_size$state == "NEW YORK"]), col = "#ffd100", lwd = 1)
points(x = ops_size$yr[ops_size$state == "MINNESOTA"],
       y = ops_size$op_size[ops_size$state == "MINNESOTA"],
       pch = 19, col = "#003865")
abline(lm(ops_size$op_size[ops_size$state == "MINNESOTA"] ~ 
            ops_size$yr[ops_size$state == "MINNESOTA"]), col = "#003865", lwd = 1)
points(x = ops_size$yr[ops_size$state == "PENNSYLVANIA"],
       y = ops_size$op_size[ops_size$state == "PENNSYLVANIA"],
       pch = 19, col = "#002244")
abline(lm(ops_size$op_size[ops_size$state == "PENNSYLVANIA"] ~ 
            ops_size$yr[ops_size$state == "PENNSYLVANIA"]), col = "#002244", lwd = 1)
points(x = ops_size$yr[ops_size$state == "MASSACHUSETTS"],
       y = ops_size$op_size[ops_size$state == "MASSACHUSETTS"],
       pch = 19, col = "#680A1D")
abline(lm(ops_size$op_size[ops_size$state == "MASSACHUSETTS"] ~ 
            ops_size$yr[ops_size$state == "MASSACHUSETTS"]), col = "#680A1D", lwd = 1)


#===============================================================================
