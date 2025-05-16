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
n_ops <- d_census %>% 
  filter(`Data Item` == "MAPLE SYRUP - OPERATIONS WITH TAPS",
         Domain == "TOTAL", 
         `Domain Category` == "NOT SPECIFIED",
         `Geo Level` == "STATE")  %>% 
  select(-c(county, `Geo Level`, `Zip Code`, `Data Item`, Domain,
            `Domain Category`, CV))

# Merge the number of taps and operations ----
inner_join(n_taps, n_ops, by = c("source", "yr", "state")) %>% 
  rename(n_taps = "value.x",
         n_ops = "value.y") %>% 
  mutate(op_size = n_taps / n_ops)


  
group_by()

# Plot 

#===============================================================================