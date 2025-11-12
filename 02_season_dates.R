#===============================================================================
# Script to analyze and plot the beginning and end of season dates
#-------------------------------------------------------------------------------

# Load dependencies ----
if(!existsFunction("brms")) library("brms") 
if(!existsFunction("%>%")) library("tidyverse") 

# Load data ----
if(!exists("d")) source("01_read_data.R")

# Set the priors ----
priors <- c(set_prior("normal(0, 10)", class = "b"),  # fixed effects for the trend over time
            set_prior("normal(0, 10)", class = "sd")) # group-level effects of region and site

# Model a linear trend for the onset (open), first boil, and end  (close) of 
# the season ----
mod_o <- brm(formula = o ~ yr + (1 + yr | region / site),
             data = d %>% filter(!is.na(o)) %>% filter(region != "PRO"), # Have to remove the provincial data for Quebec to avoid double counting
             family = gaussian(),
             prior = priors,
             #weights = d %>% filter(!is.na(o)) %>% select(w) %>% ungroup(), # TR - Need to look more into how to treat region-averages versus site values.
             chains = 4, cores = 4, iter = 4000,
             control = list(adapt_delta = 0.99))
summary(mod_o)
plot(mod_o)
mod_c <- brm(formula = c ~ yr + (1 + yr | region / site),
             data = d %>% filter(!is.na(c)) %>% filter(region != "OMSPA" & region != "PRO"), # Have to remove the provincial data for Ontario and Quebec to avoid double counting
             family = gaussian(),
             prior = priors,
             chains = 4, cores = 4, iter = 4000,
             control = list(adapt_delta = 0.99, max_treedepth = 11))
summary(mod_c)
plot(mod_c)
mod_b <- brm(formula = b ~ yr + (1 + yr | region / site), 
             data = d %>% filter(!is.na(b)) %>% filter(region != "OMSPA"), # Have to remove OMSPA to avoid double counting
             family = gaussian(),
             prior = priors,
             chains = 4, cores = 4, iter = 6000,
             control = list(adapt_delta = 0.99, max_treedepth = 12))
summary(mod_b)
plot(mod_b)

# Extract fixed effects and random effects ----
o_fixed_effects <- fixef(mod_o)  # Fixed effects (intercept and slope)
o_random_effects <- ranef(mod_o)  # Random effects by group (region)
c_fixed_effects <- fixef(mod_c)  # Fixed effects (intercept and slope)
c_random_effects <- ranef(mod_c)  # Random effects by group (region)
b_fixed_effects <- fixef(mod_b)  # Fixed effects (intercept and slope)
b_random_effects <- ranef(mod_b)  # Random effects by group (region)

# Plot NASS data for Massachusetts, Maine, New Hampshire, Vermont, New York, 
# and Pennsylvania plus individual site data from Minnesota and Vermont, as 
# well as the Ontario data from CFS and OMSPA ----- 

# Define the layout matrix
layout_matrix <- matrix(c(1:6, 6:29), nrow = 6, byrow = TRUE)

# Set the layout ----
layout(layout_matrix, widths = c(rep(1, 5), 2, rep(1, 22)), heights = rep(1, 5))
#layout.show(24)

# Set plot margins ----
par(mar = c(5, 5, 1, 1))

# Initialize administrative regions of Ontario and Quebec ----
ON <- c("AL", "AQ", "EA", "GB", "HK", "LA", "OV", "QI", "SI", "SW", "WW")
QC <- c("BSL", "BEA", "CDQ", "CDS", "EST", "LAN", "LAU", "MAU", "MOE", "MOO", 
        "QUE")
  
# Loop over regions, as there is one plot per region (i.e., US States and 
# administrative regions in Ontario and Quebec) ----
for (region in c("ME", "MA", "NH", "NY", "MN", "VT", "CFS", ON, QC)){
  
  # Determine if there are individual sites in a region (NA for region-wide averages)
  if (region != "VT" & region != "MN") {
    sites <- "NA"
  } else if (region == "VT") {
    sites <- c("NA", "VTF", "VTH", "VTC")
  } else if (region == "MN") {
    sites <- "STJ"
  }
  
  # Loop over individual sites in each region ----
  for (site in sites) {
    if (region != "VT" | (region == "VT" & site == "NA")) {
      plot(x = d$yr[d$region == region & d$site == site], 
           y = d$o[d$region == region & d$site == site], 
           pch = ifelse(site == "NA", 21, 24), 
           lwd = 1.5, 
           col = ifelse(site == "NA", "black", "darkgray"),
           xlim = c(ifelse(region != "VT", 
                           ifelse(region != "MN", 1940, 1940), 1870), 2025), 
           ylim = c(30, 150),
           axes = FALSE, 
           xlab = "Year", ylab = "Day of the year")
      text(x = 2025, y = 140, adj = 1, pos = 2,
           label = case_when(
             region == "MA" ~ "Massachusetts",
             region == "ME" ~ "Maine",
             region == "NH" ~ "New Hampshire",
             region == "NY" ~ "New York",
             region == "VT" ~ "Vermont",
             region == "PA" ~ "Pensylvannia",
             region == "MN" ~ "Minnesota",
             region == "CFS" ~ "Ontario Forest Service",
             region == "AL" ~ "Algoma",
             region == "AQ" ~ "Algonquin",
             region == "EA" ~ "Eastern Ontario",
             region == "GB" ~ "Grey-Bruce",
             region == "HK" ~ "Haliburton-Kawartha",
             region == "LA" ~ "Lanark",
             region == "OV" ~ "Ottawa Valley",
             region == "QU" ~ "Quinte",
             region == "SI" ~ "Simcoe",
             region == "SW" ~ "Southwestern Ontario",
             region == "WW" ~ "Waterloo-Wellington",
             region == "BSL" ~ "Bas-Saint-Laurent & Gaspésie",
             region == "BEA" ~ "Beauce",
             region == "CDQ" ~ "Centre-du-Québec",
             region == "CDS" ~ "Cote-du-Sud",
             region == "EST" ~ "Estrie",
             region == "LAN" ~ "Lanaudière",
             region == "LAU" ~ "Laurentides",
             region == "MAU" ~ "Mauricie",
             region == "MOE" ~ "Montérégie-Est",
             region == "MOO" ~ "Montérégie-Ouest",
             region == "QUE" ~ "Capitale-Nationale"))
      if (region == "MN") {
        x_ats <- seq(1940, 2020, by = 10)
      } else if (region %in% ON) { # Ontario
        x_ats <- seq(1940, 2020, by = 20)
      } else if (region %in% QC) { # Quebec
        x_ats <- seq(1940, 2020, by = 20)
      } else if (region != "VT") {
        x_ats <- seq(1940, 2020, by = 10)
      } else {
        x_ats <- seq(1880, 2020, by = 20)
      }
      axis(side = 1, at = x_ats)
      axis(side = 2, las = 1)
      points(x = d$yr[d$region == region & d$site == site], 
             y = d$c[d$region == region & d$site == site], 
             pch =  ifelse(site == "NA", 19, 25), 
             lwd = 1.5, 
             bg = ifelse(site == "NA", "black", "darkgray"),
             col = ifelse(site == "NA", "black", "darkgray"))
      if (site == "STJ" | region == "CFS" | region %in% c(ON, QC)) {
        points(x = d$yr[d$region == region & d$site == site], 
               y = d$b[d$region == region & d$site == site], 
               pch = 4, lwd = 1.5, 
               col = ifelse(region %in% c(ON, QC), "black", "darkgray"))
      }
    } else if (site %in% c("VTC", "VTH", "VTF", "STJ")) {
      points(x = d$yr[d$region == region & d$site == site], 
             y = d$b[d$region == region & d$site == site], 
             pch = 4, lwd = 1.5, 
             col = ifelse(site %in% c("VTC", "VTH", "VTF"), "#154734", 
                          "darkgray"))
      if (site %in% c("VTH", "VTF")) {
        points(x = d$yr[d$region == region & d$site == site], 
               y = d$o[d$region == region & d$site == site], 
               pch = 24, lwd = 1.5, col = "darkgray")
        points(x = d$yr[d$region == region & d$site == site], 
               y = d$c[d$region == region & d$site == site], 
               pch = 25, lwd = 1.5, col = "darkgray", bg = "darkgray")
      }
    }
    
    # Plot open and close trends for all sites except for VTC and CFS----
    if (site != "VTC" & region != "CFS") {
      
      # Calculate the intercept and slope for the chosen region for season open 
      if(!(region %in% c(ON, QC))){ # No season open for OMSPA and PPAQ
        o_intercept <- o_fixed_effects["Intercept", "Estimate"] + 
          o_random_effects$region[region, "Estimate", "Intercept"] +
          o_random_effects$`region:site`[paste0(region, "_", site), "Estimate", "Intercept"]
  
        o_slope <- o_fixed_effects["yr", "Estimate"] + 
          o_random_effects$region[region, "Estimate", "yr"] +
          o_random_effects$`region:site`[paste0(region, "_", site), "Estimate", "yr"]
        
        # Add the linear trends for beginning of the season ----
        abline(a = o_intercept, b = o_slope, 
               col = ifelse(site == "NA", "black", "darkgray"),
               lwd = 2, lty = 2)
      }
      # Calculate intercept and slope for season close 
      c_intercept <- c_fixed_effects["Intercept", "Estimate"] + 
        c_random_effects$region[region, "Estimate", "Intercept"] +
        c_random_effects$`region:site`[paste0(region, "_", site), "Estimate", "Intercept"]

      c_slope <- c_fixed_effects["yr", "Estimate"] + 
        c_random_effects$region[region, "Estimate", "yr"] +
        c_random_effects$`region:site`[paste0(region, "_", site), "Estimate", "yr"]
      # Add credibility interval for the trend lines ----
 
      # Add the linear trends for beginning of the season ----
      abline(a = c_intercept, b = c_slope, 
             col = ifelse(site == "NA", "black", "darkgray"),
             lwd = 2, lty = 1)
    } # End site != VTC & region != "CFS" condition 
    
    # Plot first boil dates for VTH, VTC, VTF, and STJ ----
    if (site %in% c("VTH", "VTC", "STJ") | region %in% c("CFS", ON, QC)) {
      b_intercept <- b_fixed_effects["Intercept", "Estimate"] + 
        b_random_effects$region[region, "Estimate", "Intercept"] + 
        b_random_effects$`region:site`[paste0(region,"_",site), "Estimate", "Intercept"]
      b_slope <- b_fixed_effects["yr", "Estimate"] + 
        b_random_effects$region[region, "Estimate", "yr"] + 
        b_random_effects$`region:site`[paste0(region,"_",site), "Estimate", "yr"]
      
      # Add the linear trends for first boil of the season ----
      abline(a = b_intercept, b = b_slope, 
             col = ifelse(site == "VTC", "#154734", 
                          ifelse(site == "VTH", "#FFD416", 
                                 ifelse (region == "CFS" | site == "STJ", 
                                         "darkgray", "black"))),
             lwd = 2, lty = 3)
    } 
    
  } # End site loop  
} # End loop over regions


# Plot change in season close versus latitude ----
par(mar = c (5, 6, 1, 1), mfrow = c(3, 1))
plot(x = d %>% group_by(region) %>% summarise(m_lat = mean(m_lat)) %>% 
       filter(region %in% c(ON, QC, "MA", "ME", "MN", "NH", "NY", "VT")) %>% 
       dplyr::select(m_lat) %>% unlist(), # Remove the ON and QC provincial averages and PA
     y = c_random_effects$region[, "Estimate", "yr"], 
     pch = 19, axes = FALSE, xlim = c(42, 48), ylim = c (-0.0025, 0.0045),
     xlab = expression(paste("Latitude (",degree,")")),
     ylab = "", cex = 1.2)
abline(h = 0, col ="black")
points(x = d %>% group_by(site) %>% summarise(m_lat = mean(m_lat)) %>% 
         filter (site %in% c("STJ", "VTH", "VTU", "VTF")) %>% 
         dplyr::select(m_lat) %>% unlist(),
       y = c_random_effects$`region:site`[c("MN_STJ", "VT_VTH", "VT_VTU", "VT_VTF"), "Estimate", "yr"],
       pch = 19, col = "darkgray", cex = 1.2)
mtext(side = 2, line = 4, text = expression(beta[close]))
axis(side = 1, at = seq(42, 48))
axis(side = 2, las = 1)
abline(lm(c_random_effects$region[ , "Estimate", "yr"] ~ 
            d %>% group_by(region) %>% summarise(m_lat = mean(m_lat)) %>% 
            filter (region %in% c(ON, QC, "MA", "ME", "MN", "NH", "NY", "VT")) %>% 
            dplyr::select(m_lat) %>% unlist()),
       lty = 2, lwd = 2)

# Plot change in first boil versus latitude ----
plot(x = d %>% group_by(region) %>% summarise(m_lat = mean(m_lat)) %>% 
       filter (region %in% c(ON, QC, "MN", "VT")) %>% 
       dplyr::select(m_lat) %>% unlist(),
     y = b_random_effects$region[-c(6, 20) , "Estimate", "yr"],
     pch = 19, axes = FALSE, xlim = c(42, 48), ylim = c (-0.0025, 0.0045),
     xlab = expression(paste("Latitude (",degree,")")),
     ylab = "", cex = 1.2)
abline(h = 0, col = "black")
points(x = d %>% group_by(site) %>% summarise(m_lat = mean(m_lat)) %>% 
         filter (site %in% c("VTH", "VTC", "VTU", "STJ")) %>% 
         dplyr::select(m_lat) %>% unlist(),
       y = b_random_effects$`region:site`[c("MN_STJ", "VT_VTH", "VT_VTC", "VT_VTU"), "Estimate", "yr"],
       pch = 19, col = "darkgray", cex = 1.2)
mtext(side = 2, line = 4, text = expression(beta[boil]))
axis(side = 1, at = seq(42, 48))
axis(side = 2, las = 1)
abline(lm(b_random_effects$region[-c(6, 20), "Estimate", "yr"] ~ 
            d %>% group_by(region) %>% summarise(m_lat = mean(m_lat)) %>% 
            filter (region %in% c(ON, QC, "MN", "VT")) %>% 
            dplyr::select(m_lat) %>% unlist()),
       lty = 2, lwd = 2, col ="black")


# Plot change in season open versus latitude ----
plot(x = d %>% group_by(region) %>% summarise(m_lat = mean(m_lat)) %>% 
       filter (region %in% c("MA", "ME", "MN", "NH", "NY", "VT")) %>% 
       dplyr::select(m_lat) %>% unlist(),
     y = o_random_effects$region[, "Estimate", "yr"],
     pch = 19, axes = FALSE, xlim = c(42, 48), ylim = c (-0.0025, 0.0025),
     xlab = expression(paste("Latitude (",degree,")")),
     ylab = "", cex = 1.2)
abline(h = 0, col ="black")
points(x = d %>% group_by(site) %>% summarise(m_lat = mean(m_lat)) %>% 
         filter (site %in% c("VTH", "VTF", "STJ")) %>% 
         dplyr::select(m_lat) %>% unlist(),
       y = o_random_effects$`region:site`[c("MN_STJ", "VT_VTH", "VT_VTF"), "Estimate", "yr"],
       pch = 19, col = "darkgray", cex = 1.2)
mtext(side = 2, line = 4, text = expression(beta[open]))
axis(side = 1, at = seq(42, 48))
axis(side = 2, las = 1)
abline(lm(o_random_effects$region[, "Estimate", "yr"] ~ 
            d %>% group_by(region) %>% 
            summarise(m_lat = mean(m_lat)) %>% 
            filter (region %in% c("MA", "ME", "MN", "NH", "NY", "VT")) %>% 
            dplyr::select(m_lat) %>% unlist()),
       lty = 2, lwd = 2)
