library(tidyverse)
library(sf)
library(viridis)

setwd("~/UPENN/Box Sync/WISE FELLOWSHIP FALL/FINAL_R_CODE/Data")
load("~/UPENN/Box Sync/WISE FELLOWSHIP FALL/FINAL_R_CODE/Environments/Environment_Target_Dataset.RData")

plot_residuals <- function(regression, title_plot) {
  redisuals_reg <- residuals(regression)
  labels_redisuals_reg <- names(redisuals_reg)
  map_resid <- data.frame(redisuals_reg, labels_redisuals_reg)
  dat_crime$labels_redisuals_reg <- row.names(dat_crime)
  dat_crime_residuals <- left_join(dat_crime, map_resid, by = "labels_redisuals_reg")
  
  ggplot()+
    geom_sf(data = dat_crime_residuals, aes(fill = redisuals_reg), color = NA) +
    geom_sf(data = schools, aes(), color = 'white', size = .6) +
    scale_fill_viridis() +
    labs(title = title_plot, 
         subtitle = "Schools in white",
         fill = "Residuals") +
    mapTheme()
}

schools <- read.csv("school_master.csv")
schools <- schools %>%
  dplyr::rename(level = School.Level)
schools = schools %>%
  select(lon, lat, level)
schools <- st_as_sf(schools, coords = c("lon", "lat"), crs = 4326)
schools <- schools %>%
  st_transform(st_crs(2272))

philly_blocks <- st_read("Census_Blocks_2010.shp")
philly_blocks <- st_transform(philly_blocks, 2272)
philly_blocks <- philly_blocks %>% 
  select(GEOID10, geometry)

#### regressions ####

reg1 <- lm(log(crime_index + .1) ~ 
             log(family_in_pov + .1) +
             (1 - pct_white) +
             pct_hisp +
             pct_young_male +
             log(median_hh_inc +.1) +
             avg_litter +
             log(code_violation_index + .1) +
             log(aband_vehicl_index + .1) +
             log(dumping_index + .1) +
             log(light_out_index + .1) +
             log(graffiti_index + .1) +
             log(infestation_index + .1) +
             log(bad_street_index + .1) +
             pct_civic +
             pct_culture +
             pct_comercial +
             pct_industrial +
             pct_residential +
             pct_transport + 
             pct_vacant +
             log(distance_cafe) +
             log(distance_conv) +
             log(distance_gym) +
             log(distance_institution) +
             log(distance_liquor) +
             log(distance_lodge) +
             log(distance_nightlife) +
             log(distance_restaurant) +
             log(distance_retail) +
             near_school,
           data = dat_final)

reg5 <- lm(log(violent_crime_index + .1) ~ 
             log(family_in_pov + .1) +
             (1 - pct_white) +
             pct_hisp +
             pct_young_male +
             log(median_hh_inc +.1) +
             avg_litter +
             log(code_violation_index + .1) +
             log(aband_vehicl_index + .1) +
             log(dumping_index + .1) +
             log(light_out_index + .1) +
             log(graffiti_index + .1) +
             log(infestation_index + .1) +
             log(bad_street_index + .1) +
             pct_civic +
             pct_culture +
             pct_comercial +
             pct_industrial +
             pct_residential +
             pct_transport + 
             pct_vacant +
             log(distance_cafe) +
             log(distance_conv) +
             log(distance_gym) +
             log(distance_institution) +
             log(distance_liquor) +
             log(distance_lodge) +
             log(distance_nightlife) +
             log(distance_restaurant) +
             log(distance_retail) +
             near_school,
           data = dat_final)

reg9 <- lm(log(drug_crime_index + .1) ~ 
             log(family_in_pov + .1) +
             (1 - pct_white) +
             pct_hisp +
             pct_young_male +
             log(median_hh_inc +.1) +
             avg_litter +
             log(code_violation_index + .1) +
             log(aband_vehicl_index + .1) +
             log(dumping_index + .1) +
             log(light_out_index + .1) +
             log(graffiti_index + .1) +
             log(infestation_index + .1) +
             log(bad_street_index + .1) +
             pct_civic +
             pct_culture +
             pct_comercial +
             pct_industrial +
             pct_residential +
             pct_transport + 
             pct_vacant +
             log(distance_cafe) +
             log(distance_conv) +
             log(distance_gym) +
             log(distance_institution) +
             log(distance_liquor) +
             log(distance_lodge) +
             log(distance_nightlife) +
             log(distance_restaurant) +
             log(distance_retail) +
             near_school,
           data = dat_final)

reg13 <- lm(log(property_crime_index + .1) ~ 
              log(family_in_pov + .1) +
              (1 - pct_white) +
              pct_hisp +
              pct_young_male +
              log(median_hh_inc +.1) +
              avg_litter +
              log(code_violation_index + .1) +
              log(aband_vehicl_index + .1) +
              log(dumping_index + .1) +
              log(light_out_index + .1) +
              log(graffiti_index + .1) +
              log(infestation_index + .1) +
              log(bad_street_index + .1) +
              pct_civic +
              pct_culture +
              pct_comercial +
              pct_industrial +
              pct_residential +
              pct_transport + 
              pct_vacant +
              log(distance_cafe) +
              log(distance_conv) +
              log(distance_gym) +
              log(distance_institution) +
              log(distance_liquor) +
              log(distance_lodge) +
              log(distance_nightlife) +
              log(distance_restaurant) +
              log(distance_retail) +
              near_school,
            data = dat_final)

#### plots ####

plot_residuals(reg1, "Residuals Regression all crimes near school in target dataset")
plot_residuals(reg5, "Residuals Regression violent crimes near school in target dataset")
plot_residuals(reg9, "Residuals Regression drug crimes near school in target dataset")
plot_residuals(reg13, "Residuals Regression property crimes near school in target dataset")

summary(log(dat_final$crime_index))

