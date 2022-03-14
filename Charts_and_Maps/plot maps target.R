library(tidyverse)
library(sf)
library(viridis)

load("~/UPENN/Box Sync/WISE FELLOWSHIP FALL/FINAL_R_CODE/Environments/Environment_Mirror_Dataset.RData")
load("~/UPENN/Box Sync/WISE FELLOWSHIP FALL/FINAL_R_CODE/Environments/Environment_Target_Dataset.RData")

ggplot(dat_final) +
  geom_histogram(aes(crime_index, fill = as.factor(near_school)), bins = 80) +
  labs(title = "Histogram Crime Index - Target Dataset", fill = "Near School") +
  xlab("") + ylab("") + 
  plotTheme()

ggplot(dat_final) +
  geom_histogram(aes(log(crime_index), fill = as.factor(near_school)), bins = 80) +
  labs(title = "Histogram Log of Crime Index - Target Dataset", fill = "Near School") +
  xlab("") + ylab("") + 
  plotTheme()

ggplot(dat_final) + 
  geom_point(aes(x = avg_litter, y = log(crime_index), color = as.factor(near_school))) +
  stat_smooth(method = 'lm', aes(x = dat_final$avg_litter, y = log(dat_final$crime_index))) +
  labs(title = "Correlation Log of Crime Index vs Average Litter Index", color = "Near School") +
  xlab("Census Block's Average Litter Index") + ylab("Log of Crime per area") + 
  plotTheme()

ggplot(dat_final) + 
  geom_point(aes(x = pct_hisp, y = log(crime_index), color = as.factor(near_school))) +
  stat_smooth(method = 'lm', aes(x = dat_final$pct_hisp, y = log(dat_final$crime_index))) +
  labs(title = "Correlation Log of Crime Index vs Percent Hispanic Residents", color = "Near School") +
  xlab("Census Block's Percent Hispanic Residents") + ylab("Log of Crime per area") + 
  plotTheme()

ggplot(dat_final) + 
  geom_point(aes(x = pct_white, y = log(crime_index), color = as.factor(near_school))) +
  stat_smooth(method = 'lm', aes(x = dat_final$pct_white, y = log(dat_final$crime_index))) +
  labs(title = "Correlation Log of Crime Index vs Percent White Residents", color = "Near School") +
  xlab("Census Block's Percent White Residents") + ylab("Log of Crime per area") + 
  plotTheme()

ggplot(dat_final) + 
  geom_point(aes(x = median_hh_inc, y = log(crime_index), color = as.factor(near_school))) +
  stat_smooth(method = 'lm', aes(x = dat_final$median_hh_inc, y = log(dat_final$crime_index))) +
  labs(title = "Correlation Log of Crime Index vs Median Household Income", color = "Near School") +
  xlab("Census Block's Median Household Income") + ylab("Log of Crime per area") + 
  plotTheme()

ggplot(dat_final) + 
  geom_point(aes(x = pct_comercial, y = log(crime_index), color = as.factor(near_school))) +
  stat_smooth(method = 'lm', aes(x = dat_final$pct_comercial, y = log(dat_final$crime_index))) +
  labs(title = "Correlation Log of Crime Index vs Percentage of comercial landuse", color = "Near School") +
  xlab("Census Block's Percentage of comercial landuse") + ylab("Log of Crime per area") + 
  plotTheme()

near_school_crime_indexes <-
  dat_final %>% 
  dplyr::select(near_school, crime_index, violent_crime_index, drug_crime_index,
                property_crime_index) %>% 
  gather(key, value, crime_index:property_crime_index)

ggplot(near_school_crime_indexes, aes(as.factor(near_school), value, fill = as.factor(near_school))) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~key, scales = "free") +
  labs (x = "Near school = 1, not near school = 0", y = "Mean value for all Census Blocks",
        title = "Mean value for crime indexes per school proximity",
        fill = "Near school") +
  plotTheme()

near_school_environmental_vars <-
  dat_final %>% 
  dplyr::select(near_school, code_violation_index, aband_vehicl_index, light_out_index,
                graffiti_index, dumping_index, infestation_index, bad_street_index, avg_litter) %>% 
  gather(key, value, code_violation_index:avg_litter)

ggplot(near_school_environmental_vars, aes(as.factor(near_school), value, fill = as.factor(near_school))) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~key, scales = "free") +
  labs (x = "Near school = 1, not near school = 0", y = "Mean value for all Census Blocks",
        title = "Mean value for environmental features of census blocks per school proximity",
        fill = "Near school") +
  plotTheme()

# distance_vars <-
#   dat_final %>% 
#   dplyr::select(near_school, distance_cafe, distance_conv, distance_gym, distance_restaurant, distance_retail,
#                 distance_institution, distance_liquor, distance_lodge, distance_nightlife, distance_pharmacy) %>% 
#   gather(key, value, distance_cafe:distance_pharmacy)
# 
# ggplot(distance_vars, aes(as.factor(near_school), value, fill = as.factor(near_school))) +
#   geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
#   facet_wrap(~key, scales = "free") +
#   labs (x = "Near school = 1, not near school = 0", y = "Mean value for all Census Blocks",
#         title = "Mean value for distance to business features of census blocks per school proximity",
#         fill = "Near school") +
#   plotTheme()

ggplot()+
  geom_sf(data = dat_crime, aes(fill = distance_cafe), color = NA) +
  scale_fill_viridis() +
  labs(title = "Mean Distance to 5 closest Cafes", fill = "Average distance") +
  mapTheme()

ggplot()+
  geom_sf(data = dat_crime, aes(fill = log(crime_index + .1)), color = NA) +
  scale_fill_viridis() +
  labs(title = "Log of crimes per area", fill = "Log value") +
  mapTheme()

ggplot()+
  geom_sf(data = dat_crime, aes(fill = log(violent_crime_index + .1)), color = NA) +
  scale_fill_viridis() +
  labs(title = "Log of violent crimes per area", fill = "Log value") +
  mapTheme()

ggplot()+
  geom_sf(data = dat_crime, aes(fill = log(drug_crime_index + .1)), color = NA) +
  scale_fill_viridis() +
  labs(title = "Log of drug crimes per area", fill = "Log value") +
  mapTheme()

ggplot()+
  geom_sf(data = dat_crime, aes(fill = log(property_crime_index + .1)), color = NA) +
  scale_fill_viridis() +
  labs(title = "Log of property crimes per area", fill = "Log value") +
  mapTheme()

