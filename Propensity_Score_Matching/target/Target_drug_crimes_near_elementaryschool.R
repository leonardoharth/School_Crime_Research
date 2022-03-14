#### INITIAL SETTINGS####
library(tidyverse)
library(MatchIt)
library(reshape2)
library(matrixStats)

load("~/UPENN/Box Sync/WISE FELLOWSHIP FALL/FINAL_R_CODE/Environments/Environment_Target_Dataset.RData")
load("~/UPENN/Box Sync/WISE FELLOWSHIP FALL/FINAL_R_CODE/Environments/Environment_Mirror_Dataset.RData")

remove(dat_crime, dat_crime_MIRROR, mapTheme, plotTheme, nn_function)

#### PMS ####

set.seed(1234)

target_near_school_match <- matchit(near_elementaryschool ~ 
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
             log(distance_retail),
           data = dat_final, method="nearest", ratio=1)

summary(target_near_school_match)

dat_final <- dat_final %>% 
  mutate(
    log_family_pov = log(family_in_pov + 0.1),
    pct_nonwhite = (1 - pct_white),
    log_median_HH_income = log(median_hh_inc +.1),
    log_code_violation = log(code_violation_index + .1),
    log_abandoned_vehic = log(aband_vehicl_index + .1),
    log_dumping = log(dumping_index + .1),
    log_lights_out = log(light_out_index + .1),
    log_graffiti = log(graffiti_index + .1),
    log_infestation = log(infestation_index + .1),
    log_bad_street = log(bad_street_index + .1),
    log_dist_cafe = log(distance_cafe),
    log_dist_conv = log(distance_conv),
    log_dist_gym = log(distance_gym),
    log_dist_inst = log(distance_institution),
    log_dist_liq = log(distance_liquor),
    log_dist_lodge = log(distance_lodge),
    log_dist_nightlife = log(distance_nightlife),
    log_dist_rest = log(distance_restaurant),
    log_dist_retail = log(distance_retail),
    log_crime_index = log(crime_index + 0.1),
    log_violent_crime_index = log(violent_crime_index + 0.1),
    log_drug_crime_index = log(drug_crime_index + 0.1),
    log_property_crime_index = log(property_crime_index + 0.1)
  )

#### TEST PAIRED MATCHES ####

# Create a DF with paired matches
Paired_crime_index <- cbind(dat_final[row.names(target_near_school_match$match.matrix),"log_drug_crime_index"], dat_final[target_near_school_match$match.matrix,"log_drug_crime_index"])
names(Paired_crime_index)[1] <- "Near_elementarySchool"
names(Paired_crime_index)[2] <- "Not_Near_elementarySchool"

# Run a T-test
ttest_drugcrime_nearelementaryschool_target <- t.test(Paired_crime_index$Near_elementarySchool, Paired_crime_index$Not_Near_elementarySchool, paired = TRUE)

remove(dat_final, dat_final_MIRROR, Paired_crime_index, target_near_school_match)
