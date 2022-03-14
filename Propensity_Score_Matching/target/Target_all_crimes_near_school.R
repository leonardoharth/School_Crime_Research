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

target_near_school_match <- matchit(near_school ~ 
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

#### Create visualization of difference between matched and unmatched data ####

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

target_near_school_match_DF <- match.data(target_near_school_match)[1:ncol(dat_final)]

matched_treated <- filter(target_near_school_match_DF, near_school == 1) %>% 
  select(log_family_pov, pct_nonwhite, pct_hisp, pct_young_male, log_median_HH_income, avg_litter, log_code_violation,
         log_abandoned_vehic, log_dumping, log_lights_out, log_graffiti, log_infestation, log_bad_street,
         pct_civic, pct_culture, pct_comercial, pct_industrial, pct_residential, pct_transport, pct_vacant,
         log_dist_cafe, log_dist_conv, log_dist_gym, log_dist_inst, log_dist_liq, log_dist_lodge,
         log_dist_nightlife, log_dist_rest, log_dist_retail)
matched_control <- filter(target_near_school_match_DF, near_school == 0) %>% 
  select(log_family_pov, pct_nonwhite, pct_hisp, pct_young_male, log_median_HH_income, avg_litter, log_code_violation,
         log_abandoned_vehic, log_dumping, log_lights_out, log_graffiti, log_infestation, log_bad_street,
         pct_civic, pct_culture, pct_comercial, pct_industrial, pct_residential, pct_transport, pct_vacant,
         log_dist_cafe, log_dist_conv, log_dist_gym, log_dist_inst, log_dist_liq, log_dist_lodge,
         log_dist_nightlife, log_dist_rest, log_dist_retail)
unmatched_treated <- filter(dat_final, near_school == 1) %>% 
  select(log_family_pov, pct_nonwhite, pct_hisp, pct_young_male, log_median_HH_income, avg_litter, log_code_violation,
         log_abandoned_vehic, log_dumping, log_lights_out, log_graffiti, log_infestation, log_bad_street,
         pct_civic, pct_culture, pct_comercial, pct_industrial, pct_residential, pct_transport, pct_vacant,
         log_dist_cafe, log_dist_conv, log_dist_gym, log_dist_inst, log_dist_liq, log_dist_lodge,
         log_dist_nightlife, log_dist_rest, log_dist_retail)
unmatched_control <- filter(dat_final, near_school == 0) %>% 
  select(log_family_pov, pct_nonwhite, pct_hisp, pct_young_male, log_median_HH_income, avg_litter, log_code_violation,
         log_abandoned_vehic, log_dumping, log_lights_out, log_graffiti, log_infestation, log_bad_street,
         pct_civic, pct_culture, pct_comercial, pct_industrial, pct_residential, pct_transport, pct_vacant,
         log_dist_cafe, log_dist_conv, log_dist_gym, log_dist_inst, log_dist_liq, log_dist_lodge,
         log_dist_nightlife, log_dist_rest, log_dist_retail)

summary_match_df <- as.data.frame(colMeans(matched_treated))

mean_matched_control <- as.data.frame(colMeans(matched_control))
summary_match_df <- cbind(summary_match_df, mean_matched_control)

mean_unmatched_treated <- as.data.frame(colMeans(unmatched_treated))
summary_match_df <- cbind(summary_match_df, mean_unmatched_treated)

mean_unmatched_control <- as.data.frame(colMeans(unmatched_control))
summary_match_df <- cbind(summary_match_df, mean_unmatched_control)

match_df_for_plots <- as.data.frame(summary_match_df) %>% 
  mutate(variable = row.names(summary_match_df)) %>% 
  dplyr::rename(mean_matched_treated = `colMeans(matched_treated)`,
         mean_matched_control = `colMeans(matched_control)`,
         mean_unmatched_treated = `colMeans(unmatched_treated)`,
         mean_unmatched_control = `colMeans(unmatched_control)`)

remove(summary_match_df, mean_matched_control, mean_unmatched_control, mean_unmatched_treated, matched_control, matched_treated,
       unmatched_control, unmatched_treated)

match_df_for_plots$difference_matched <- match_df_for_plots$mean_matched_treated - match_df_for_plots$mean_matched_control
match_df_for_plots$difference_unmatched <- match_df_for_plots$mean_unmatched_treated - match_df_for_plots$mean_unmatched_control
match_df_for_plots <- match_df_for_plots %>% 
  select(variable, difference_matched, difference_unmatched)

ggplot(match_df_for_plots) +
  geom_point(aes(x = variable, y = difference_matched), color = '#e66101', size = 2) +
  geom_point(aes(x = variable, y = difference_unmatched), color = '#5e3c99', size = 2) +
  coord_flip() +
  labs(title = "Difference in averages for regression variables in matched and unmatched sets",
       subtitle = "Matched in orange, unmatched in blue") +
  xlab("") +
  ylab("Average for blocks near schools - Average for blocks not near schools")

#### TEST PAIRED MATCHES ####

# Create a DF with paired matches
Paired_crime_index <- cbind(dat_final[row.names(target_near_school_match$match.matrix),"log_crime_index"], dat_final[target_near_school_match$match.matrix,"log_crime_index"])
names(Paired_crime_index)[1] <- "Near_School"
names(Paired_crime_index)[2] <- "Not_Near_School"

Paired_crime_index$Difference <- Paired_crime_index$Near_School - Paired_crime_index$Not_Near_School

ggplot(Paired_crime_index) +
  geom_histogram(aes(Difference), color = NA, fill = "#fec44f", bins = 200, alpha = .5) +
  geom_density(aes(x = Difference), alpha = .2, fill = "blue") +
  geom_vline(aes(xintercept = mean(Difference)),
             color="red", linetype="dashed", size=1) +
  labs(title = "Distribution of differences in pairs", subtitle = "Mean difference in red") + 
  xlab("") + ylab("")

# Run a T-test
ttest_allcrime_nearschool_target <- t.test(Paired_crime_index$Near_School, Paired_crime_index$Not_Near_School, paired = TRUE)
ttest_allcrime_nearschool$estimate

# Create visualization of crime index variation
Paired_crime_index_melt <- melt(Paired_crime_index)
Paired_crime_index_melt <- Paired_crime_index_melt %>% 
  dplyr::rename(treated = variable, crime_index = value)

ggplot(Paired_crime_index_melt) +
  geom_density(aes(x = crime_index, fill = as.factor(treated)), alpha = .5) +
  scale_fill_manual(values = c("#edf8b1", "#7fcdbb", "#2c7fb8")) +
  facet_wrap(Paired_crime_index_melt$treated) + 
  labs(title = "Log of Crime Index Density for matched pairs") +
  xlab("Log of Crime per Area")+ylab("") +
  theme(legend.position="none")

summary(Paired_crime_index$Near_School)
summary(Paired_crime_index$Not_Near_School)

remove(dat_final, dat_final_MIRROR, match_df_for_plots, Paired_crime_index, Paired_crime_index_melt,
       target_near_school_match, target_near_school_match_DF)