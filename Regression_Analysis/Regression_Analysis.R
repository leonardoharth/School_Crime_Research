#### Initial Settings ####
library(tidyverse)
library(stargazer)
library(MASS)

load("~/UPENN/Box Sync/WISE FELLOWSHIP FALL/FINAL_R_CODE/Environments/Environment_Target_Dataset.RData")
load("~/UPENN/Box Sync/WISE FELLOWSHIP FALL/FINAL_R_CODE/Environments/Environment_Mirror_Dataset.RData")

setwd("~/UPENN/Box Sync/WISE FELLOWSHIP FALL/FINAL_R_CODE/Regression_Analysis")

remove(nn_function, plotTheme, mapTheme)

plot_coefficient <- function(regression, title_plt, subtitle_plt) {
  all_coeffs_reg <- coefficients(regression)
  all_labels_reg <- names(regression$coefficients)
  df_reg1_all <- data.frame(all_coeffs_reg, all_labels_reg)
  df_reg1_all <- df_reg1_all[order(df_reg1_all$all_coeffs_reg),]
  df_reg1_all <- filter(df_reg1_all, df_reg1_all$all_labels_reg != "(Intercept)")
  
  ggplot(df_reg1_all) +
    geom_bar(aes(x = reorder(all_labels_reg, all_coeffs_reg), y = all_coeffs_reg, fill = all_coeffs_reg > 0), stat = 'identity',
             show.legend = FALSE) +
    coord_flip() +
    labs(title = title_plt, subtitle = subtitle_plt, fill = "") +
    xlab("") +
    ylab("")
}
plot_fitted_vs_observed <- function(regression, dataframe, dependent_var) {
  fitted_reg <- fitted(regression)
  labels_fitted_reg <- names(regression$fitted)
  map_fitted <- data.frame(fitted_reg, labels_fitted_reg)
  dataframe$labels_fitted_reg <- row.names(dataframe)
  dat_crime_fitted <- left_join(dataframe, map_fitted, by = "labels_fitted_reg")
  ggplot()+
    geom_point(data = dat_crime_fitted, aes(x = log(crime_index), y = dat_crime_fitted$fitted_reg)) +
    stat_smooth(data = dat_crime_fitted, aes(log(crime_index), log(crime_index), colour="Perfect Prediction"), method = "lm", se = FALSE, size = 1) +
    stat_smooth(data = dat_crime_fitted, aes(log(crime_index), fitted_reg, colour="Model Prediction"), method = "lm", se = FALSE, size = 1) + 
    labs(title = "Fitted values x Measured Values", 
         colour = "") +
    xlab(dependent_var) +
    ylab("Fitted Values")
}

#### INITIAL REGRESSION EXPLORATION ####

reg_full <- lm(log(crime_index + .1) ~ 
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

summary(reg_full)

plot_coefficient(reg_full, "Target Dataset - Dependent Variable = All Crimes", "Effect = All Schools")
plot_fitted_vs_observed(reg_full, dat_crime, "Log of crime index")

stepWISE <- stepAIC(reg_full, trace = "FALSE", direction = "backward")
stepWISE$anova

# the final model removes 02 variables: (1 - pct_white) and log(median_hh_inc +.1)

final_model_backward_stepwise <- lm(log(crime_index + 0.1) ~ log(family_in_pov + 0.1) + 
                                      pct_hisp + 
                                      pct_young_male + 
                                      avg_litter + 
                                      log(code_violation_index + 0.1) + 
                                      log(aband_vehicl_index + 0.1) + 
                                      log(dumping_index + 0.1) +
                                      log(light_out_index + 0.1) + 
                                      log(graffiti_index + 0.1) + 
                                      log(infestation_index + 0.1) + 
                                      log(bad_street_index + 0.1) + 
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

summary(final_model_backward_stepwise)

# The R-Squared is not affected by removing variables
# I will continue the analysis without removing variables

plot_coefficient(final_model_backward_stepwise, "Target Dataset - Dependent Variable = All Crimes", "Effect = All Schools")
plot_fitted_vs_observed(final_model_backward_stepwise, dat_crime, "Log of crime index")

#### REGRESSIONS TARGET - ALL CRIMES ####

# clean up environment:
remove(reg_full, final_model_backward_stepwise, stepWISE)

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

reg2 <- lm(log(crime_index + .1) ~ 
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
             near_highschool,
           data = dat_final)

reg3 <- lm(log(crime_index + .1) ~ 
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
             near_middleschool,
           data = dat_final)

reg4 <- lm(log(crime_index + .1) ~ 
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
             near_elementaryschool,
           data = dat_final)

stargazer(reg1, reg2, reg3, reg4, type = "text", out = "Target_allcrimes.html")

#### REGRESSIONS TARGET - VIOLENT CRIMES ####

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

reg6 <- lm(log(violent_crime_index + .1) ~ 
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
             near_highschool,
           data = dat_final)

reg7 <- lm(log(violent_crime_index + .1) ~ 
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
             near_middleschool,
           data = dat_final)

reg8 <- lm(log(violent_crime_index + .1) ~ 
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
             near_elementaryschool,
           data = dat_final)

stargazer(reg5, reg6, reg7, reg8, type = "text", out = "Target_violentcrimes.html")

#### REGRESSIONS TARGET - DRUG CRIMES ####

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

reg10 <- lm(log(drug_crime_index + .1) ~ 
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
             near_highschool,
           data = dat_final)

reg11 <- lm(log(drug_crime_index + .1) ~ 
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
             near_middleschool,
           data = dat_final)

reg12 <- lm(log(drug_crime_index + .1) ~ 
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
             near_elementaryschool,
           data = dat_final)

stargazer(reg9, reg10, reg11, reg12, type = "text", out = "Target_drugcrimes.html")

#### REGRESSIONS TARGET - DRUG CRIMES ####

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

reg14 <- lm(log(property_crime_index + .1) ~ 
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
              near_highschool,
            data = dat_final)

reg15 <- lm(log(property_crime_index + .1) ~ 
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
              near_middleschool,
            data = dat_final)

reg16 <- lm(log(property_crime_index + .1) ~ 
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
              near_elementaryschool,
            data = dat_final)

stargazer(reg13, reg14, reg15, reg16, type = "text", out = "Target_propertycrimes.html")

#### REGRESSIONS MIRROR - ALL CRIMES ####

reg17 <- lm(log(crime_index + .1) ~ 
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
           data = dat_final_MIRROR)

reg18 <- lm(log(crime_index + .1) ~ 
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
             near_highschool,
           data = dat_final_MIRROR)

reg19 <- lm(log(crime_index + .1) ~ 
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
             near_middleschool,
           data = dat_final_MIRROR)

reg20 <- lm(log(crime_index + .1) ~ 
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
             near_elementaryschool,
           data = dat_final_MIRROR)

stargazer(reg17, reg18, reg19, reg20, type = "text", out = "Mirror_allcrimes.html")

#### REGRESSIONS MIRROR - VIOLENT CRIMES ####

reg21 <- lm(log(violent_crime_index + .1) ~ 
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
            data = dat_final_MIRROR)

reg22 <- lm(log(violent_crime_index + .1) ~ 
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
              near_highschool,
            data = dat_final_MIRROR)

reg23 <- lm(log(violent_crime_index + .1) ~ 
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
              near_middleschool,
            data = dat_final_MIRROR)

reg24 <- lm(log(violent_crime_index + .1) ~ 
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
              near_elementaryschool,
            data = dat_final_MIRROR)

stargazer(reg21, reg22, reg23, reg24, type = "text", out = "Mirror_violentcrimes.html")

#### REGRESSIONS MIRROR - DRUG CRIMES ####

reg25 <- lm(log(drug_crime_index + .1) ~ 
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
            data = dat_final_MIRROR)

reg26 <- lm(log(drug_crime_index + .1) ~ 
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
              near_highschool,
            data = dat_final_MIRROR)

reg27 <- lm(log(drug_crime_index + .1) ~ 
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
              near_middleschool,
            data = dat_final_MIRROR)

reg28 <- lm(log(drug_crime_index + .1) ~ 
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
              near_elementaryschool,
            data = dat_final_MIRROR)

stargazer(reg25, reg26, reg27, reg28, type = "text", out = "Mirror_drugcrimes.html")

#### REGRESSIONS MIRROR - PROPERTY CRIMES ####

reg29 <- lm(log(property_crime_index + .1) ~ 
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
            data = dat_final_MIRROR)

reg30 <- lm(log(property_crime_index + .1) ~ 
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
              near_highschool,
            data = dat_final_MIRROR)

reg31 <- lm(log(property_crime_index + .1) ~ 
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
              near_middleschool,
            data = dat_final_MIRROR)

reg32 <- lm(log(property_crime_index + .1) ~ 
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
              near_elementaryschool,
            data = dat_final_MIRROR)

stargazer(reg29, reg30, reg31, reg32, type = "text", out = "Mirror_propertycrimes.html")

#### CREATE SUMMARY OF REGS DF ####

create_line <- function (regression, dataset_input, crimetype_input, school_effect_title_input) {
  summary_reg <- summary(regression)
  values_reg <- as.matrix(summary_reg$coefficients)
  value_coef <- values_reg[30,1]
  sd_school_effect <- values_reg[30,2]
  dataset <- dataset_input
  crimetype <- crimetype_input
  school_effect <- school_effect_title_input
  return (cbind(dataset,school_effect, crimetype, value_coef, sd_school_effect))
}

line1 <- create_line(reg1, "Target", "All Crimes", "All Schools")
line2 <- create_line(reg2, "Target", "All Crimes", "High Schools")
line3 <- create_line(reg3, "Target", "All Crimes", "Mid Schools")
line4 <- create_line(reg4, "Target", "All Crimes", "Elementary Schools")
line5 <- create_line(reg5, "Target", "Violent Crimes", "All Schools")
line6 <- create_line(reg6, "Target", "Violent Crimes", "High Schools")
line7 <- create_line(reg7, "Target", "Violent Crimes", "Mid Schools")
line8 <- create_line(reg8, "Target", "Violent Crimes", "Elementary Schools")
line9 <- create_line(reg9, "Target", "Drug Crimes", "All Schools")
line10 <- create_line(reg10, "Target", "Drug Crimes", "High Schools")
line11 <- create_line(reg11, "Target", "Drug Crimes", "Mid Schools")
line12 <- create_line(reg12, "Target", "Drug Crimes", "Elementary Schools")
line13 <- create_line(reg13, "Target", "Property Crimes", "All Schools")
line14 <- create_line(reg14, "Target", "Property Crimes", "High Schools")
line15 <- create_line(reg15, "Target", "Property Crimes", "Mid Schools")
line16 <- create_line(reg16, "Target", "Property Crimes", "Elementary Schools")
line17 <- create_line(reg17, "Mirror", "All Crimes", "All Schools")
line18 <- create_line(reg18, "Mirror", "All Crimes", "High Schools")
line19 <- create_line(reg19, "Mirror", "All Crimes", "Mid Schools")
line20 <- create_line(reg20, "Mirror", "All Crimes", "Elementary Schools")
line21 <- create_line(reg21, "Mirror", "Violent Crimes", "All Schools")
line22 <- create_line(reg22, "Mirror", "Violent Crimes", "High Schools")
line23 <- create_line(reg23, "Mirror", "Violent Crimes", "Mid Schools")
line24 <- create_line(reg24, "Mirror", "Violent Crimes", "Elementary Schools")
line25 <- create_line(reg25, "Mirror", "Drug Crimes", "All Schools")
line26 <- create_line(reg26, "Mirror", "Drug Crimes", "High Schools")
line27 <- create_line(reg27, "Mirror", "Drug Crimes", "Mid Schools")
line28 <- create_line(reg28, "Mirror", "Drug Crimes", "Elementary Schools")
line29 <- create_line(reg29, "Mirror", "Property Crimes", "All Schools")
line30 <- create_line(reg30, "Mirror", "Property Crimes", "High Schools")
line31 <- create_line(reg31, "Mirror", "Property Crimes", "Mid Schools")
line32 <- create_line(reg32, "Mirror", "Property Crimes", "Elementary Schools")

all_regs_df <- rbind(line1, line2, line3, line4, line5, line6, line7, line8, line9, line10, line11, line12,
                     line13, line14, line15, line16, line17, line18, line19, line20, line21, line22, line23,
                     line24, line25, line26, line27, line28, line29, line30, line31, line32)

all_regs_df <- as.data.frame(all_regs_df)
row.names(all_regs_df) <- NULL

write.csv(all_regs_df,"~/UPENN/Box Sync/WISE FELLOWSHIP FALL/FINAL_R_CODE/Environments/all_regs_for_plot.csv", row.names = FALSE)

remove(line1, line2, line3, line4, line5, line6, line7, line8, line9, line10, line11, line12,
       line13, line14, line15, line16, line17, line18, line19, line20, line21, line22, line23,
       line24, line25, line26, line27, line28, line29, line30, line31, line32)
remove(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, reg9, reg10, reg11, reg12, reg13, reg14, reg15, reg16, reg17,
       reg18, reg19, reg20, reg21, reg22, reg23, reg24, reg25, reg26, reg27, reg28, reg29, reg30, reg31, reg32)
remove(dat_crime, dat_crime_MIRROR, dat_final, dat_final_MIRROR, create_line, plot_coefficient, plot_fitted_vs_observed)
