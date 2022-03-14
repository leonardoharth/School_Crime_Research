setwd("~/UPENN/Box Sync/WISE FELLOWSHIP FALL/FINAL_R_CODE/Data")
library(tidyverse)
library(lubridate)

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )}

crime_incidence <- read.csv("incidents_part1_part2.csv")

crime_incidence$dispatch_as_date <- ymd(crime_incidence$dispatch_date)
crime_incidence$year_crime <- year(crime_incidence$dispatch_as_date)
crime_incidence$month_crime <- month(crime_incidence$dispatch_as_date)
crime_incidence$day_of_week <- weekdays(crime_incidence$dispatch_as_date)
crime_incidence$is_weekend <- if_else(crime_incidence$day_of_week == "Sunday" | crime_incidence$day_of_week == "Saturday", 1, 0)

crime_incidence$month_crime <- as.integer(crime_incidence$month_crime)
crime_incidence$summer <- if_else(crime_incidence$month_crime > 5 & crime_incidence$month_crime < 9, 1, 0)

crime_incidence$target <- if_else(crime_incidence$is_weekend == 0 & crime_incidence$hour_ > 5 & crime_incidence$hour_ < 21 & crime_incidence$summer == 0, 1, 0)

crime_incidence$target2 <- if_else(crime_incidence$target == 1, "target", "mirror")

crime_incidence <- crime_incidence %>% 
  mutate(crime_type = 
           case_when(
             ucr_general == 400 ~ "violent_crime", # aggravated assaults
             ucr_general == 100 ~ "violent_crime", # homicide
             ucr_general == 800 ~ "violent_crime", # other assaults
             ucr_general == 200 ~ "violent_crime", # rape
             ucr_general == 300 ~ "violent_crime", # robbery
             ucr_general == 1700 ~ "violent_crime", # other sex offenses
             
             ucr_general == 1800 ~ "drug_crime", # narcotics
             
             ucr_general == 900 ~ "crime_against_property", # arson
             ucr_general == 500 ~ "crime_against_property", # burglary 
             ucr_general == 700 ~ "crime_against_property", # Motor Vehicle Theft
             ucr_general == 600 ~ "crime_against_property", # theft
             ucr_general == 1300 ~ "crime_against_property", # Receiving Stolen Property
             
             ucr_general == 2600 ~ "other_crimes", # All Other Offenses
             ucr_general == 2400 ~ "other_crimes", # Disorderly Conduct
             ucr_general == 2100 ~ "other_crimes", # DUI
             ucr_general == 2200 ~ "other_crimes", # Liquor Law Violations
             ucr_general == 2000 ~ "other_crimes", # Offenses Against Family and Children
             ucr_general == 1600 ~ "other_crimes", # Prostitution
             ucr_general == 2300 ~ "other_crimes", # Public Drunkenness
             ucr_general == 2500 ~ "other_crimes", # Vagrancy/Loitering
             ucr_general == 1400 ~ "other_crimes", # Vandalism/Criminal Mischief
             ucr_general == 1500 ~ "other_crimes", # weapon Violations
             
             ucr_general == 1200 ~ "unrelated", # Embezzlement
             ucr_general == 1000 ~ "unrelated", # Forgery
             ucr_general == 1100 ~ "unrelated", # Fraud
             ucr_general == 1900 ~ "unrelated", # Gambling
             
             TRUE ~ "non_categorized"
           )
  )

summ_crime <- crime_incidence %>% 
  group_by(crime_type, target2) %>% 
  mutate(crime_in = 1) %>% 
  summarise(total_count = sum(crime_in))

options(scipen = 999)

summ_crime$crime_type[summ_crime$crime_type == "crime_against_property"] <- "property_crime"

ggplot(summ_crime) +
  geom_bar(aes(x = crime_type, y = total_count, fill = target2), stat = "identity", position = "dodge") +
  labs(title = "Distribution of crime per crime type", fill = "Dataset") +
  xlab("") +
  ylab("") +
  plotTheme()