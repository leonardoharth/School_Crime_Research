library(tidyverse)

load("~/UPENN/Box Sync/WISE FELLOWSHIP FALL/FINAL_R_CODE/Environments/PSM/allcrime_target.RData")
load("~/UPENN/Box Sync/WISE FELLOWSHIP FALL/FINAL_R_CODE/Environments/PSM/violentcrime_target.RData")
load("~/UPENN/Box Sync/WISE FELLOWSHIP FALL/FINAL_R_CODE/Environments/PSM/drugcrime_target.RData")
load("~/UPENN/Box Sync/WISE FELLOWSHIP FALL/FINAL_R_CODE/Environments/PSM/propertycrime_target.RData")
load("~/UPENN/Box Sync/WISE FELLOWSHIP FALL/FINAL_R_CODE/Environments/PSM/allcrime_mirror.RData")
load("~/UPENN/Box Sync/WISE FELLOWSHIP FALL/FINAL_R_CODE/Environments/PSM/violentcrime_mirror.RData")
load("~/UPENN/Box Sync/WISE FELLOWSHIP FALL/FINAL_R_CODE/Environments/PSM/drugcrime_mirror.RData")
load("~/UPENN/Box Sync/WISE FELLOWSHIP FALL/FINAL_R_CODE/Environments/PSM/propertycrime_mirror.RData")

setwd("~/UPENN/Box Sync/WISE FELLOWSHIP FALL/FINAL_R_CODE/Environments")

dat <- read.csv("all_regs_for_plot.csv") %>% 
  dplyr::select(-value_coef, -sd_school_effect)

dat$school_effect <- case_when(
  dat$school_effect == "All Schools" ~ "1 - All",
  dat$school_effect == "High Schools" ~ "2 - High",
  dat$school_effect == "Mid Schools" ~ "3 - Mid",
  dat$school_effect == "Elementary Schools" ~ "4 - Elem.",
  TRUE ~ "Unknown"
)

dat$ttest <- 0
dat$ttest <- as.double(dat$ttest)

dat$ttest[1] <- ttest_allcrime_nearschool_target$estimate
dat$ttest[2] <- ttest_allcrime_nearhighschool_target$estimate
dat$ttest[3] <- ttest_allcrime_nearmiddleschool_target$estimate
dat$ttest[4] <- ttest_allcrime_nearelementaryschool_target$estimate
dat$ttest[5] <- ttest_violentcrime_nearschool_target$estimate
dat$ttest[6] <- ttest_violentcrime_nearhighschool_target$estimate
dat$ttest[7] <- ttest_violentcrime_nearmiddleschool_target$estimate
dat$ttest[8] <- ttest_violentcrime_nearelementaryschool_target$estimate
dat$ttest[9] <- ttest_drugcrime_nearschool_target$estimate
dat$ttest[10] <- ttest_drugcrime_nearhighschool_target$estimate
dat$ttest[11] <- ttest_drugcrime_nearmiddleschool_target$estimate
dat$ttest[12] <- ttest_drugcrime_nearelementaryschool_target$estimate
dat$ttest[13] <- ttest_propertycrime_nearschool_target$estimate
dat$ttest[14] <- ttest_propertycrime_nearhighschool_target$estimate
dat$ttest[15] <- ttest_propertycrime_nearmiddleschool_target$estimate
dat$ttest[16] <- ttest_propertycrime_nearelementaryschool_target$estimate
dat$ttest[17] <- ttest_allcrime_nearschool_mirror$estimate
dat$ttest[18] <- ttest_allcrime_nearhighschool_mirror$estimate
dat$ttest[19] <- ttest_allcrime_nearmiddleschool_mirror$estimate
dat$ttest[20] <- ttest_allcrime_nearelementaryschool_mirror$estimate
dat$ttest[21] <- ttest_violentcrime_nearschool_mirror$estimate
dat$ttest[22] <- ttest_violentcrime_nearhighschool_mirror$estimate
dat$ttest[23] <- ttest_violentcrime_nearmiddleschool_mirror$estimate
dat$ttest[24] <- ttest_violentcrime_nearelementaryschool_mirror$estimate
dat$ttest[25] <- ttest_drugcrime_nearschool_mirror$estimate
dat$ttest[26] <- ttest_drugcrime_nearhighschool_mirror$estimate
dat$ttest[27] <- ttest_drugcrime_nearmiddleschool_mirror$estimate
dat$ttest[28] <- ttest_drugcrime_nearelementaryschool_mirror$estimate
dat$ttest[29] <- ttest_propertycrime_nearschool_mirror$estimate
dat$ttest[30] <- ttest_propertycrime_nearhighschool_mirror$estimate
dat$ttest[31] <- ttest_propertycrime_nearmiddleschool_mirror$estimate
dat$ttest[32] <- ttest_propertycrime_nearelementaryschool_mirror$estimate

dat$stderror <- 0
dat$stderror <- as.double(dat$stderror)

dat$stderror[1] <- ttest_allcrime_nearschool_target$stderr
dat$stderror[2] <- ttest_allcrime_nearhighschool_target$stderr
dat$stderror[3] <- ttest_allcrime_nearmiddleschool_target$stderr
dat$stderror[4] <- ttest_allcrime_nearelementaryschool_target$stderr
dat$stderror[5] <- ttest_violentcrime_nearschool_target$stderr
dat$stderror[6] <- ttest_violentcrime_nearhighschool_target$stderr
dat$stderror[7] <- ttest_violentcrime_nearmiddleschool_target$stderr
dat$stderror[8] <- ttest_violentcrime_nearelementaryschool_target$stderr
dat$stderror[9] <- ttest_drugcrime_nearschool_target$stderr
dat$stderror[10] <- ttest_drugcrime_nearhighschool_target$stderr
dat$stderror[11] <- ttest_drugcrime_nearmiddleschool_target$stderr
dat$stderror[12] <- ttest_drugcrime_nearelementaryschool_target$stderr
dat$stderror[13] <- ttest_propertycrime_nearschool_target$stderr
dat$stderror[14] <- ttest_propertycrime_nearhighschool_target$stderr
dat$stderror[15] <- ttest_propertycrime_nearmiddleschool_target$stderr
dat$stderror[16] <- ttest_propertycrime_nearelementaryschool_target$stderr
dat$stderror[17] <- ttest_allcrime_nearschool_mirror$stderr
dat$stderror[18] <- ttest_allcrime_nearhighschool_mirror$stderr
dat$stderror[19] <- ttest_allcrime_nearmiddleschool_mirror$stderr
dat$stderror[20] <- ttest_allcrime_nearelementaryschool_mirror$stderr
dat$stderror[21] <- ttest_violentcrime_nearschool_mirror$stderr
dat$stderror[22] <- ttest_violentcrime_nearhighschool_mirror$stderr
dat$stderror[23] <- ttest_violentcrime_nearmiddleschool_mirror$stderr
dat$stderror[24] <- ttest_violentcrime_nearelementaryschool_mirror$stderr
dat$stderror[25] <- ttest_drugcrime_nearschool_mirror$stderr
dat$stderror[26] <- ttest_drugcrime_nearhighschool_mirror$stderr
dat$stderror[27] <- ttest_drugcrime_nearmiddleschool_mirror$stderr
dat$stderror[28] <- ttest_drugcrime_nearelementaryschool_mirror$stderr
dat$stderror[29] <- ttest_propertycrime_nearschool_mirror$stderr
dat$stderror[30] <- ttest_propertycrime_nearhighschool_mirror$stderr
dat$stderror[31] <- ttest_propertycrime_nearmiddleschool_mirror$stderr
dat$stderror[32] <- ttest_propertycrime_nearelementaryschool_mirror$stderr

dat <- dat %>% 
  mutate(
    linestart = dat$ttest - 2*(dat$stderror),
    lineend = dat$ttest + 2*(dat$stderror)
  )

ggplot(dat) +
  geom_point(aes(x = school_effect, y = ttest, color = dataset), size = 5) +
  geom_segment(aes(x = school_effect, xend = school_effect, y = linestart, yend = lineend, color = dataset),
               size = 2, alpha = .5) +
  facet_wrap(~crimetype, ncol = 4) +
  xlab("") + ylab("") +
  labs(title = "Estimate difference in T-tests for matched pairs +- 2 Std. Errors", color = "Dataset") +
  theme(axis.text.x = element_text(angle = 90))

