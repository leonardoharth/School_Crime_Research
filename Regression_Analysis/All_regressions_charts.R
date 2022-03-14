library(tidyverse)
library(pdp)
library(grid)
library(gridExtra)

setwd("~/UPENN/Box Sync/WISE FELLOWSHIP FALL/FINAL_R_CODE/Environments")

dat <- read.csv("all_regs_for_plot.csv")

dat$line_start <- dat$value_coef - 2*(dat$sd_school_effect)
dat$line_end <- dat$value_coef + 2*(dat$sd_school_effect)

dat$school_effect <- case_when(
  dat$school_effect == "All Schools" ~ "1. All",
  dat$school_effect == "High Schools" ~ "2. High",
  dat$school_effect == "Mid Schools" ~ "3. Mid",
  dat$school_effect == "Elementary Schools" ~ "4 Elem.",
  TRUE ~ "Unknowm"
)

mirror <- filter(dat, dataset == "Mirror")
target <- filter(dat, dataset == "Target")

ggplot(dat) +
  geom_point(aes(x = school_effect, y = value_coef, color = dataset), size = 5) +
  geom_segment(aes(x = school_effect, xend = school_effect, 
                   y = line_start, yend = line_end, color = dataset), size = 2, alpha = .5) +
  facet_wrap(dat$crimetype, nrow = 1) +
  labs(title = "COEFFICIENT OF EFFECT +- 2 STANTARD ERRORS", color = "Dataset") +
  xlab("") + ylab("") +
  theme(axis.text.x = element_text(angle = 90))

target_plot <- ggplot(target) +
  geom_point(aes(x = school_effect, y = value_coef, color = school_effect), size = 5) +
  geom_segment(aes(x = school_effect, xend = school_effect, 
                   y = line_start, yend = line_end, color = school_effect), size = 1.5) +
  scale_color_manual(values = c("#d7191c", "#fdae61", "#2c7bb6", "#abd9e9")) +
  facet_wrap(target$crimetype, nrow = 1) +
  labs(title = "Target Dataset", color = "") +
  xlab("") + ylab("") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

mirror_plot <- ggplot(mirror) +
  geom_point(aes(x = school_effect, y = value_coef, color = school_effect), size = 5) +
  geom_segment(aes(x = school_effect, xend = school_effect, 
                   y = line_start, yend = line_end, color = school_effect), size = 1.5) +
  scale_color_manual(values = c("#d7191c", "#fdae61", "#2c7bb6", "#abd9e9")) +
  facet_wrap(mirror$crimetype, nrow = 1) +
  labs(title = "Mirror Dataset", color = "") +
  xlab("") + ylab("") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

grid.arrange(target_plot, mirror_plot, ncol = 1, 
             top = textGrob("COEFFICIENT OF EFFECT +- 2 STANTARD ERRORS",gp=gpar(fontsize=18, font = 2)))
