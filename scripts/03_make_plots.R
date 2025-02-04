library(tidyverse)
library(readxl)

#old stuff based on JMP output. TO DO: Need to update using variables in R env from analysis pipeline.
#quick plot for tnc report - using a lsmeans table outputted from jmp 
treatment_updn <- read_excel("data/processed data/jmp_lsmeans_treatment_updown.xlsx") %>%
  rename("least_sq_mean" = "Least Sq Mean",
         "up_dn" = "Upstream/Downstream",
         "std_error" = "Std Error")
kelp <- read_csv("data/processed data/processed_kelp_v2.csv") %>%
  rename("distance" = "Distance.from.pyc")

#overall grazing (not separated by up/down) - double check least sq mean u are using vs. raw data or something else
ggplot(data = treatment_updn, aes(x = Treatment, y = least_sq_mean) ) +
  geom_boxplot() +
  theme_classic() +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Upstream/Downstream")

#grazing by treatment and region
ggplot(data = treatment_updn, aes(x = Treatment, y = least_sq_mean, 
                                  fill = up_dn) ) +
  geom_bar(stat='identity', position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = least_sq_mean - std_error, ymax = least_sq_mean + std_error),
                width=.2, position=position_dodge(.9)) +
  theme_classic() +
  scale_fill_manual(values = c('#DAF7A6','#5A5A5A') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Upstream/Downstream")
  
#grazing by treatment and distance
kelp_down <- kelp %>%
  filter(Upstream.Downstream == "D")
ggplot(data = kelp_down, aes(x = distance, y = Kelp_grazed_pcntchange, color=Treatment)) +
  geom_point() +
  geom_smooth(method = lm, aes(group=Treatment), se=TRUE) +
  theme_classic() +
  scale_color_manual(values = c("#f15b12", "#90a8c9", "#662267")) +
  ylab("% Change Kelp Grazed") +
  xlab("Distance from cage (# clines)")
