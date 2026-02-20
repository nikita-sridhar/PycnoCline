#figures for caged vs. control look identical. compare raw data (completely raw + versus after cleanup)

kelp_intermediary1 <- kelp_raw %>%
  mutate(weight_pcnt_change = ((Kelp_weight_before_g - Kelp_weight_after_g)/
                                 Kelp_weight_before_g)*100 )

kelp_intermediary_1.5 <- kelp_raw %>%
  filter(Trial == c(2,"Pilot"))

kelp_intermediary2 <- kelp

#between kelp_intermdiary1 and 2, removed pilot and trial 2, added some pycno columns, 
#^^ there should only be a 20 row difference btwn kelp_intermediary 1 and 2 
#(number of rows of pilot + trial 2)


#histogram
ggplot(kelp_intermediary1, aes(x=weight_pcnt_change))+
  geom_histogram() +
  facet_wrap(vars(Treatment))

ggplot(kelp_intermediary2, aes(x=weight_pcnt_change))+
  geom_histogram() +
  facet_wrap(vars(Treatment))

ggplot(kelp, aes(x = weight_pcnt_change)) +
  geom_histogram() +
  facet_wrap(vars(Treatment))

###########################################
#OLD SCRIPT FOR PLOTS BASED ON JMP OUTPUT
###########################################

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


