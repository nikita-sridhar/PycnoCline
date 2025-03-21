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


################################################################################
#plots based on R scripts
################################################################################

######
#model
######

#active vs.control 

ggplot(mod1b_withsd, 
       aes(x = Treatment, y = predicted, fill = Treatment)) +
  
  geom_bar(stat = "identity", 
           position = position_dodge()) +
  
  geom_errorbar(data = mod1b_withsd,
                aes(ymin = raw_mean-raw_sd, ymax =raw_mean+raw_sd), 
                position=position_dodge()) +
  
  labs(title = "Predicted values of % kelp grazed from GLMM", 
       x = "Predator treatment", y = "% kelp grazed", 
       fill = "Predator treatment",
       caption = "Note: error bars are made with respect to SD and mean of raw data") + #maybe this isn't the best  - it shows variability in the raw data, but idk
  
  theme_classic() +
  scale_fill_manual(values = c('#DAF7A6','#5A5A5A') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Predator treatment")

#caged and control upstream vs. downstream

ggplot(mod1a.1_withsd, 
       aes(x = Treatment, y = predicted, fill = up_down)) +
  
  geom_bar(stat = "identity", 
           position = position_dodge()) +
  
  geom_errorbar(data = mod1a.1_withsd,
                aes(ymin = raw_mean-raw_sd, ymax =raw_mean+raw_sd), 
                position=position_dodge()) +
  
  labs(title = "Predicted values of % kelp grazed from GLMM", 
       x = "Predator treatment", y = "% kelp grazed", 
       fill = "Upstream/downstream of cage",
       caption = "Note: error bars are made with respect to SD and mean of raw data") + #maybe this isn't the best  - it shows variability in the raw data, but idk
  
  theme_classic() +
  scale_fill_manual(values = c('#DAF7A6','#5A5A5A') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Predator treatment")

#using model with combined treatments (which earlier we said we shouldn't do but its what made the initial interesting graph)
#caged and control

ggplot(mod1d_withsd, 
       aes(x = Treatment, y = predicted, fill = up_down)) +
  
  geom_bar(stat = "identity", 
           position = position_dodge()) +
  
  #geom_errorbar(aes(ymin = raw_mean-raw_sd, ymax =raw_mean+raw_sd), 
                #position=position_dodge()) +
  
  labs(title = "Predicted values of % kelp grazed from GLMM", 
       x = "Predator treatment", y = "% kelp grazed", 
       fill = "Upstream/downstream of cage",
       caption = "Note: error bars are made with respect to SD and mean of raw data") + #maybe this isn't the best  - it shows variability in the raw data, but idk
  
  theme_classic() +
  scale_fill_manual(values = c('#DAF7A6','#5A5A5A') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Predator treatment")


#########
#raw data
#########

#active vs.control 

ggplot(kelp%>%filter(Treatment == c("Active","Control")), 
       aes(x = Treatment, y = weight_pcnt_change, fill = Treatment)) +
  
  geom_bar(stat = "summary", 
           position = position_dodge()) +
  
  geom_errorbar(stat="summary") +
  
  labs(title = "% kelp grazed from raw data for active and control treatments", 
       x = "Predator treatment", y = "% kelp grazed", 
       fill = "Predator treatment",
       caption = "Error bars show SE") + 
  
  theme_classic() +
  scale_fill_manual(values = c('#DAF7A6','#5A5A5A') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Predator treatment")

#caged and control upstream vs. downstream

ggplot(kelp%>%filter(Treatment == c("Caged","Control")), 
       aes(x = Treatment, y = weight_pcnt_change, fill = up_down)) +
  
  geom_bar(stat = "summary", 
           position = position_dodge()) +
  
  geom_errorbar(stat = "summary", position = position_dodge())+
  
  labs(title = "% kelp grazed from raw data for caged and control treatments", 
       x = "Predator treatment", y = "% kelp grazed", 
       fill = "Upstream/downstream of cage",
       caption = "Error bars show SE") + #maybe this isn't the best  - it shows variability in the raw data, but idk
  
  theme_classic() +
  scale_fill_manual(values = c('#DAF7A6','#5A5A5A') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Predator treatment")


#trying to investigate caged vs. control bc bars look nearly identical
ggplot(kelp%>%filter(Treatment == c("Caged","Control")), 
       aes(x = Treatment, y = weight_pcnt_change, fill = up_down)) +
  
  geom_point( aes(color=up_down)) +
  
  geom_errorbar(stat = "summary", position = position_dodge())+
  
  labs(title = "% kelp grazed from raw data for caged and control treatments", 
       x = "Predator treatment", y = "% kelp grazed", 
       fill = "Upstream/downstream of cage",
       caption = "Error bars show SE") + #maybe this isn't the best  - it shows variability in the raw data, but idk
  
  theme_classic() +
  scale_fill_manual(values = c('#DAF7A6','#5A5A5A') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Predator treatment")



#########
#ls means
#########


