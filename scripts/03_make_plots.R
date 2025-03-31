library(tidyverse)
library(readxl)


######
#model
######

#active vs.control 

ggplot(predicted_mod_1b, 
       aes(x = Treatment, y = predicted, fill = Treatment)) +
  
  geom_bar(stat = "identity", 
           position = position_dodge()) +
  
  geom_errorbar(aes(ymin = predicted-std.error, ymax =predicted+std.error), 
                position=position_dodge()) +
  
  labs(title = "Predicted values of % kelp grazed from GLMM", 
       x = "Predator treatment", y = "% kelp grazed", 
       fill = "Predator treatment",
       caption = "Note: error bars are made with respect to SE of model") + #maybe this isn't the best  - it shows variability in the raw data, but idk
  
  theme_classic() +
  scale_fill_manual(values = c('#DAF7A6','#5A5A5A') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Predator treatment")

#caged and control upstream vs. downstream

ggplot(mod1a.1_withsd, 
       aes(x = Treatment, y = predicted, fill = up_down)) +
  
  geom_bar(stat = "identity", 
           position = position_dodge()) +
  
  geom_errorbar(aes(ymin = raw_mean-raw_sd, ymax =raw_mean+raw_sd), 
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
  
  geom_bar(stat = "summary", 
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

a <- ggplot(avg_kelp%>%filter(Treatment %in% c("Active","Control")), 
       aes(x = Treatment, y = avg_pcnt_change, fill = Treatment)) +
  
  geom_bar(stat = "identity", 
           position = position_dodge()) +
  
  geom_errorbar(aes(ymin = avg_pcnt_change-se_pcnt_change, 
                    ymax = avg_pcnt_change+se_pcnt_change),
                position = position_dodge()) +
  
  labs(title = "% kelp grazed from raw data for active and control treatments", 
       x = "Predator treatment", y = "% kelp grazed", 
       fill = "Predator treatment",
       caption = "Error bars show 1 SE from mean") + 
  
  theme_classic() +
  scale_fill_manual(values = c('#DAF7A6','#5A5A5A') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Predator treatment")

#caged and control upstream vs. downstream

b <- ggplot(avg_up_dn_kelp%>%filter(Treatment %in% c("Caged","Control")), 
       aes(x = Treatment, y = avg_pcnt_change,fill = up_down)) +
  
  geom_bar(stat = "identity", 
           position = position_dodge()) +
  
  geom_errorbar(aes(ymin = avg_pcnt_change-se_pcnt_change, 
                    ymax = avg_pcnt_change+se_pcnt_change),
                position = position_dodge())+
  
  labs(title = "% kelp grazed from raw data for caged and control treatments", 
       x = "Predator treatment", y = "% kelp grazed", 
       fill = "Upstream/downstream of cage",
       caption = "Error bars show 1 SE from mean") + #maybe this isn't the best  - it shows variability in the raw data, but idk
  
  theme_classic() +
  scale_fill_manual(values = c('#DAF7A6','#5A5A5A') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Predator treatment")


#caged and control mean

c <- ggplot(avg_kelp%>%filter(Treatment %in% c("Caged","Control")), 
       aes(x = Treatment, y = avg_pcnt_change,fill = Treatment)) +
  
  geom_bar(stat = "identity", 
           position = position_dodge()) +
  
  geom_errorbar(aes(ymin = avg_pcnt_change-se_pcnt_change, 
                    ymax = avg_pcnt_change+se_pcnt_change),
                position = position_dodge())+
  
  labs(title = "% kelp grazed from raw data for caged and control treatments", 
       x = "Predator treatment", y = "% kelp grazed", 
       fill = "Upstream/downstream of cage",
       caption = "Error bars show 1 SE from mean") + #maybe this isn't the best  - it shows variability in the raw data, but idk
  
  theme_classic() +
  
  scale_fill_manual(values = c('#DAF7A6','#5A5A5A') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Predator treatment")




