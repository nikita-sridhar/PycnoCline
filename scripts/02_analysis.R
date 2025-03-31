############
#load files 
############
#should already be loaded if you ran  01_manipulation first
#kelp <- read_csv("data/processed data/manipulated data/kelp.csv")
#behavior <- read_csv("data/processed data/manipulated data/urch_behavior.csv")
#add urchin roster

kelp <- kelp %>% 
  mutate(across(Treatment, as.factor))

#for plots
avg_kelp <- kelp %>%
  group_by(Treatment) %>%
  mutate(avg_pcnt_change = mean(weight_pcnt_change),
         sd_pcnt_change = sd(weight_pcnt_change),
         se_pcnt_change = sd(weight_pcnt_change)/sqrt(length(weight_pcnt_change))) %>%
  ungroup() %>%
  group_by(Treatment, up_down) %>%
  mutate(updn_avg_pcnt_change = mean(weight_pcnt_change),
            updn_sd_pcnt_change = sd(weight_pcnt_change),
            updn_se_pcnt_change = sd(weight_pcnt_change)/sqrt(length(weight_pcnt_change)))



#######################################################
# Model 1a: Kelp Grazed in Caged vs. Control treatments
#######################################################

mod_1a <- lmer(sqrt(abs(weight_pcnt_change)) ~ 
                 
                 #Fixed effects
                 Treatment + up_down + avg_dist_from_pyc + 
                 Treatment*up_down + Treatment*avg_dist_from_pyc + up_down*avg_dist_from_pyc +
                 Treatment*up_down*avg_dist_from_pyc +
                 
                 #Random effects
                 (1| Trial) + (1 | Rep_per_trial),
                 
                 #Data filter: caged and control treatments
                 data = kelp %>% 
                 filter(Treatment == "Caged" | Treatment == "Control"))

anova(mod_1a)
parameters::p_value(mod_1a)

sjPlot::tab_model(mod_1a)



#########################################################################
# Model 1a.1: Kelp Grazed in Caged vs. Control treatments - but simplified
#########################################################################

mod_1a.1 <- lmer(weight_pcnt_change ~ 
                 
                 #Fixed effects
                 Treatment + up_down +
                 
                 #Random effects
                 (1| Trial) + (1 | Rep_per_trial),
               
               #Data filter: caged and control treatments
               data = kelp %>% 
                 filter(Treatment == "Caged" | Treatment == "Control"))

sjPlot::tab_model(mod_1a.1)

#to get predicted values for a plot:
predicted_mod_1a.1 <- ggpredict(model = mod_1a.1, terms = c("Treatment", "up_down")) %>% 
  rename(Treatment = x,
         up_down = group) 

#######################################################
#Model 1b: Kelp Grazed in Active vs. Control treatments
#######################################################

mod_1b <- lmer(weight_pcnt_change ~ #removed the sqrt abs transformation bc ggeeffects didn't work..might consider removing for all mods
                 
                 #Fixed effects
                 Treatment + 
                 
                 #Random effects
                 (1| Trial) + (1 | Rep_per_trial),
               
               #Data filter: caged and control treatments
               data = kelp %>% 
                 filter(Treatment == "Active" | Treatment == "Control"))

sjPlot::tab_model(mod_1b)

#to get predicted values for a plot:
predicted_mod_1b <- ggpredict(model = mod_1b, terms = "Treatment") %>% 
  rename(Treatment = x) 


#######################################################
#Model 1c: Kelp Grazed across all treatments - main
#######################################################
#most informative model

mod_1c <- glmmTMB::glmmTMB(abs(weight_pcnt_change) ~ 
                 
                 #Fixed effects
                 Treatment + up_down + avg_dist_from_pyc + up_down_ratio +
                 Treatment*up_down + Treatment*avg_dist_from_pyc + up_down*avg_dist_from_pyc +
                 Treatment*up_down*avg_dist_from_pyc +
                 
                 #Random effects
                 (1| Trial) + (1 | Rep_per_trial),
                
                 data = kelp)

sjPlot::tab_model(mod_1c)

predicted_mod_1c <- ggpredict(model = mod_1c, terms = c("Treatment","up_down")) %>% 
  rename(Treatment = x,
         up_down = group) 


#######################################################
#Model 1d: Kelp Grazed across all treatments - simplified
#######################################################

mod_1d <- lmer((weight_pcnt_change) ~ 
                 
                 #Fixed effects
                 Treatment + up_down +
                 
                 #Random effects
                 (1| Trial) + (1 | Rep_per_trial),
               
               
               data = kelp)


sjPlot::tab_model(mod_1d)

#to get predicted values for a plot:
predicted_mod_1d <- ggpredict(model = mod_1d, terms = c("Treatment","up_down")) %>% 
  rename(Treatment = x,
         up_down = group) 



