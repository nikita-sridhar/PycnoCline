############
#load files 
############
#should already be loaded if you ran  01_manipulation first
#kelp <- read_csv("data/processed data/manipulated data/kelp.csv")
#behavior <- read_csv("data/processed data/manipulated data/urch_behavior.csv")
#add urchin roster

kelp <- kelp %>% 
  mutate(across(Treatment, as.factor))

#calculating SD by group for raw data for error bars - error across treatments (not including up/dn)
kelp_processed_sd_treatment <- kelp %>%
  group_by(Treatment) %>%
  mutate(raw_sd = sd(weight_pcnt_change),
         raw_mean = mean(weight_pcnt_change)) %>%
  ungroup() %>%
  mutate(across(Treatment, as.factor)) %>%
  unique()

kelp_processed_sd_treatment_updn <- kelp %>%
  group_by(Treatment, up_down) %>%
  mutate(raw_sd = sd(weight_pcnt_change),
         raw_mean = mean(weight_pcnt_change)) %>%
  ungroup() %>%
  mutate(across(c(Treatment, up_down), as.factor)) %>%
  unique()

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


#merging SD (calculated over treatment) for future plots
mod1a.1_withsd <- kelp_processed_sd_treatment_updn %>%
  distinct(Treatment, up_down, raw_sd, raw_mean) %>%
  merge(predicted_mod_1a.1)

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

#merging SD (calculated over treatment) for future plots
mod1b_withsd <- kelp_processed_sd_treatment %>%
  distinct(Treatment, raw_sd, raw_mean) %>%
  merge(predicted_mod_1b)

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
                
                 data = kelp,
               
                family = gamma())

sjPlot::tab_model(mod_1c)

ggplot(kelp, aes(x=weight_pcnt_change))+
  geom_histogram() +
  facet_wrap(vars(Treatment))


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

#merging SD (calculated over treatment) for future plots
mod1d_withsd <- kelp_processed_sd_treatment_updn %>%
  distinct(Treatment, raw_sd, raw_mean) %>%
  merge(predicted_mod_1d)

#TO DO: Figure out why p values not lining up with what I did in JMP. also, can't take
#sqrt of negative num, and sometimes value was negative so need to think abt that
#also, taking sqrt of abs not solution bc if neg means increase in kelp mass. 
#figure out what you did for this in JMP - maybe remove these values?



#TO DO: calculate lsmeans and St error to use for plots.
