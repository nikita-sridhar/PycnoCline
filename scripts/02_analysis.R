############
#load files 
############
#should already be loaded if you ran  01_manipulation first
#kelp <- read_csv("data/processed data/manipulated data/kelp.csv")
#behavior <- read_csv("data/processed data/manipulated data/urch_behavior.csv")
#add urchin roster


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


#######################################################
#Model 1b: Kelp Grazed in Active vs. Control treatments
#######################################################

mod_1b <- lmer(sqrt(abs(weight_pcnt_change)) ~ 
                 
                 #Fixed effects
                 Treatment + 
                 
                 #Random effects
                 (1| Trial) + (1 | Rep_per_trial),
               
               #Data filter: caged and control treatments
               data = kelp %>% 
                 filter(Treatment == "Active" | Treatment == "Control"))

anova(mod_1b)
parameters::p_value(mod_1b)


#######################################################
#Model 1c: Kelp Grazed across all treatments
#######################################################
#most informative model

mod_1c <- lmer(sqrt(abs(weight_pcnt_change)) ~ 
                 
                 #Fixed effects
                 Treatment + up_down + avg_dist_from_pyc + up_down_ratio +
                 Treatment*up_down + Treatment*avg_dist_from_pyc + up_down*avg_dist_from_pyc +
                 Treatment*up_down*avg_dist_from_pyc +
                 
                 #Random effects
                 (1| Trial) + (1 | Rep_per_trial),
              
               
                 data = kelp)

anova(mod_1c)
parameters::p_value(mod_1c)

#TO DO: Figure out why p values not lining up with what I did in JMP. also, can't take
#sqrt of negative num, and sometimes value was negative so need to think abt that
#also, taking sqrt of abs not solution bc if neg means increase in kelp mass. 
#figure out what you did for this in JMP - maybe remove these values?

#TO DO: calculate lsmeans and St error to use for plots.
