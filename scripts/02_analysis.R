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

mod_1a <- lmer(sqrt(weight_pcnt_change) ~ 
                 
                 #Fixed effects
                 Treatment + up_down + avg_dist_from_pyc + 
                 Treatment*up_down + Treatment*avg_dist_from_pyc + up_down*avg_dist_from_pyc +
                 Treatment*up_down*avg_dist_from_pyc +
                 
                 #Random effects
                 (1| Trial) + (1 | Rep_per_trial),
                 
                 data = kelp)

anova(mod_1a)
parameters::p_value(mod_1a)








