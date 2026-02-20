

#determining whether or not to include interaction term
cagecontrol_mod_1 <- glmmTMB::glmmTMB((pos_weight_pcnt_change) ~ 
                                        
                                        Treatment + Position  + 
                                        
                                        
                                        (1| Trial/Rep_per_trial),
                                      
                                      data = kelp ,
                                      
                                      family = Gamma(link = "log"))

cagecontrol_mod_2 <- glmmTMB::glmmTMB((pos_weight_pcnt_change) ~ 
                                        
                                        Treatment*Position +
                                        
                                        
                                        (1| Trial/Rep_per_trial),
                                      
                                      data = kelp ,
                                      
                                      family = Gamma(link = "log"))

anova(cagecontrol_mod_1,cagecontrol_mod_2)

#centering response var
kelp$pos_weight_pcnt_change <- as.numeric(kelp$pos_weight_pcnt_change)
kelp <- kelp %>%
  mutate(kelp_pos_center = pos_weight_pcnt_change - mean(pos_weight_pcnt_change, na.rm=T) )


#######################################################################################
#ALL
#######################################################################################
#all treatments, all regions of the tank
all_mod <- glmmTMB::glmmTMB((pos_weight_pcnt_change) ~ 
                              
                              #Fixed effects
                              Treatment + 
                              
                              #Random effects
                              (1| Trial/Tank/Rep_per_trial),
                            
                            #Data filter: caged and control treatments
                            data = kelp,
                            family = gaussian(link = "identity"))

sjPlot::tab_model(all_mod)

#to get predicted values for a plot:
predicted_all_mod <- ggpredict(model = all_mod, terms = "Treatment") %>% 
  rename(Treatment = x) 

#######################################################
# Model 1a: Kelp Grazed in Caged vs. Control treatments
#######################################################

mod_1a <- lmer(sqrt(abs(weight_pcnt_change)) ~ 
                 
                 #Fixed effects
                 Treatment + Position + avg_dist_from_pyc + 
                 Treatment*Position + Treatment*avg_dist_from_pyc + Position*avg_dist_from_pyc +
                 Treatment*Position*avg_dist_from_pyc +
                 
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
                   Treatment + Position +
                   
                   #Random effects
                   (1| Trial) + (1 | Rep_per_trial),
                 
                 #Data filter: caged and control treatments
                 data = kelp %>% 
                   filter(Treatment == "Caged" | Treatment == "Control"))

sjPlot::tab_model(mod_1a.1)

#to get predicted values for a plot:
predicted_mod_1a.1 <- ggpredict(model = mod_1a.1, terms = c("Treatment", "Position")) %>% 
  rename(Treatment = x,
         Position = group) 

#now try same thing but as a GLMM
mod_1a.1_glmm <- glmmTMB::glmmTMB(weight_pcnt_change ~ 
                                    
                                    Treatment + Position +
                                    
                                    
                                    (1| Trial) + (1 | Rep_per_trial),
                                  
                                  data = kelp %>% 
                                    filter(Treatment == "Caged" | Treatment == "Control"),
                                  
                                  family = gaussian(link = "identity"))

sjPlot::tab_model(mod_1a.1_glmm)


#######################################################
#Model 1b: Kelp Grazed in Active vs. Control treatments
#######################################################

mod_1b <- glmmTMB::glmmTMB(weight_pcnt_change ~ 
                             
                             #Fixed effects
                             Treatment + 
                             
                             #Random effects
                             (1| Trial) + (1 | Rep_per_trial),
                           
                           #Data filter: caged and control treatments
                           data = kelp %>% 
                             filter(Treatment == "Active" | Treatment == "Control"),
                           family = gaussian(link = "identity"))

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
                             Treatment + Position + avg_dist_from_pyc + up_down_ratio +
                             Treatment*Position + Treatment*avg_dist_from_pyc + Position*avg_dist_from_pyc +
                             Treatment*Position*avg_dist_from_pyc +
                             
                             #Random effects
                             (1| Trial) + (1 | Rep_per_trial),
                           
                           data = kelp)

sjPlot::tab_model(mod_1c)

predicted_mod_1c <- ggpredict(model = mod_1c, terms = c("Treatment","Position")) %>% 
  rename(Treatment = x,
         Position = group) 


#######################################################
#Model 1d: Kelp Grazed across all treatments - simplified
#######################################################

mod_1d <- lmer((weight_pcnt_change) ~ 
                 
                 #Fixed effects
                 Treatment + Position +
                 
                 #Random effects
                 (1| Trial) + (1 | Rep_per_trial),
               
               
               data = kelp)


sjPlot::tab_model(mod_1d)

#to get predicted values for a plot:
predicted_mod_1d <- ggpredict(model = mod_1d, terms = c("Treatment","Position")) %>% 
  rename(Treatment = x,
         Position = group) 
#what im actually using, but investigating random effects:
transformed_kelp <- kelp %>%
  mutate(pos_weight_pcnt_change = ifelse(weight_pcnt_change <= 0, 0.00001, weight_pcnt_change))

#right skewed - 0 inflation?
ggplot(transformed_kelp, aes(x=pos_weight_pcnt_change))+
  geom_histogram() +
  facet_wrap(vars(Treatment))

cagecontrol_mod <- glmmTMB::glmmTMB(pos_weight_pcnt_change ~ 
                                      
                                      Treatment + Position + Treatment*Position +
                                      
                                      (1| Trial/Rep_per_trial/Kelp_ID),
                                    
                                    data = transformed_kelp %>% 
                                      filter(Treatment == "Caged" | Treatment == "Control"),
                                    
                                    family = Gamma(link = "log"))

sjPlot::tab_model(cagecontrol_mod)

activecontrol_mod <- glmmTMB::glmmTMB(pos_weight_pcnt_change ~ 
                                        
                                        Treatment  +
                                        
                                        (1| Trial/Rep_per_trial/Kelp_ID),
                                      
                                      data = transformed_kelp %>% 
                                        filter(Treatment == "Active" | Treatment == "Control"),
                                      
                                      family = Gamma(link = "log"))

sjPlot::tab_model(activecontrol_mod)


all_mod <- glmmTMB::glmmTMB(pos_weight_pcnt_change ~ 
                              
                              Treatment  +
                              
                              (1| Trial/Rep_per_trial/Kelp_ID),
                            
                            data = transformed_kelp,
                            
                            family = Gamma(link = "log"))

sjPlot::tab_model(all_mod)

#####################################################
#trying to use raw diff in kelp weight rather than pcnt
#####################################################

#Whole tank --------------------------------------------------------------------

#Distribution - normal
ggplot(kelp, aes(x = (weight_diff))) +
  geom_density() +
  facet_wrap(vars(Treatment))

#log linear model (aka generalized linear model w gaussian family)
model_glm <- glm(sqrt(weight_diff) ~ 
                   
                   Treatment,  
                 
                 data = kelp, 
                 family = gaussian(link = "identity"))

summary(model_glm)
anova(model_glm)
EMM_updn <- emmeans(model_glm, ~ Treatment, type = "response")
EMM_updn
CON_updn <- pairs(EMM_updn)
CON_updn


model_glm <- lm(sqrt(pos_weight_diff) ~ 
                  
                  Treatment,  
                
                data = kelp, 
                family = gaussian(link = "identity"))

summary(model_glm)
anova(model_glm)
EMM_updn <- emmeans(model_glm, ~ Treatment, type = "response")
EMM_updn
CON_updn <- pairs(EMM_updn)
CON_updn

model_glm <- lm((pos_weight_diff) ~ 
                  
                  Treatment,  
                
                data = kelp, 
                family = gaussian(link = "identity"))

summary(model_glm)
anova(model_glm)
EMM_updn <- emmeans(model_glm, ~ Treatment, type = "response")
EMM_updn
CON_updn <- pairs(EMM_updn)
CON_updn

model_glm <- glmmTMB(sqrt(pos_weight_diff) ~ 
                       
                       Treatment,  
                     
                     data = kelp, 
                     family = gaussian(link = "identity"))

summary(model_glm)
anova(model_glm)
EMM_updn <- emmeans(model_glm, ~ Treatment, type = "response")
EMM_updn
CON_updn <- pairs(EMM_updn)
CON_updn


model_glm <- glmmTMB(sqrt(pos_weight_diff) ~ 
                       
                       Treatment,  
                     
                     data = kelp, 
                     family = gaussian(link = "identity"))

summary(model_glm)
anova(model_glm)
EMM_updn <- emmeans(model_glm, ~ Treatment, type = "response")
EMM_updn
CON_updn <- pairs(EMM_updn)
CON_updn

model_glm <- glm((prob_weight_pcnt_change) ~ 
                   
                   Treatment,  
                 
                 data = kelp, 
                 family = beta_family())

summary(model_glm)
anova(model_glm)
EMM_updn <- emmeans(model_glm, ~ Treatment, type = "response")
EMM_updn
CON_updn <- pairs(EMM_updn)
CON_updn

model_beta <- betareg(prob_weight_pcnt_change ~ 
                        
                        Treatment + Kelp_weight_before_g + Treatment*Kelp_weight_before_g,  
                      
                      data = kelp)
summary(model_beta)
EMM_updn <- emmeans(model_beta, ~ Treatment, type = "response")
EMM_updn
CON_updn <- pairs(EMM_updn)
CON_updn

model_beta <- lm(sqrt(pos_weight_pcnt_change) ~ 
                   
                   Treatment + Kelp_weight_before_g,  
                 
                 data = kelp)
summary(model_beta)
EMM_updn <- emmeans(model_beta, ~ Treatment, type = "response")
EMM_updn
CON_updn <- pairs(EMM_updn)
CON_updn


model_glm <- lm(weight_diff ~ 
                  
                  Treatment,  
                
                data = kelp)

summary(model_glm)
anova(model_glm)
EMM_updn <- emmeans(model_glm, ~ Treatment, type = "response")
EMM_updn
CON_updn <- pairs(EMM_updn)
CON_updn


model_glm <- glmmTMB(sqrt(pos_weight_pcnt_change) ~ 
                       
                       Treatment ,  
                     
                     data = kelp, 
                     family = gaussian(link = "identity"))

summary(model_glm)
anova(model_glm)
EMM_updn <- emmeans(model_glm, ~ Treatment, type = "response")
EMM_updn
CON_updn <- pairs(EMM_updn, type = "response")
CON_updn


#Caged vs. control w updn ------------------------------------------------------
#******* THIS IS THE ONE ********
#do this but glm and show random effect doesn't matter
model <- lm(weight_diff ~ 
              
              Treatment + Position + Treatment*Position,  
            
            data = kelp)

summary(model)
anova(model)

EMM <- emmeans(model, ~ Treatment + Position + Treatment*Position, type = "response")


contrast_weights <- list(
  Caged_Control = c(control_upstream = 0.5,
                    active_upstream = 0,
                    caged_upstream = -0.5,
                    control_downstream = 0.5,
                    active_downstream = 0,
                    caged_downstream = -0.5),
  
  Active_Caged = c(control_upstream = 0,
                   active_upstream = 0.5,
                   caged_upstream = -0.5,
                   control_downstream = 0,
                   active_downstream = 0.5,
                   caged_downstream = -0.5),
  
  Active_Control = c(control_upstream = -0.5,
                     active_upstream = 0.5,
                     caged_upstream = 0,
                     control_downstream = -0.5,
                     active_downstream = 0.5,
                     caged_downstream = 0),
  
  Active_CagedControl = c(control_upstream = -0.25,
                          active_upstream = 0.5,
                          caged_upstream = -0.25,
                          control_downstream = -0.25,
                          active_downstream = 0.5,
                          caged_downstream = -0.25),
  
  CagedDown_CagedUp = c(control_upstream = 0,
                        active_upstream = 0,
                        caged_upstream = -1,
                        control_downstream = 0,
                        active_downstream = 0,
                        caged_downstream = 1),
  
  ControlDown_ControlUp = c(control_upstream = -1,
                            active_upstream = 0,
                            caged_upstream = 0,
                            control_downstream = 1,
                            active_downstream = 0,
                            caged_downstream = 0)
  
)


mod_contrasts <- contrast(EMM, contrast_weights)
mod_contrasts %>% 
  data.frame() %>%   
  mutate(across(where(is.numeric), round, digits = 4)) %>%
  flextable() 

contrast(mod_contrasts)



#lm but only caged and contorl treatments

model_glm <- lm(weight_diff ~ 
                  
                  Treatment + Position + Treatment*Position,  
                
                data = kelp%>% filter(Treatment == c("Caged","Control")))

summary(model_glm)
anova(model_glm)
EMM_updn <- emmeans(model_glm, ~ Treatment|Position, type = "response")
EMM_updn
CON_updn <- pairs(EMM_updn, type = "response")
CON_updn

EMM_updn <- emmeans(model_glm, ~ Position|Treatment, type = "response")
EMM_updn
CON_updn <- pairs(EMM_updn, type = "response")
CON_updn

##############################################################################
#adding the value of most neg value to ALL values to shift distribution to not 
#have 0s but still be able to use transformations/diff models other than lm 
#(only lm really can take 0s)

kelp2 <- kelp %>%
  mutate(weight_diff_shifted = weight_diff + 3.597)

ggplot(kelp2, aes(x = weight_diff_shifted)) +
  geom_density() +
  facet_wrap(vars(Treatment))

ggplot(kelp2, aes(x = sqrt(weight_diff_shifted))) +
  geom_density() +
  facet_wrap(vars(Treatment))

#whole tank
glmm <- glmmTMB(sqrt(weight_diff_shifted) ~ 
                  
                  Treatment,
                
                data = kelp2 ,
                
                family = gaussian(link = "identity"))

summary(glmm)
EMM_updn <- emmeans(glmm, ~ Treatment, type = "response")
EMM_updn
CON_updn <- pairs(EMM_updn, type = "response")
CON_updn


#all treatments w up/dn
glmm <- glmmTMB(sqrt(weight_diff_shifted) ~ 
                  
                  Treatment + Position  + Treatment*Position,
                
                data = kelp2 ,
                
                family = gaussian(link = "identity"))

summary(glmm)
EMM_updn <- emmeans(glmm, ~ Treatment|Position, type = "response")
EMM_updn
CON_updn <- pairs(EMM_updn, type = "response")
CON_updn

EMM_updn <- emmeans(glmm, ~ Position|Treatment, type = "response")
EMM_updn
CON_updn <- pairs(EMM_updn, type = "response")
CON_updn

#caged and control w up/dn
glmm <- glmmTMB(sqrt(weight_diff_shifted) ~ 
                  
                  Treatment + Position  + Treatment*Position,
                
                data = kelp2 %>% filter(Treatment == c("Caged","Control")) ,
                
                family = gaussian(link = "identity"))

summary(glmm)
EMM_updn <- emmeans(glmm, ~ Treatment|Position, type = "response")
EMM_updn
CON_updn <- pairs(EMM_updn, type = "response")
CON_updn


#back to what I started w to see how on earth i got sig values for everyhting 
#caged and control w up/dn
glmm <- glmmTMB(sqrt(pos_weight_pcnt_change) ~ 
                  
                  Treatment  +  (1|Trial),
                
                data = kelp2  ,
                
                family = gaussian(link = "identity"))

summary(glmm)
EMM_updn <- emmeans(glmm, ~ Treatment, type = "response")
EMM_updn
CON_updn <- pairs(EMM_updn, type = "response")
CON_updn

glmm <- glmmTMB(sqrt(pos_weight_pcnt_change) ~ 
                  
                  Treatment + Position + Treatment*Position +  (1|Trial),
                
                data = kelp2  ,
                
                family = gaussian(link = "identity"))

summary(glmm)
EMM_updn <- emmeans(glmm, ~ Treatment|Position, type = "response")
EMM_updn
CON_updn <- pairs(EMM_updn, type = "response")
CON_updn

glmm <- glmmTMB(sqrt(pos_weight_pcnt_change) ~ 
                  
                  Treatment + Position + Treatment*Position +  (1|Trial),
                
                data = kelp2 %>% filter(Treatment == c("Caged", "Control")),
                
                family = gaussian(link = "identity"))

summary(glmm)
EMM_updn <- emmeans(glmm, ~ Treatment|Position, type = "response")
EMM_updn
CON_updn <- pairs(EMM_updn, type = "response")
CON_updn

