############
#load files 
############
#should already be loaded if you ran  00_cleaning first
#kelp_clean <- read_csv("data/processed data/cleaned data/kelp_clean.csv")
#behavior_clean <- read_csv("data/processed data/cleaned data/behavior_clean.csv")


###########################################
#adding new columns to kelp and behavior df
###########################################

kelp <- kelp_clean %>%
  mutate(weight_diff = Kelp_weight_before_g - Kelp_weight_after_g,
         #cage positioned at cline 5.5. kelp ID is analogous to cline, since
         #kelp blade 1 placed in cline 1 etc. inflow at cline 0 and outflow
         #at cline 12. If kelp ID < 5.5, closer to inflow half of tank in 
         #relation to cage, and therefore upstream of star (arbitrary for control
         #and active treatments where no star in cage).
         Position = ifelse(Kelp_ID < 5.5,"Upstream","Downstream")) %>%
  
  #making trial and rep_per_trial character and treatment a factor
  mutate(across(c(Trial, Rep_per_trial), as.character)) %>%
  mutate(across(c(Treatment, Position), as.factor)) 
  


urch_behavior <- behavior_clean %>%
  distinct(Day_numrecord, Treatment, Trial, Rep_per_trial, Cline, Cline_seg, .keep_all = TRUE)

###########################################
#averaging kelp blades in a tank
###########################################

#for plots
#averaged over trials (and thus also 12 blades in a tank)
avg_kelp_wholetank <- kelp %>%
  group_by(Treatment) %>%
  mutate(avg_weight_diff = mean(weight_diff),
         sd_weight_diff = sd(weight_diff),
         se_weight_diff = sd(weight_diff)/sqrt(length(weight_diff)))  %>%
  distinct(Treatment, avg_weight_diff, sd_weight_diff, se_weight_diff)

avg_kelp_updn <- kelp %>%
  group_by(Treatment, Position) %>%
  mutate(updn_avg_weight_diff = mean(weight_diff),
         updn_sd_weight_diff = sd(weight_diff),
         updn_se_weight_diff = sd(weight_diff)/sqrt(length(weight_diff))) %>%
  distinct(Treatment, updn_avg_weight_diff, updn_sd_weight_diff, updn_se_weight_diff)



