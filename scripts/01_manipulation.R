############
#load files 
############
#should already be loaded if you ran  00_cleaning first
#kelp_clean <- read_csv("data/processed data/cleaned data/kelp_clean.csv")
#behavior_clean <- read_csv("data/processed data/cleaned data/behavior_clean.csv")


###########################################
#adding new columns to kelp and behavior df
###########################################

#pyc position per time pt scale (used to calculated upstream/downstream for active)
pyc_position <- behavior_clean %>%
  select(Date, Day_numrecord, Time, Trial, Rep_per_trial, Treatment, Cline, Pyc_position) %>%
  group_by(Trial, Rep_per_trial, Treatment, Day_numrecord) %>%
  mutate(#If active treatment, pyc position is equal to the value of the cline 
    #where the corresponding pycno position is 1. index cline value where 
    #pyc position equal to 1. If not active, change to 5.5 (cage position)
    Pyc_position = ifelse(Treatment == "Active", 
                          (Pyc_position = Cline[Pyc_position == 1]), 
                          5.5)) %>%
  na.omit()  #some NAs from when didn't record behavioral data (only NA for active bc position for caged/control is known)

#finding avg pyc position per trial (used to calculated upstream/downstream for active)
avg_pyc_position <- pyc_position %>%
  group_by(Trial, Treatment, Rep_per_trial) %>%
  summarise(avg_pyc_position = mean(Pyc_position))

kelp <- kelp_clean %>%
  #merging avg pyc position to determine whether a blade is upstream/downstream of the pyc in active
  merge(avg_pyc_position, by = c("Treatment", "Trial", "Rep_per_trial")) %>%
  mutate(weight_diff = Kelp_weight_before_g - Kelp_weight_after_g,
         #kelp ID is analogous to cline, since kelp blade 1 placed in cline 1 etc. 
         #cage positioned at cline 5.5, this is what "avg_pyc_position" is for 
         #caged and control. inflow at cline 0 and outflow at cline 12. 
         Position = ifelse(Kelp_ID < avg_pyc_position,"Upstream","Downstream")) %>%
  
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

#calculating sample size for raw data
#whole experiment:
kelp %>%  summarize(count = n())

#whole tank:
kelp %>% group_by(Treatment) %>% summarize(count = n())

#up/dn segments
kelp %>% group_by(Treatment, Position) %>% summarize(count = n())


