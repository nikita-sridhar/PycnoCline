############
#load files 
############
#should already be loaded if you ran  00_cleaning first
#kelp_clean <- read_csv("data/processed data/cleaned data/kelp_clean.csv")
#behavior_clean <- read_csv("data/processed data/cleaned data/behavior_clean.csv")
#add urchin roster

#################################
#find avg pyc position in a trial
#################################

#on per time pt scale
pyc_position <- behavior_clean %>%
  select(Date, Day_numrecord, Time, Trial, Rep_per_trial, Treatment, Cline, Pyc_position) %>%
  group_by(Trial, Rep_per_trial, Treatment, Day_numrecord) %>%
  mutate(#If active treatment, pyc position is equal to the value of the cline 
    #where the corresponding pycno position is 1. index cline value where 
    #pyc position equal to 1. If not active, change to 5.5 (cage position)
    Pyc_position = ifelse(Treatment == "Active", 
                          (Pyc_position = Cline[Pyc_position == 1]), 
                          5.5)) %>%
  na.omit()

#on per trial scale
avg_pyc_position <- pyc_position %>%
  group_by(Trial, Treatment,Rep_per_trial) %>%
  summarise(avg_pyc_position = mean(Pyc_position))

#################################
#distance btwn cline/kelp and pyc   
#################################

#at each time point, need to calculate distance btwn cline/kelp and pyc position, 
#then average this to get back on the trial scale. cline num and kelp id synonymous

avg_dist_from_pyc <- pyc_position %>%
  mutate(dist_from_pyc = abs(Cline-Pyc_position)) %>%
  group_by(Trial, Treatment, Rep_per_trial) %>%
  summarise(avg_dist_from_pyc = mean(dist_from_pyc))

################
#pycno activity
################

#for caged/control this is 0. for active treatments,  calculate difference
#between clines in diff time points to get dist moved. but, w 3 hour sampling 
#interval, pyc could've moved way more. so we also  use % time active, which is, 
#if pyc moved from one time point to the next, it gets an "active" tally. the 
#percentage activity is calculated as sum(active)/num time point intervals * 100

pyc_activity <- pyc_position %>%
  #getting rid of cline rows (don't need that anymore) - want at day_num scale
  group_by(Trial, Treatment, Day_numrecord, Rep_per_trial) %>%
  summarise(Pyc_position = mean(Pyc_position)) %>%
  ungroup() %>%
  #flag movement (active = true/false) when position is different to prev time point position
  arrange(Trial,Treatment, Rep_per_trial, Day_numrecord) %>%
  group_by(Trial, Treatment, Rep_per_trial) %>%
  mutate(Pyc_position_next_time = lead(Pyc_position, 1),
         pyc_active = ifelse(Pyc_position == Pyc_position_next_time,"N","Y"),
         Pyc_dist_moved = abs(Pyc_position_next_time - Pyc_position)) %>%
  ungroup()

pyc_pcnt_activity <- pyc_activity %>%
  filter(!is.na(pyc_active)) %>%
  group_by(Trial, Treatment, Rep_per_trial) %>%
  summarise(pyc_pcnt_activity = (sum(pyc_active == "Y", na.rm=TRUE)/n())*100,
            pyc_avg_dist_moved = mean(Pyc_dist_moved, na.rm = TRUE))

#####################################
#adding urchin info for kelp analysis
#####################################

#urch_size <- urch_roster %>%
#group_by(Trial,Tank) %>%
#summarise(mean(Size_cm), var(Size_cm)) %>%


###########################################
#adding new columns to kelp and behavior df
###########################################

kelp <- kelp_clean %>%
  #adding columns for avg pyc position, distance from pyc, and pyc activity
  merge(avg_pyc_position, by = c("Treatment", "Trial", "Rep_per_trial")) %>%
  merge(avg_dist_from_pyc, by = c("Treatment", "Trial","Rep_per_trial")) %>%
  merge(pyc_pcnt_activity, by = c("Treatment", "Trial","Rep_per_trial")) %>%
  mutate(#difference in kelp weight and pcnt change in kelp weight
    weight_diff = Kelp_weight_before_g - Kelp_weight_after_g,
    weight_pcnt_change = ((Kelp_weight_before_g - Kelp_weight_after_g)/
                            Kelp_weight_before_g)*100,
    #kelp blade's upstream/downstream position relative to pycno
    up_down = ifelse(Kelp_ID < avg_pyc_position,"Up","Down")) %>%
  #up_down ratio calculation
  group_by(Trial, Treatment, Rep_per_trial) %>%
  mutate(up_down_ratio = sum(up_down=="Up")/sum(up_down=="Down")) %>%
  ungroup() %>%
#making trial and rep_per_trial character
  mutate(across(c(Trial, Rep_per_trial), as.character))

#can add urchin roster info if you want:
#left_join(urch_size,by = c("Trial","Tank")) %>%

urch_behavior <- behavior_clean %>%
  select(-Pyc_position) %>%
  
  #these are on time pt level
  merge(pyc_position[,c("Pyc_position","Treatment", "Trial", "Rep_per_trial", "Cline")], 
        by = c("Treatment", "Trial", "Rep_per_trial", "Cline")) %>%
  merge(pyc_activity[,c("pyc_active","Treatment", "Trial", "Rep_per_trial")], 
        by = c("Treatment", "Trial", "Rep_per_trial"))  %>%
 
   #these are averaged over all time pts within trial
  merge(avg_pyc_position, by = c("Treatment", "Trial", "Rep_per_trial")) %>%
  merge(avg_dist_from_pyc, by = c("Treatment", "Trial", "Rep_per_trial")) %>%
  merge(pyc_pcnt_activity, by = c("Treatment", "Trial", "Rep_per_trial")) %>%
  
  mutate(#upstream/downstream position relative to pycno
    up_down = ifelse(Cline < avg_pyc_position,"Up","Down")) %>%
  #up_down ratio calculation
  group_by(Trial, Treatment, Rep_per_trial) %>%
  mutate(up_down_ratio = sum(up_down=="Up")/sum(up_down=="Down")) %>%
  ungroup() #%>%
#adding urchin roster info
#left_join(urch_size,by = c("Trial","Tank")) %>%



#TO DO: investigate why kelp variable doesn't have
#i think there's some prob in manipualtion -- bc no trial 10 in manipulated kelp. and model results
#are v difff to JMP - nothin significant now -- need to investigate
