library(tidyverse)

#load files
kelp <- read_csv("data/raw data/RawData - Kelp.csv")
behavior <- read_csv("data/raw data/RawData - Behavior.csv")


#find avg pyc position in a trial - will be used later, esp imp for active treatment

#on per time pt scale
pyc_position <- behavior %>%
  select(Date, Day_numrecord,Time,Trial,Treatment,Cline,Pyc_position) %>%
  mutate(Pyc_position = ifelse(Pyc_position == 1, Cline, NA)) 
#on per trial scale
avg_pyc_position <- pyc_position %>%
  group_by(Trial, Treatment) %>%
   summarise(avg_pyc_position = mean(Pyc_position)) %>%
  ungroup() %>%
  mutate(avg_pyc_position = ifelse(Treatment %in% c("Caged", "Control"),5.5, avg_pyc_position))


#for distance from pyc calculation, can't use average pyc value as that is on a whole trial
#scale - instead at each time point need to calculate distance, then average this
#to get back on the trial scale. cline num and kelp id synonymous
dist_from_pyc <- behavior %>%
  group_by(Trial, Treatment, Day_numrecord) %>%
  summarise(dist_from_pyc = abs())

kelp <- kelp %>%
  #adding column for avg pyc position
  merge(avg_pyc_position, by = c("Treatment", "Trial")) %>%
  mutate(#column for difference in kelp weight and the pcnt change in kelp weight
         weight_diff = Kelp_weight_before_g - Kelp_weight_after_g,
         weight_pcnt_change = ((Kelp_weight_before_g - Kelp_weight_after_g)/
                                Kelp_weight_before_g)*100,
         #column for blade's upstream/downstream position relative to pycno
         up_down = ifelse(Kelp_ID < avg_pyc_position,"Upstream","Downstream"))
   



  