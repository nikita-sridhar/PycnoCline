library(tidyverse)

behavior <- read.csv("data/raw data/ 2023_Pycnocline_RawData - Behavior_8_17_23.csv")
processed_kelp <- read.csv("data/processed data/processed_kelp.csv") 
processed_kelp$Treatment[processed_kelp$Treatment =="Active "] <- "Active"
processed_kelp$Trial_rep <- paste(processed_kelp$Trial, processed_kelp$Rep_per_trial, sep = "_")
urch_roster <- read.csv("data/raw data/urchin_roster.csv")


#not sure what this is
pyc_positions <- behavior %>%
  filter(grepl('Active',Treatment)) %>%
  select(Trial,Treatment,Day_numrecord,Cline,Pyc_position) %>%
  filter(Pyc_position ==1) %>%
  filter(Treatment == "Active ")

#ratio of upstream vs. downstream 
pyc_updown <- processed_kelp %>%
 group_by(Trial, Treatment, Rep_per_trial) %>%
  summarise(up_down = sum(Upstream.Downstream=="U")/sum(Upstream.Downstream=="D"))

processed_kelp_new <- left_join(processed_kelp, pyc_updown, by = c("Trial","Treatment", "Rep_per_trial"))

urch_size <- urch_roster %>%
  filter(Trial != "Pilot") %>%
  group_by(Trial,Tank) %>%
  summarise(mean(Size_cm), var(Size_cm)) %>%
  transform(Trial = as.numeric(Trial))

processed_kelp_new <- left_join(processed_kelp_new,urch_size,by = c("Trial","Tank")) %>%
  filter(Trial != 2) %>%
  mutate(Trial = as.character(Trial),
         Tank = as.character(Tank),
         Kelp_ID = as.character(Kelp_ID)) %>%
  mutate(pyc_active = if_else(Pcnt_time_active > 0, "Y", "N"))

test <- processed_kelp_new %>% filter(Treatment == "Active")

write.csv(processed_kelp_new, "data/processed data/processed_kelp_v2.csv")                            
                            

#trying to look at behavior data - creating upstream/downstream categories for caged/control
behavior_new <- behavior %>%
  mutate(Upstream_Downstream = case_when((Treatment != "Active") ~ 1,
                                          (WHR < 1.02 & sexe = 1) ~ 2,
                                          (WHR >= 0.85 & sexe = 2) ~ 3)
                                                               
behavior$UpDn = NA

behavior_new <- behavior %>%
  if

