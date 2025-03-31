###########
#load files
###########
kelp_raw <- read_csv("data/raw data/2023_Pycnocline_RawData - Kelp.csv")
behavior_raw <- read_csv("data/raw data/2023_Pycnocline_RawData - Behavior.csv")
#add urchin roster

###########
#clean kelp
###########
kelp_clean <- kelp_raw %>%
  filter(Trial != "Pilot") %>%
  filter(Trial != 2) %>%
  select(-Observer, -Notes) %>%
  mutate(Trial = as.character(Trial))

###############
#clean behavior
###############
behavior_clean <- behavior_raw %>%
  filter(Trial != "Pilot",
         Trial != 2) %>%
  #if treatment contains number 1/2, change rep_per_trial to 1/2. else, change to 1 
  #(since if treatment doesn't have a number next to it it's a 1 of 1 treatment for that trial)
  mutate(Rep_per_trial = case_when(grepl("1", Treatment) ~ 1,
                                   grepl("2", Treatment) ~ 2, TRUE ~ 1), .after = Trial,
         Treatment = case_when(grepl("Active", Treatment) ~ "Active",
                               grepl("Control", Treatment) ~ "Control",
                               grepl("Caged", Treatment) ~ "Caged"),
         Trial = as.character(Trial))
  



  