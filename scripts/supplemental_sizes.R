#PYCNOCLINE - sizes

set_flextable_defaults(
  font.size = 11, font.family = "Arial",
  font.color = "#333333",
  table.layout = "fixed",
  border.color = "gray",
  padding.top = 3, padding.bottom = 3,
  padding.left = 4, padding.right = 4)


#trial info w some formatting
trial_info <- read_csv("data/raw/2023_Pycnocline_RawData - Trial.csv") %>%
  rename(Tank = Tank_num) %>%
  mutate(Rep_per_trial = case_when(grepl("1", Treatment) ~ 1,
                                   grepl("2", Treatment) ~ 2, TRUE ~ 1), .after = Trial,
         Treatment = case_when(grepl("Active", Treatment) ~ "Active",
                               grepl("Control", Treatment) ~ "Control",
                               grepl("Caged", Treatment) ~ "Caged"),
         Trial = as.character(Trial))



#making a table of average urchin sizes in Pycnolcine experiment
urch_roster <-  read_csv("data/raw/2023_Pycnocline_RawData - Urchin Roster.csv")

urch_size <- urch_roster %>%
  left_join(trial_info, by = c("Tank", "Trial")) %>%
group_by(Treatment) %>%
  summarise(size_mean_cm=mean(Size_cm, na.rm = TRUE), 
            size_sd_cm=sd(Size_cm, na.rm = TRUE)) %>%
  ungroup() %>%
  data.frame()

#flextable formatting
flextable(urch_size) %>%
  #header formatting
  add_header_lines(values = "Experiment 1") %>%
  italic(i = 1, part = "header") %>%
  bold(i = 2, part = "header") %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>%
  set_header_labels(size_mean_cm = 'Mean test size (cm)',
                    size_sd_cm = 'SD of test sizes (cm)') %>%
  italic(part = "all", j = 1) %>%
  #body formatting
  colformat_double(digits = 2) %>% #setting number of decimal places
  
  autofit() 
  


#average pycno sizes
pycno_roster <- read_csv("data/raw/2023_Pycnocline_RawData - Pycno Roster.csv") %>%
  mutate(Rep_per_trial = case_when(grepl("1", Treatment) ~ 1,
                                   grepl("2", Treatment) ~ 2, TRUE ~ 1), .after = Trial,
         Treatment = case_when(grepl("Active", Treatment) ~ "Active",
                               grepl("Control", Treatment) ~ "Control",
                               grepl("Caged", Treatment) ~ "Caged"),
         Trial = as.character(Trial)) %>%
  group_by(Treatment) %>%
  summarise(radius_mean_cm=mean(Radius_cm, na.rm = TRUE), 
            radius_sd_cm=sd(Radius_cm, na.rm = TRUE),
            wet_weight_mean_g=mean(Wet_weight_g, na.rm = TRUE),
            wet_weight_sd_g=sd(Wet_weight_g, na.rm = TRUE)) %>%
  ungroup() %>%
  data.frame()

pycno_roster <- pycno_roster[-3,]

#flextable formatting
flextable(pycno_roster) %>%
  #header formatting
  add_header_lines(values = "Experiment 1") %>%
  italic(i = 1, part = "header") %>%
  bold(i = 2, part = "header") %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>%
  set_header_labels(radius_mean_cm = 'Mean Radius (cm)',
                    radius_sd_cm = 'SD of Radius (cm)',
                    wet_weight_mean_g= "Mean Wet Weight (g)",
                    wet_weight_sd_g= "SD of Wet Weight (g)") %>%
  italic(part = "all", j = 1) %>%
  #body formatting
  colformat_double(digits = 2) %>% #setting number of decimal places
  
  autofit() 
