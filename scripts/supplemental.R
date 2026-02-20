#PYCNOCLINE

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
  set_header_labels(size_mean_cm = 'Mean Urchin Size (cm)',
                    size_sd_cm = 'SD of Urchin Sizes (cm)') %>%
  italic(part = "all", j = 1) %>%
  bold(part = "header") %>%
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
  set_header_labels(radius_mean_cm = 'Mean Radius (cm)',
                    radius_sd_cm = 'SD of Radius (cm)',
                    wet_weight_mean_g= "Mean Wet Weight (g)",
                    wet_weight_sd_g= "SD of Wet Weight (g)") %>%
  italic(part = "all", j = 1) %>%
  bold(part = "header") %>%
  #body formatting
  colformat_double(digits = 2) %>% #setting number of decimal places
  
  autofit() 


#behavior ###########################################################

#touching kelp, in a crevice, : of all urchins in a tank for each time a recording was made (day_numrecord)
#what percent are touching kelp? average over daynumrecord to get average per trial/treatment,
#average over trial to get average per treatment. 

urch_bhvr_tank <- urch_behavior %>%
  group_by(Day_numrecord, Treatment, Trial, Rep_per_trial, Position) %>%
  summarise(tank_total_urchin = sum(TOTAL_clinesegURCH, na.rm = TRUE),
            #num urchins on kelp (grazing and/or touching kelp)
            tank_total_onkelp = sum(urch_GRAZE + urch_TOUCHKELP),
            pcnt_onkelp = (tank_total_onkelp/tank_total_urchin)*100,
            #num urchins moving)
            tank_total_moving = sum(Urch_MOVING),
            pcnt_moving = (tank_total_moving/tank_total_urchin)*100,
            #num urchins in a crevice)
            tank_total_crev = sum(urch_CREVICE),
            pcnt_crev = (tank_total_crev/tank_total_urchin)*100) 

urch_bhvr_trial <- urch_bhvr_tank %>%
  group_by(Treatment, Trial) %>%
  summarise(#on kelp
    mean_onkelp = mean(pcnt_onkelp, na.rm = TRUE),
    sd_onkelp = sd(pcnt_onkelp, na.rm = TRUE), 
    #moving
    mean_moving = mean(pcnt_moving, na.rm = TRUE),
    sd_moving = sd(pcnt_moving, na.rm = TRUE),
    #crevice
    mean_crev = mean(pcnt_crev, na.rm = TRUE),
    sd_crev = sd(pcnt_crev, na.rm = TRUE))
  

urch_bhvr_treatment <- urch_bhvr_tank %>%
  group_by(Treatment) %>%
  summarise(#on kelp
            mean_onkelp = mean(pcnt_onkelp, na.rm = TRUE),
            sd_onkelp = sd(pcnt_onkelp, na.rm = TRUE), 
            #moving
            mean_moving = mean(pcnt_moving, na.rm = TRUE),
            sd_moving = sd(pcnt_moving, na.rm = TRUE),
            #crevice
            mean_crev = mean(pcnt_crev, na.rm = TRUE),
            sd_crev = sd(pcnt_crev, na.rm = TRUE))

urch_bhvr_treatment_updn <- urch_bhvr_tank %>%
  group_by(Treatment, Position) %>%
  summarise(#on kelp
    mean_onkelp = mean(pcnt_onkelp, na.rm = TRUE),
    sd_onkelp = sd(pcnt_onkelp, na.rm = TRUE), 
    #moving
    mean_moving = mean(pcnt_moving, na.rm = TRUE),
    sd_moving = sd(pcnt_moving, na.rm = TRUE),
    #crevice
    mean_crev = mean(pcnt_crev, na.rm = TRUE),
    sd_crev = sd(pcnt_crev, na.rm = TRUE))
  

#reshaping data for facetwrap
  #means
  urch_bhvr_treatment_long <- urch_bhvr_treatment %>%
  pivot_longer(cols = c("mean_onkelp","sd_onkelp", "mean_moving","sd_moving",
                        "mean_crev","sd_crev"),
               names_to = c("sumstat", "behavior"),
               names_sep = "_",
               values_to = "value") %>%
  
  pivot_wider(names_from = sumstat, values_from = value)
  
  urch_bhvr_treatment_updn_long <- urch_bhvr_treatment_updn %>%
    pivot_longer(cols = c("mean_onkelp","sd_onkelp", "mean_moving","sd_moving",
                          "mean_crev","sd_crev"),
                 names_to = c("sumstat", "behavior"),
                 names_sep = "_",
                 values_to = "value") %>%
  pivot_wider(names_from = sumstat, values_from = value)
  

labels <- c("crev" = "In crevice",
            "moving" = "Moving",
            "onkelp" = "On kelp")


#plotting % urchins displaying behavior - FULL TANK ----------------------------
fulltank <- ggplot(urch_bhvr_treatment_long, aes(x = Treatment, y = mean)) +
  
  geom_col(position = position_dodge(width = 0.9), fill = "#ADD8E6") +
  
  geom_errorbar(aes(ymin = mean - sd,
                    ymax = mean + sd),
                position = position_dodge(width = 0.9),
                width = 0.1,
                color = "#A9A9A9") +
  
  facet_wrap(~behavior, labeller = as_labeller(labels)) +
  
  labs(y = "Mean % of urchins displaying behavior",
       caption = "Error bars represent mean +/- 1 SD",
       title = "Sea urchin behaviors across the entire tank") +
  
  theme_bw()

#plotting same as above but - UP/DN regions ------------------------------------
dw <- 0.9 # dodge width
off <- dw / 4

updn <- ggplot(urch_bhvr_treatment_updn_long, aes(x = Treatment, y = mean, fill = Position)) +
  
  geom_col(position = position_dodge(width = 0.9)) +
  
  geom_errorbar(aes(ymin = mean - sd,
                    ymax = mean + sd),
                position = position_dodge(width = 0.9),
                width = 0.1,
                color = "#A9A9A9") +
  
  scale_fill_manual(values = c("#c5e3ed","#7997a1")) +

  facet_wrap(~behavior, labeller = as_labeller(labels)) +
  
  labs(y = "Mean % of urchins displaying behavior",
       caption = "Error bars represent mean +/- 1 SD",
       fill = "Position relative to cage",
       title = "Sea urchin behaviors across tank segments") +
  
  theme_bw()

fulltank / updn


#now trying models ###############################################################
#need to try this including adding pycno movement as a fixed effect
ggplot(urch_bhvr_tank, aes(x = pcnt_onkelp)) + #right skew - use gamma 
  geom_density() +
  facet_wrap(vars(Treatment)) 

urch_bhvr_tank_forgamma <- urch_bhvr_tank %>%
  mutate(pcnt_crev = ifelse(pcnt_crev <= 0, 0.0001, pcnt_crev),
         pcnt_moving = ifelse(pcnt_moving <= 0, 0.0001, pcnt_moving),
         pcnt_onkelp = ifelse(pcnt_onkelp <= 0, 0.0001, pcnt_onkelp))

crev_model <- glmmTMB(pcnt_crev ~ 
                        Treatment + Position + Treatment*Position + (1|Trial),  
                 data = urch_bhvr_tank_forgamma,
                 family = Gamma(link = "log")) #nothing sig
moving_model <- glmmTMB(pcnt_moving ~ 
                          Treatment + Position + Treatment*Position + (1|Trial),  
                        data = urch_bhvr_tank_forgamma,
                        family = Gamma(link = "log")) #treatment is sig
onkelp_model <- lm(pcnt_moving ~ 
                     Treatment + Position + Treatment*Position,  
                   data = urch_bhvr_tank) #treatment is sig

car::Anova(crev_model)
car::Anova(moving_model)
car::Anova(onkelp_model)

#crev
car::Anova(crev_model) %>%
  data.frame() %>%
  as_tibble(rownames = " ", colnames = " ") %>%
  mutate(Pr..Chisq. = ifelse(Pr..Chisq. < 0.0001, "< 0.0001",round(Pr..Chisq., 3))) %>%
  flextable() %>%
  #header formatting
  bold(i = 1, part = "header") %>%
  align(i = 1, align = 'left', part = 'header') %>%
  set_header_labels("Pr..Chisq." = "Pr(>Chisq)") %>%
  #body formatting
  colformat_double(j = c(1:4), digits = 3) %>%
  vline_left() %>%
  vline_right() %>%
  autofit()  

#moving
car::Anova(moving_model) %>%
  data.frame() %>%
  as_tibble(rownames = " ", colnames = " ") %>%
  mutate(Pr..Chisq. = ifelse(Pr..Chisq. < 0.0001, "< 0.0001",round(Pr..Chisq., 3))) %>%
  flextable() %>%
  #header formatting
  bold(i = 1, part = "header") %>%
  align(i = 1, align = 'left', part = 'header') %>%
  set_header_labels("Pr..Chisq." = "Pr(>Chisq)") %>%
  #body formatting
  colformat_double(j = c(1:4), digits = 3) %>%
  vline_left() %>%
  vline_right() %>%
  autofit()

#onkelp
car::Anova(onkelp_model) %>%
  data.frame() %>%
  as_tibble(rownames = " ", colnames = " ") %>%
  mutate(Pr..F. = ifelse(Pr..F. < 0.0001, "< 0.0001",round(Pr..F., 3))) %>%
  flextable() %>%
  #header formatting
  bold(i = 1, part = "header") %>%
  align(i = 1, align = 'left', part = 'header') %>%
  set_header_labels("Pr..F." = "Pr(>F)") %>%
  #body formatting
  colformat_double(j = c(1:4), digits = 3) %>%
  vline_left() %>%
  vline_right() %>%
  autofit()                                          




