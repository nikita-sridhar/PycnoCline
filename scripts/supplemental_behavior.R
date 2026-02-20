#PYCNOCLINE behavior ###########################################################

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
  add_header_lines(values = "In crevice") %>%
  italic(i = 1, part = "header") %>%
  bold(i = 2, part = "header") %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>%
  set_header_labels("Pr..Chisq." = "Pr(>Chisq)") %>%
  #body formatting
  colformat_double(j = c(1:4), digits = 3) %>%
  autofit()  

#moving
car::Anova(moving_model) %>%
  data.frame() %>%
  as_tibble(rownames = " ", colnames = " ") %>%
  mutate(Pr..Chisq. = ifelse(Pr..Chisq. < 0.0001, "< 0.0001",round(Pr..Chisq., 3))) %>%
  flextable() %>%
  #header formatting
  add_header_lines(values = "Moving") %>%
  italic(i = 1, part = "header") %>%
  bold(i = 2, part = "header") %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>%
  set_header_labels("Pr..Chisq." = "Pr(>Chisq)") %>%
  #body formatting
  colformat_double(j = c(1:4), digits = 3) %>%
  autofit()

#onkelp
car::Anova(onkelp_model) %>%
  data.frame() %>%
  as_tibble(rownames = " ", colnames = " ") %>%
  mutate(Pr..F. = ifelse(Pr..F. < 0.0001, "< 0.0001",round(Pr..F., 3))) %>%
  flextable() %>%
  #header formatting
  add_header_lines(values = "On kelp") %>%
  italic(i = 1, part = "header") %>%
  bold(i = 2, part = "header") %>%  align(i = 1, align = 'left', part = 'header') %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>%
  set_header_labels("Pr..F." = "Pr(>F)") %>%
  #body formatting
  colformat_double(j = c(1:4), digits = 3) %>%
  autofit()                                          




