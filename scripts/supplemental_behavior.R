#PYCNOCLINE behavior ###########################################################

#touching kelp, in a crevice, moving: % of all urchins in a tank for each time a 
#recording was made (day_numrecord)

urch_bhvr_tank <- urch_behavior %>%
  group_by(Day_numrecord, Treatment, Trial, Rep_per_trial, Position) %>%
  summarise(tank_total_urchin = sum(TOTAL_clinesegURCH, na.rm = TRUE),
            #num urchins on kelp (grazing and/or touching kelp)
            tank_total_onkelp = sum(urch_GRAZE + urch_TOUCHKELP),
            pcnt_onkelp = ((tank_total_onkelp/tank_total_urchin)*100),
            #num urchins moving)
            tank_total_moving = sum(Urch_MOVING),
            pcnt_moving = ((tank_total_moving/tank_total_urchin)*100),
            #num urchins in a crevice)
            tank_total_crev = sum(urch_CREVICE),
            pcnt_crev = ((tank_total_crev/tank_total_urchin))*100) 


#averaging at treatment scale (for full tank plots)

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

#averaging at treatment and up down scale (for up/dn plot)

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


#############################################################################
#MODELS
#############################################################################

ggplot(urch_bhvr_tank, aes(x = pcnt_onkelp)) +  
  geom_density() +
  facet_wrap(vars(Treatment)) 

urch_bhvr_tank_forgamma <- urch_bhvr_tank %>%
  mutate(pcnt_crev = ifelse(pcnt_crev <= 0, 0.0001, pcnt_crev),
         pcnt_moving = ifelse(pcnt_moving <= 0, 0.0001, pcnt_moving),
         pcnt_onkelp = ifelse(pcnt_onkelp <= 0, 0.0001, pcnt_onkelp))

urch_bhvr_tank_forbeta <- urch_bhvr_tank %>%
  mutate(pcnt_crev = case_when(pcnt_crev == 0 ~ 0.0001,
                               pcnt_crev >= 1 ~ 0.9999),
         pcnt_moving = case_when(pcnt_moving == 0 ~ 0.0001,
                                 pcnt_moving >= 1 ~ 0.9999),
         pcnt_onkelp = case_when(pcnt_onkelp == 0 ~ 0.0001,
                                 pcnt_onkelp >= 1 ~ 0.9999))

#note: tried beta, didn't run (convergence issues). 
crev_model <- glmmTMB(pcnt_crev ~ 
                      Treatment + Position + Treatment*Position + (1|Trial),  
                      data = urch_bhvr_tank_forgamma,
                      family = Gamma(link="log")) #nothing sig

moving_model <- glmmTMB(pcnt_moving ~ 
                        Treatment + Position + Treatment*Position + (1|Trial),  
                        data = urch_bhvr_tank_forgamma,
                        family = Gamma(link="log")) #treatment is sig

onkelp_model <- lmer(pcnt_onkelp ~ 
                      Treatment + Position + Treatment*Position + (1|Trial),  
                    data = urch_bhvr_tank) #all sig

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
  set_table_properties(width = 1, layout = "autofit") %>%
  fontsize(size = 14, part = "all")

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
  align(j = 4, align = 'right', part = "all") %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>%
  set_header_labels("Pr..Chisq." = "Pr(>Chisq)") %>%
  #body formatting
  colformat_double(j = c(1:4), digits = 3) %>%
  set_table_properties(width = 1, layout = "autofit") %>%
  fontsize(size = 14, part = "all")

#onkelp
car::Anova(onkelp_model) %>%
  data.frame() %>%
  as_tibble(rownames = " ", colnames = " ") %>%
  mutate(Pr..Chisq. = ifelse(Pr..Chisq. < 0.0001, "< 0.0001",round(Pr..Chisq., 3))) %>%
  flextable() %>%
  #header formatting
  add_header_lines(values = "On kelp") %>%
  italic(i = 1, part = "header") %>%
  bold(i = 2, part = "header") %>%    
  align(j = 4, align = 'right', part = "all") %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>%
  set_header_labels("Pr..Chisq." = "Pr(>Chisq)") %>%
  #body formatting
  colformat_double(j = c(1:4), digits = 3) %>%
  set_table_properties(width = 1, layout = "autofit") %>%
  fontsize(size = 14, part = "all")     


#############################################################################
#POST HOC TEST
#############################################################################

#for all behaviors, interaction term not signif (only treatment), so do contrasts at the scale of just treatment

EMM_moving <- emmeans(moving_model, ~ Treatment)
EMM_onkelp <- emmeans(onkelp_model, ~ Treatment)

#since these terms not sig in model, not that relevant. but adding here for plotting estimates
EMM_crev <- emmeans(crev_model, ~ Treatment)
EMM_onkelp_updn <- emmeans(onkelp_model, ~ Treatment*Position)
EMM_moving_updn <- emmeans(moving_model, ~ Treatment*Position)
EMM_crev_updn <- emmeans(crev_model, ~ Treatment*Position)


pairs(EMM_moving)
pairs(EMM_onkelp)
pairs(EMM_onkelp_updn)

#for plots - whole tank
EMM_moving_summary <- summary(EMM_moving) %>%
  mutate(behavior = "moving") %>%
  select(-asymp.LCL, -asymp.UCL)
EMM_onkelp_summary <- summary(EMM_onkelp) %>%
  mutate(behavior = "onkelp") %>%
  select(-lower.CL, -upper.CL)
EMM_crev_summary <- summary(EMM_crev) %>%
  mutate(behavior = "crev") %>%
  select(-asymp.LCL, -asymp.UCL)

EMM_summary_fulltank <- rbind(EMM_moving_summary, EMM_onkelp_summary, EMM_crev_summary)

EMM_onkelp_updn_summary <- summary(EMM_onkelp_updn) %>%
  mutate(behavior = "onkelp") %>%
  select(-lower.CL, -upper.CL)
EMM_moving_updn_summary <- summary(EMM_moving_updn) %>%
  mutate(behavior = "moving") %>%
  select(-asymp.LCL, -asymp.UCL)
EMM_crev_updn_summary <- summary(EMM_crev_updn) %>%
  mutate(behavior = "crev") %>%
  select(-asymp.LCL, -asymp.UCL)

EMM_summary_updn <- rbind(EMM_moving_updn_summary, EMM_onkelp_updn_summary, EMM_crev_updn_summary)


###############################################################
#PLOTS
###############################################################

labels <- c("crev" = "In crevice",
            "moving" = "Moving",
            "onkelp" = "On kelp")

#plotting % urchins displaying behavior - FULL TANK ----------------------------
fulltank <- ggplot(EMM_summary_fulltank, aes(x = Treatment, y = emmean)) +
  
  geom_col(position = position_dodge(width = 0.9), fill = "#ADD8E6") +
  
  geom_errorbar(aes(ymin = emmean - SE,
                    ymax = emmean + SE),
                position = position_dodge(width = 0.9),
                width = 0.1,
                color = "#A9A9A9") +
  
  facet_wrap(~behavior, labeller = as_labeller(labels), scales = "free_y") +
  
  labs(y = "Estimated marginal mean % of urchins displaying behavior",
       caption = "Error bars represent marginal mean +/- 1 SE",
       title = "Sea urchin behaviors across the entire tank") +
  
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.3), add = c(0, 1)))  +
  
  theme_test()

#plotting same as above but - UP/DN regions ------------------------------------
dw <- 0.9 # dodge width
off <- dw / 4

updn <- ggplot(EMM_summary_updn, aes(x = Treatment, y = emmean, fill = Position)) +
  
  geom_col(position = position_dodge(width = 0.9)) +
  
  geom_errorbar(aes(ymin = emmean - SE,
                    ymax = emmean + SE),
                position = position_dodge(width = 0.9),
                width = 0.1,
                color = "#A9A9A9") +
  
  scale_fill_manual(values = c("#c5e3ed","#7997a1")) +
  
  facet_wrap(~behavior, labeller = as_labeller(labels), scales = "free_y") +
  
  labs(y = "Estimated marginal mean % of urchins displaying behavior",
       caption = "Error bars represent marginal mean +/- 1 SE",
       fill = "Position relative to cage",
       title = "Sea urchin behaviors across tank segments") +
  
  theme_test()

fulltank / updn




