

######
#model
######

#active vs.control 

ggplot(predicted_mod_1b, 
       aes(x = Treatment, y = predicted, fill = Treatment)) +
  
  geom_bar(stat = "identity", 
           position = position_dodge()) +
  
  geom_errorbar(aes(ymin = predicted-std.error, ymax =predicted+std.error), 
                position=position_dodge()) +
  
  labs(title = "Predicted values of % kelp grazed from GLMM", 
       x = "Predator treatment", y = "% kelp grazed", 
       fill = "Predator treatment",
       caption = "Note: error bars are made with respect to SE of model") + #maybe this isn't the best  - it shows variability in the raw data, but idk
  
  theme_classic() +
  scale_fill_manual(values = c('#DAF7A6','#5A5A5A') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Predator treatment")

#caged and control upstream vs. downstream

ggplot(mod1a.1_withsd, 
       aes(x = Treatment, y = predicted, fill = Position)) +
  
  geom_bar(stat = "identity", 
           position = position_dodge()) +
  
  #geom_errorbar(aes(ymin = raw_mean-raw_sd, ymax =raw_mean+raw_sd), 
  #position=position_dodge()) +
  
  labs(title = "Predicted values of % kelp grazed from GLMM", 
       x = "Predator treatment", y = "% kelp grazed", 
       fill = "Upstream/downstream of cage",
       caption = "Note: error bars are made with respect to SD and mean of raw data") + #maybe this isn't the best  - it shows variability in the raw data, but idk
  
  theme_classic() +
  scale_fill_manual(values = c('#DAF7A6','#5A5A5A') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Predator treatment")

#using model with combined treatments (which earlier we said we shouldn't do but its what made the initial interesting graph)
#caged and control

ggplot(mod1d_withsd, 
       aes(x = Treatment, y = predicted, fill = Position)) +
  
  geom_bar(stat = "identity", 
           position = position_dodge()) +
  
  #geom_errorbar(aes(ymin = raw_mean-raw_sd, ymax =raw_mean+raw_sd), 
  #position=position_dodge()) +
  
  labs(title = "Predicted values of % kelp grazed from GLMM", 
       x = "Predator treatment", y = "% kelp grazed", 
       fill = "Upstream/downstream of cage",
       caption = "Note: error bars are made with respect to SD and mean of raw data") + #maybe this isn't the best  - it shows variability in the raw data, but idk
  
  theme_classic() +
  scale_fill_manual(values = c('#DAF7A6','#5A5A5A') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Predator treatment")





#########
#raw data
#########

#active vs.control 

a_c <- ggplot(avg_kelp%>%filter(Treatment %in% c("Active","Control")), 
              aes(x = Treatment, y = avg_pcnt_change, fill = Treatment)) +
  
  geom_bar(stat = "identity", 
           position = position_dodge()) +
  
  geom_errorbar(aes(ymin = avg_pcnt_change-se_pcnt_change, 
                    ymax = avg_pcnt_change+se_pcnt_change),
                position = position_dodge()) +
  
  labs(title = "Kelp grazed by urchins under different predator activity levels", 
       x = "Predator treatment", y = "% kelp grazed", 
       fill = "Predator treatment") + 
  
  theme_classic() +
  scale_fill_manual(values = c('#DAF7A6','#5A5A5A') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Predator treatment")

a_c

#caged and control upstream vs. downstream

c_c_updn <- ggplot(avg_kelp%>%filter(Treatment %in% c("Caged","Control")), 
                   aes(x = Treatment, y = updn_avg_pcnt_change,fill = Position)) +
  
  geom_bar(stat = "identity", 
           position = position_dodge()) +
  
  geom_errorbar(aes(ymin = updn_avg_pcnt_change-updn_se_pcnt_change, 
                    ymax = updn_avg_pcnt_change+updn_se_pcnt_change),
                position = position_dodge())+
  
  labs(x = "Predator treatment", y = "% kelp grazed", 
       fill = "Upstream/downstream") + #maybe this isn't the best  - it shows variability in the raw data, but idk
  
  theme_classic() +
  scale_fill_manual(values = c('#fcbba1','#5A5A5A') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Upstream/downstream of cage")

c_c_updn
#caged and control mean

c_c <- ggplot(avg_kelp%>%filter(Treatment %in% c("Caged","Control")), 
              aes(x = Treatment, y = avg_pcnt_change,fill = Treatment)) +
  
  geom_bar(stat = "identity", 
           position = position_dodge()) +
  
  geom_errorbar(aes(ymin = avg_pcnt_change-se_pcnt_change, 
                    ymax = avg_pcnt_change+se_pcnt_change),
                position = position_dodge())+
  
  labs(x = "Predator treatment", y = "% kelp grazed", 
       fill = "Upstream/downstream of cage",
       caption = "Error bars show 1 SE from mean") + #maybe this isn't the best  - it shows variability in the raw data, but idk
  
  theme_classic() +
  
  scale_fill_manual(values = c('#bcbddc','#5A5A5A') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Predator treatment")

c_c

#test plot - caged upstream vs. 

a_c / (c_c_updn + c_c) +
  plot_annotation(tag_levels = 'A')


#active vs.control 

ggplot(predicted_mod_1b, 
       aes(x = Treatment, y = predicted, fill = Treatment)) +
  
  geom_bar(stat = "identity", 
           position = position_dodge()) +
  
  geom_errorbar(aes(ymin = predicted-std.error, ymax =predicted+std.error), 
                position=position_dodge()) +
  
  labs(title = "Predicted values of % kelp grazed from GLMM", 
       x = "Predator treatment", y = "% kelp grazed", 
       fill = "Predator treatment",
       caption = "Note: error bars are made with respect to SE of model") + 
  
  theme_classic() +
  scale_fill_manual(values = c('#DAF7A6','#5A5A5A') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Predator treatment")

#caged and control upstream vs. downstream

ggplot(predicted_mod_1a.1, 
       aes(x = Treatment, y = predicted, fill = Position)) +
  
  geom_bar(stat = "identity", 
           position = position_dodge()) +
  
  labs(title = "Predicted values of % kelp grazed from GLMM", 
       x = "Predator treatment", y = "% kelp grazed", 
       fill = "Upstream/downstream of cage",
       caption = "Note: error bars are made with respect to SD and mean of raw data") + #maybe this isn't the best  - it shows variability in the raw data, but idk
  
  theme_classic() +
  scale_fill_manual(values = c('#DAF7A6','#5A5A5A') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Predator treatment")

###########

#active vs.control 

a_c <- ggplot(avg_kelp%>%filter(Treatment %in% c("Active","Control")), 
              aes(x = Treatment, y = avg_pcnt_change, fill = Treatment)) +
  
  geom_bar(stat = "identity", 
           position = position_dodge()) +
  
  geom_errorbar(aes(ymin = avg_pcnt_change-se_pcnt_change, 
                    ymax = avg_pcnt_change+se_pcnt_change),
                position = position_dodge(),
                width = 0.1) +
  
  geom_signif(comparisons = list(c("Active", "Control")),
              map_signif_level = TRUE) +
  
  labs(title = "Kelp grazed by urchins under different predator activity levels", 
       x = "Predator treatment", y = "% kelp grazed", 
       fill = "Predator treatment") + 
  
  theme_classic() +
  scale_fill_manual(values = c('#DAF7A6','#5A5A5A') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Predator treatment")

a_c

#caged and control upstream vs. downstream

c_c_updn <- ggplot(avg_kelp%>%filter(Treatment %in% c("Caged","Control")), 
                   aes(x = Treatment, y = updn_avg_pcnt_change,fill = Position)) +
  
  geom_bar(stat = "identity", 
           position = position_dodge()) +
  
  geom_errorbar(aes(ymin = updn_avg_pcnt_change-updn_se_pcnt_change, 
                    ymax = updn_avg_pcnt_change+updn_se_pcnt_change),
                position = position_dodge())+
  
  geom_signif(y_position = c(33,27), xmin = c(0.8,1.8), 
              xmax = c(1.2,2.2), annotation = c("***","NS"),
              tip_length = 0) +
  
  labs(x = "Predator treatment", y = "% kelp grazed", 
       fill = "Upstream/downstream") + 
  
  theme_classic() +
  scale_fill_manual(values = c('#fcbba1','#5A5A5A') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Upstream/downstream of cage")

c_c_updn


#caged and control mean

c_c <- ggplot(avg_kelp%>%filter(Treatment %in% c("Caged","Control")), 
              aes(x = Treatment, y = avg_pcnt_change,fill = Treatment)) +
  
  geom_bar(stat = "identity", 
           position = position_dodge()) +
  
  geom_errorbar(aes(ymin = avg_pcnt_change-se_pcnt_change, 
                    ymax = avg_pcnt_change+se_pcnt_change),
                position = position_dodge(),
                width = 0.1)+
  
  geom_signif(comparisons = list(c("Caged", "Control")),
              map_signif_level = TRUE,
              annotation = c("NS")) +
  
  labs(x = "Predator treatment", y = "% kelp grazed", 
       fill = "Upstream/downstream of cage",
       caption = "Error bars show 1 SE from mean") + #maybe this isn't the best  - it shows variability in the raw data, but idk
  
  theme_classic() +
  
  scale_fill_manual(values = c('#bcbddc','#5A5A5A') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Predator treatment")

c_c

#test plot - caged upstream vs. 

a_c / ( c_c + c_c_updn) +
  plot_annotation(tag_levels = 'A')


#final plots but w  letters for signif
#reordering variables to show up on x axis
avg_kelp_wholetank$Treatment <- factor(avg_kelp_wholetank$Treatment, 
                                       levels=c('Control', 'Caged', 'Active'))
avg_kelp_updn$Treatment <- factor(avg_kelp_updn$Treatment, 
                                  levels=c('Control', 'Caged', 'Active'))
avg_kelp_updn$Position <- factor(avg_kelp_updn$Position, levels=c('Upstream', 'Downstream'))

#for up/dn graph, adding identifier var of treatment_updn to color individual bars eventually
avg_kelp_updn$unique_treatment <- paste(avg_kelp_updn$Treatment, 
                                        avg_kelp_updn$Position, sep = "_")
avg_kelp_updn$unique_treatment <- as.factor(avg_kelp_updn$unique_treatment)
avg_kelp_updn$unique_treatment <- factor(avg_kelp_updn$unique_treatment, 
                                         levels=c('Control_Upstream','Control_Downstream',
                                                  'Caged_Upstream','Caged_Downstream',
                                                  'Active_Upstream','Active_Downstream'))


fulltank <- 
  ggplot(avg_kelp_wholetank, 
         aes(x = Treatment, y = avg_weight_diff, fill = Treatment)) +
  
  geom_col(position = position_dodge()) +
  
  geom_errorbar(aes(ymin = avg_weight_diff-se_weight_diff, 
                    ymax = avg_weight_diff+se_weight_diff),
                position = position_dodge(),
                width = 0.1) +
  
  annotate("text", x = 1, y = 5.5, label = "a", 
           color = "#636363", size = 3.5) +  
  annotate("text", x = 2, y = 5.45, label = "a", 
           color = "#636363", size = 3.5) + 
  annotate("text", x = 3, y = 4.2, label = "b", 
           color = "#636363", size = 3.5) +
  coord_cartesian(ylim = c(0,5.1), clip = "off") +
  
  
  labs(title = "Kelp grazed across the entire tank\n", 
       x = "Predator treatment",
       fill = "Predator treatment") + 
  
  theme_classic() +
  scale_fill_manual(values = c('#5A5A5A','#b3cde0', "#fcbba1") ) +
  ylab("Mean difference in kelp weight (g)") +
  labs(fill = "Predator treatment") 


# define dodge width (must match your geom_bar)
dw <- 0.9
off <- dw / 2

updn <- 
  ggplot(avg_kelp_updn, 
         aes(x = as.numeric(Treatment), 
             y = updn_avg_weight_diff, 
             fill = unique_treatment)) +
  
  geom_col(position = position_dodge(width = dw)) +
  
  geom_errorbar(aes(ymin = updn_avg_weight_diff-updn_se_weight_diff, 
                    ymax = updn_avg_weight_diff+updn_se_weight_diff),
                position = position_dodge(width = dw),
                width = 0.1)+
  
  
  #significance from tukey letters
  annotate("text", x = 0.74, y = 7, label = "a", 
           color = "#636363", size = 3.5) +  
  annotate("text", x = 0.84, y = 7, label = "b", 
           color = "#636363", size = 3.5) +
  annotate("text", x = 1.18, y = 5.3, label = "b", 
           color = "#636363", size = 3.5) +  
  annotate("text", x = 1.28, y = 5.3, label = "c", 
           color = "#636363", size = 3.5) +
  annotate("text", x = 1.78, y = 7.7, label = "a", 
           color = "#636363", size = 3.5) +
  annotate("text", x = 2.23, y = 4.65, label = "c", 
           color = "#636363", size = 3.5) +
  annotate("text", x = 2.76, y = 4.4, label = "c", 
           color = "#636363", size = 3.5) +
  annotate("text", x = 3.16, y = 5.1, label = "b", 
           color = "#636363", size = 3.5) +
  annotate("text", x = 3.26, y = 5.1, label = "c", 
           color = "#636363", size = 3.5) +
  
  geom_bracket(
    xmin = c("Control", "B"),       # X-position for the left end of the bracket
    xmax = c("B", "C"),       # X-position for the right end of the bracket
    label = c("**", "n.s."),  # Manual labels for each bracket
    y.position = c(8, 8.5)) +
  
  labs(x = "Predator treatment", 
       fill = "Position") + 
  
  ##5A5A5A','#D3D3D3','#005b96','#b3cde0','#9d5600','#fcbba1
  theme_classic() +
  scale_fill_manual(labels = c("Control upstream","Control downstream","Caged upstream","Caged downstream","Active upstream","Active downstream"), values = c('#5A5A5A','#D3D3D3','#005b96','#b3cde0','#9d5600','#fcbba1') )  +
  ylab(" ") +
  labs(fill = "Position in relation to cage",
       title = "Kelp grazed across tank segments\n") 


fulltank / updn

#this was final plot but w letters as significance - changing to bars for ms
#bar chart 
ggplot(kelp_processed, 
       aes(x = Urch_habitat_treatment, y = weight_diff_mean, 
           fill = Pred_treatment)) + 
  
  geom_bar(stat = "identity", 
           position = position_dodge(width=0.9)) +
  
  geom_errorbar(aes(ymin = weight_diff_mean-weight_diff_se, 
                    ymax =weight_diff_mean+weight_diff_se),
                position= position_dodge(width=0.9),
                width = 0.1) +
  
  labs(title = "Kelp grazed by urchins from different source habitats", 
       x = "Urchin habitat", y = "Mean difference in kelp weight (g)", #specify across the 12 blades in a tank
       fill = "Pred treatment") +
  
  theme_classic() +
  scale_fill_manual(values = c('#5A5A5A','#c994c7')) +
  ylab("Mean difference in kelp weight (g)") +
  scale_x_discrete(labels = c("Barren","Kelp forest")) +
  labs(fill = "Predator treatment") +
  
  annotate("text", x = 0.78, y = 9.7, label = "a", 
           color = "#636363", size = 4) +
  annotate("text", x = 1.23, y = 5, label = "b", 
           color = "#636363", size = 4) +
  annotate("text", x = 1.78, y = 1.4, label = "c", 
           color = "#636363", size = 4) +
  annotate("text", x = 2.23, y = 0.5, label = "c", 
           color = "#636363", size = 4)



