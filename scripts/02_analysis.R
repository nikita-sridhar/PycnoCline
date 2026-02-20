############
#load files 
############
#should already be loaded if you ran  01_manipulation first
#kelp <- read_csv("data/processed data/manipulated data/kelp.csv")


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


#calculation of predation rate: 
#total number of urchins eaten / number of urchins in active treatment:
#this was calculated manually based on notes written of when urchins eaten

#% of urchins consumed
4/(24*14)*100 #14 total active treatments

#predation rate: two instances of predation
#pred1 - 1 urchin eaten by 1 star in 57.17 hours (trial duration)
p1 <- 1/57.17  
#pred2 - 3 urchins eaten by 1 star in 57.17 hours (trial duration)
p2 <- 3/57.17 
#mean pred rate
mean(c(p1,p2))
