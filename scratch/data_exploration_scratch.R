#figures for caged vs. control look identical. compare raw data (completely raw + versus after cleanup)

kelp_intermediary1 <- kelp_raw %>%
  mutate(weight_pcnt_change = ((Kelp_weight_before_g - Kelp_weight_after_g)/
                                 Kelp_weight_before_g)*100 )

kelp_intermediary2 <- kelp

#between kelp_intermdiary1 and 2, removed pilot and trial 2, added some pycno columns, 
#^^ there should only be a 20 row difference btwn kelp_intermediary 1 and 2 
#(number of rows of pilot + trial 2)


#histogram
ggplot(kelp_intermediary1, aes(x=weight_pcnt_change))+
  geom_histogram() +
  facet_wrap(vars(Treatment))

ggplot(kelp_intermediary2, aes(x=weight_pcnt_change))+
  geom_histogram() +
  facet_wrap(vars(Treatment))


#model
