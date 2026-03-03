
#calculation of predation rate: 
#total number of urchins eaten / number of urchins in active treatment:
#this was calculated manually based on notes written of when urchins eaten

#% of urchins consumed
4/(24*14)*100 #14 total active treatments, 24 urchins per tank, 4 urchins consumed (from notes)

#predation rate: two instances of predation
#pred1 - 1 urchin eaten by 1 star in 57.17 hours (trial duration)
p1 <- 1/57.17  
#pred2 - 3 urchins eaten by 1 star in 57.17 hours (trial duration)
p2 <- 3/57.17 
#mean pred rate
mean(c(p1,p2))
