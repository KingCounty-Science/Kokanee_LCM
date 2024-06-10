#=== === === === === === === ===
# Script started by Rebekah Stiling, Bailey Keeler, and Jim Bower Spring 2024
# This is the beginning of a Kokanee lifecycle model
# rstiling@kingcounty.gov, bkeeler@kingcounty.gov
#=== === === === === === === ===

#lists based on observations
hat_egg_surv_list <-c(.9, .9, .9, .9, .9, .5, .8) #Jim estimate. changed 1s to .9 
percent_female_list <-c(0.29, 0.28, 0.49, 0.26, 0.39, 0.41, 0.34 ,0.36, 0.30, 0.32, 0.37, 0.34) #spreadsheet
nat_egg_surv_min <- 0.015 #lower bound of 95% CI #0.015
nat_egg_surv_max <- 0.176 #upper bound of 95% CI #0.176
n_nat_eggs_start <- 120000 #Estimate for how many eggs are available with 300 spawners
n_hat_eggs_start <- 7500
nat_fry_to_spawn_survival <- .0175 #.0175 #1.75% geometric mean from jim 2009-2018 
hat_fry_to_spawn_survival <- .0005 #0.05% geometric mean from jim 2009-2018 
# nat_brood_to_spawn_2 <- c(.04, 0.03, 0.05) prep to build randomness in the future
portion_nat_brood_to_spawn_age <- c(.0434, .7405, .2123, .0038) #average values for a composition that sums to 1, to estimate the percent of brood that later returned as spawners from 2009-2018
portion_hat_brood_to_spawn_age <- c(.0782, .8842, .0376, 0) 

portion_spawner_to_hatch = NA
max_num_spawners = 300
portion_spawner_to_hatch_low_year = .1 
portion_spawner_to_hatch_high_year= .05
loss_to_disease = 0 #write the number of portion lost 
fecundity_threshold = 1000 #if over this number, then average fecundity is lower
high_fecundity = 1200
low_fecundity = 900

years = 50 #how many years would we like to run the model
runs = 100 # a run is going through the cycle for the number of years desired. how many runs do we do Goal: 1000

grand_df <- matrix(data = NA, nrow = years+4, ncol = runs+1) #because outputs are placed 2-5 years into the future, the loop needs to extend 4 years past the desired length so future predicions have a place to go. 

grand_df[,1] <- 1:(years+4)

for(j in 1:runs) {
## Create an empty matrix for filling in a loop 
#natural fish df
nat_df <- matrix(data = NA, nrow = years + 4, ncol = 8)
colnames(nat_df) <- c("year","2yo", "3yo","4yo","5yo", "sum_spawn_rets", "sum_spawn", "eggs")

#hatchery fish df
hat_df <- matrix(data = NA, nrow = years + 4, ncol = 8)
colnames(hat_df) <- c("year", "2yo", "3yo","4yo","5yo", "sum_spawn_rets", "sum_spawn", "eggs")

for(i in 1:years) 
{
  if(i < 5) { 
  #egg to fry ####
  n_nat_eggs <- n_nat_eggs_start #For the first 4 years, we use seed numbers for the eggs to estimate future spawners.
  n_hat_eggs <- n_hat_eggs_start
  } else {
    n_nat_eggs  # but after the first four years, the number of eggs is determined by the number of fish spawning
    n_hat_eggs 
  }
  
  nat_egg_surv <- runif(n = 1, min = nat_egg_surv_min, max = nat_egg_surv_max) # survival is pulled from a range of possibilities
  hat_egg_surv <- sample(x = hat_egg_surv_list, size = 1, replace = TRUE) # hatch survival is pulled from a list of past events
  n_nat_fry <- n_nat_eggs*nat_egg_surv
  n_hat_fry <- n_hat_eggs*hat_egg_surv
  
  #fry to lake survival to transition to spawner by ages ####
  
  ## natural org fish ####
  n_nat_spawner <- n_nat_fry*nat_fry_to_spawn_survival
  
  n_nat_year_2_spawners<-n_nat_spawner*portion_nat_brood_to_spawn_age[1]
  n_nat_year_3_spawners<-n_nat_spawner*portion_nat_brood_to_spawn_age[2]
  n_nat_year_4_spawners<-n_nat_spawner*portion_nat_brood_to_spawn_age[3]
  n_nat_year_5_spawners<-n_nat_spawner*portion_nat_brood_to_spawn_age[4]
  
  nat_df[i,"year"] <- i
  nat_df[i+1,"2yo"] <- n_nat_year_2_spawners
  nat_df[i+2,"3yo"] <- n_nat_year_3_spawners
  nat_df[i+3,"4yo"] <- n_nat_year_4_spawners
  nat_df[i+4,"5yo"] <- n_nat_year_5_spawners
  nat_df[i,"sum_spawn_rets"] <- sum(nat_df[i,2:5], na.rm = TRUE)

  ## hatchery fish ####
  n_hat_spawner <- n_hat_fry*hat_fry_to_spawn_survival
  
  n_hat_year_2_spawners<-n_hat_spawner*portion_hat_brood_to_spawn_age[1]
  n_hat_year_3_spawners<-n_hat_spawner*portion_hat_brood_to_spawn_age[2]
  n_hat_year_4_spawners<-n_hat_spawner*portion_hat_brood_to_spawn_age[3]
  n_hat_year_5_spawners<-n_hat_spawner*portion_hat_brood_to_spawn_age[4]
  
  hat_df[i,"year"] <- i
  hat_df[i+1,"2yo"] <- n_hat_year_2_spawners
  hat_df[i+2,"3yo"] <- n_hat_year_3_spawners
  hat_df[i+3,"4yo"] <- n_hat_year_4_spawners
  hat_df[i+4,"5yo"] <- n_hat_year_5_spawners
  hat_df[i,"sum_spawn_rets"] <- sum(hat_df[i,2:5], na.rm = TRUE)
  
  if(i < 5) { 
    next } # this tells the code to go back to the top to the next iteration, don't do any more of the script. It only does this for iterations 1-4 while getting the first row full population of spawners established. Otherwise it carries on with the rest.
  else 
    {
  #total the spawners returning for the next round
  nat_spawner_rets <- nat_df[i,"sum_spawn_rets"]

  #total the spawners returning for the next round
  hat_spawner_rets <- hat_df[i,"sum_spawn_rets"]
  
  # spawner to egg ####
  #total spawners that returned
  total_spawners <- nat_spawner_rets + hat_spawner_rets
  
  # spawner loss to disease 
  healthy_spawners = total_spawners*(1-loss_to_disease)
  
  if (healthy_spawners < max_num_spawners) {
    portion_spawner_to_hatch = portion_spawner_to_hatch_low_year
  } else {(portion_spawner_to_hatch = portion_spawner_to_hatch_high_year) }
  
  ## spawners -split into portions to use later ####
  hat_spawners <- portion_spawner_to_hatch * healthy_spawners 
  nat_spawners <- healthy_spawners - hat_spawners
  hat_df[i,"sum_spawn"] <- hat_spawners
  nat_df[i,"sum_spawn"] <- nat_spawners
  
  #percent female and fecundity
  percent_female <- sample(x = percent_female_list, size = 1, replace = TRUE)
  
  if (healthy_spawners < fecundity_threshold) {
    fecundity = high_fecundity
  } else  {(fecundity = low_fecundity) }
  
  #natural spawners to eggs
  nat_female_spawners <- nat_spawners*percent_female
  n_nat_eggs_1 <-nat_female_spawners*fecundity
  
  nat_df[i,"eggs"]<- n_nat_eggs_1 #store the number of eggs in the data frame
  n_nat_eggs <- n_nat_eggs_1 # store the number of eggs as the name for starting the next iteration
  
  #hatchery spawners to eggs
  hat_female_spawners <- hat_spawners*percent_female
  n_hat_eggs_1 <-hat_female_spawners*fecundity
  
  hat_df[i,"eggs"]<- n_hat_eggs_1
  n_hat_eggs <- n_hat_eggs_1
  
    }
  

  grand_df[i, j+1] <- healthy_spawners

}

if (j == 1) {
  plot(x = grand_df[5:(years),1], 
       y = grand_df[5:(years),j+1], type = "l",
       xlab = "years", 
       ylab = "healthy spawners",
       ylim = c(0, 1000),
       col = "gray") 
} else { 
  lines(x = grand_df[5:(years),1],
        y = grand_df[5:(years),1+j],
        col= "gray")
} 


}  

lines(x = grand_df[5:(years),1],
      y = rowMeans(grand_df[,-1], na.rm = TRUE)[5:(years)])


#grand_df[i, j] 
# plot(x = grand_df[5:(years-4),1], 
#      y = grand_df[5:(years-4),2], type = "l") 
# lines(grand_df[5:(years-4),1],
#       y = grand_df[5:(years-4),3])
# lines(grand_df[5:(years-4),1],
#       y = grand_df[5:(years-4),4])



# 
# # Plot last iteration #####
# par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
# plot(x = nat_df[5:(years-5),1], y = nat_df[5:(years-5),"sum_spawn"], xlab = "years", ylab = "natural spawners")
# plot(x = hat_df[5:(years-5),1], y = hat_df[5:(years-5),"sum_spawn"], xlab = "years", ylab = "hatchery spawners")
# plot(x = (nat_df[-(1:4),"year"])-4, y = nat_df[-(1:4),"eggs"], xlab = "years", ylab = "natural eggs")
# plot(x = (hat_df[-(1:4),"year"])-4, y = hat_df[-(1:4),"eggs"], xlab = "years", ylab = "hatchery eggs")
# par(mfrow=c(1,1)) # Change back to 1 x 1

round(nat_df)
round(hat_df)
