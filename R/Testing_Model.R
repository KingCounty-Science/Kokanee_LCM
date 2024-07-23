#Bailey Playing with Model by inputting spawner estimates from index stream surveyes and seeing what #s are after running model for 10years. Comparing 2009 to 2010 data since 2009 had high spawner estimates and 2010 had low. Running both out to 2023 and see if they lead to similar means. 
#Starting eggs based off year 2009 data: 101 Hatchery spawners. 2838 natural spawners. Fecundity low: 900. Natural eggs start: 2554200. Hatchery eggs start: 90900
#starting eggs based off year 2010 data: 33 hatchery spawners. 46 natural spawners. Fecundity high: 1200. natural eggs start: 55200. Hatchery eggs start: 39600

#List of scenarios
scenarios <- c("sc1.0") #change to run only 1 scenario 
years = 30 #how many years would we like to run the model
runs = 1000 # a run is going through the cycle for the number of years desired. how many runs do we do Goal: 1000

#create a data frame that will store all the returner numbers
returner_df <- matrix(data = NA, nrow = years, ncol = length(scenarios)+1) #this is for storing the average output of each scenario, the +1 allows for a col named "years"
colnames(returner_df) <- c("year",scenarios) #name the columns

returner_df[,1] <- 1:years #populate the first column of the df with the number of years, plus

for(k in 1:length(scenarios)) {
  scen <- scenarios[k]
  
  #lists based on observations ####
  hat_egg_surv_list <-c(.9, .9, .9, .9, .9, .5, .8) #Jim estimate. changed 1s to .9 
  nat_egg_surv_min <- 0.015 #lower bound of 95% CI #0.015
  nat_egg_surv_max <- 0.176 #upper bound of 95% CI #0.176
  n_nat_eggs_start <- 120000 
  n_hat_eggs_start <- 7500
  nat_fry_to_spawn_survival <- .0197 #1.97% geometric mean from jim 2009-2019 updated 6/10/24
  hat_fry_to_spawn_survival <- .0006 #0.06% geometric mean from jim 2009-2019 updated 6/10/24
  # nat_brood_to_spawn_2 <- c(.04, 0.03, 0.05) prep to build randomness in the future
  portion_nat_brood_to_spawn_age <- c(.0596, .6818, .2552, .0034) #average values for a composition that sums to 1, to estimate the percent of brood that later returned as spawners from 2009-2019. updated 5/29/24
  portion_hat_brood_to_spawn_age <- c(.1091, .7757, .1151, 0) #updated 6/10/24
  
  portion_spawner_to_hatch = NA
  max_num_spawners = 300
  carry_capacity = 22500
  portion_spawner_to_hatch_low_year_list <- c(0, .05, .1, .15) #list probabilities to capture variability seen in data. Will randomly select from this list.  
  portion_spawner_to_hatch_high_year= .05 #when max_num_spawners >300, always pull 0.05 fish (5%). Jim pondering this to incorporate hatchery capacity. 
  percent_female_list <-c(0.29, 0.28, 0.49, 0.26, 0.39, 0.41, 0.34 ,0.36, 0.30, 0.32, 0.37, 0.34) #spreadsheet
  loss_to_disease = 0 #write the number of portion lost 
  fecundity_threshold = 1000 #if over this number, then average fecundity is lower
  high_fecundity = 1200
  low_fecundity = 900
  
  ## alterations due to scenarios ####
  if (scen == "sc2.1" | scen == "A" | scen == "B" | scen == "C"  ) {
    hat_fry_to_spawn_survival <- nat_fry_to_spawn_survival #as the numeric value for hatchery-only fry to adult survival (show improved lake survival rate for hatchery fry : adult equal to natural fry : adult survival rate). 
  } 
  if (scen == "sc2.2"){
    hat_fry_to_spawn_survival <- nat_fry_to_spawn_survival*.5 # the as the numeric value for hatchery-only fry to adult survival (show improved lake survival rate for hatchery fry : adult half the natural fry : adult survival rate). 
  }   
  if (scen == "sc2.3"){
    hat_fry_to_spawn_survival <- nat_fry_to_spawn_survival*2 # the as the numeric value for hatchery-only fry to adult survival (show improved lake survival rate for hatchery fry : adult double the natural fry : adult survival rate). 
  } 
  if (scen == "sc3.1"| scen == "A" | scen == "D"| scen == "E"  ){
    portion_spawner_to_hatch_low_year_list <- portion_spawner_to_hatch_low_year_list*2 # double values in the list compared to sc1.0; keep zero
    portion_spawner_to_hatch_high_year= portion_spawner_to_hatch_high_year*2 #double value to .1 
  }
  if (scen == "sc3.2"){
    portion_spawner_to_hatch_low_year_list <- portion_spawner_to_hatch_low_year_list*3 # triple values in the list compared to sc1.0; keep zero
    portion_spawner_to_hatch_high_year= portion_spawner_to_hatch_high_year*3 #triple value to .15 
  }
  if (scen == "sc4.1"| scen == "B" | scen == "D" | scen == "F"  ){
    nat_egg_surv_min <- nat_egg_surv_min + 0.02 #increase survival by + 2%
    nat_egg_surv_max <- nat_egg_surv_max + 0.02 #increase survival by + 2%
  }
  if (scen == "sc4.2"){
    nat_egg_surv_min <- nat_egg_surv_min + 0.04 #increase survival by + 4%
    nat_egg_surv_max <- nat_egg_surv_max + 0.04 #increase survival by + 4%
  }
  if (scen == "sc5.1"| scen == "C"| scen == "E" | scen == "F" ){
    nat_fry_to_spawn_survival <- nat_fry_to_spawn_survival*2 #doubling status quo natural fry to adult survival
  }
  if (scen == "sc5.2"){
    nat_fry_to_spawn_survival <- nat_fry_to_spawn_survival*3 #triple status quo natural fry to adult survival
  }
  
  grand_df <- matrix(data = NA, nrow = years+4, ncol = runs+1) #because outputs are placed 2-5 years into the future, the loop needs to extend 4 years past the desired length so future predictions have a location on the matrix to go. Without these extra rows the model gets an error towards the last year.
  
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
      
      ## natural org fish fry to spawn ####
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
      
      ## hatchery fish fry to spawn ####
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
        
        #carrying capacity ####
        if (total_spawners > carry_capacity) {
          total_spawners <- carry_capacity
        }
        
        # spawner loss to disease 
        healthy_spawners = total_spawners*(1-loss_to_disease)
        
        # determine what portion of spawners will go to the hatchery if it is a low return year
        portion_spawner_to_hatch_low_year <- sample(portion_spawner_to_hatch_low_year_list, replace = TRUE, size=1)
        
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
      
    } #end i loop
    
    #plot based on manually set y-axis range
    if (j == 1) {
      plot(x = grand_df[5:(years),1], 
           y = grand_df[5:(years),j+1], type = "l",
           xlab = "years", 
           ylab = "healthy spawners",
           ylim = c(0, 500),   #set y-axis manually here
           col = "gray") 
    } else { 
      lines(x = grand_df[5:(years),1],
            y = grand_df[5:(years),1+j],
            col= "gray")
    } 
    
    
  }  #end j loop
  
  lines(x = grand_df[5:(years),1],
        y = rowMeans(grand_df[,-1], na.rm = TRUE)[5:(years)])
  
  #establish top of range in data fram from model output 
  top_of_range <- max(grand_df, na.rm = TRUE)
  
  grand_df_plot_filename <- paste0("Output/plot_mean_spawners_",scen, ".tiff")
  tiff(filename = grand_df_plot_filename, width = 6, height = 6, units = "in", pointsize = 10, res = 400, family = "sans", compression = "lzw")
  
  par(mar = c(5,5,2,2))
  
  #plot based on model output range 
  plot(x = grand_df[5:nrow(grand_df)],
       y = grand_df[5:nrow(grand_df),2],
       type = "l",
       xlab = "years", 
       ylab = "healthy spawners",
       ylim = c(0, top_of_range),
       col = "gray")
  for(i in c(3:dim(grand_df)[2])){
    lines(x = grand_df[5:nrow(grand_df)],
          y = grand_df[5:nrow(grand_df), i],
          col = "gray")
  }
  
  lines(x = grand_df[5:(years),1],
        y = rowMeans(grand_df[,-1], na.rm = TRUE)[5:(years)])
  
  dev.off()
  
  #save the grand_df
  grand_df_filename <- paste0("Output/mean_spawners_",scen, ".csv")
  write.csv(grand_df, grand_df_filename)
  
  returner_df[5:(years),k+1] <- rowMeans(grand_df[,-1], na.rm = TRUE)[5:(years)]
  
} #end k loop

#save the returner_df
returner_df_filename <- paste0("Output/overview_mean_spawners.csv")
write.csv(returner_df, returner_df_filename)

#establish top of range in data fram from model output 
top_of_range_returner <- max(returner_df, na.rm = TRUE)

tiff(filename = "Output/returner_runs.tiff", width = 6, height = 6, units = "in", pointsize = 10, res = 400, family = "sans", compression = "lzw")

par(mar = c(5,5,2,2))

#plot based on model output range 
plot(x = returner_df[5:nrow(returner_df), 1],
     y = returner_df[5:nrow(returner_df),2],
     type = "l",
     xlab = "years", 
     ylab = "healthy spawners",
     ylim = c(0, top_of_range_returner),
     col = "gray")
for(i in c(3:dim(returner_df)[2])){
  lines(x = returner_df[5:nrow(returner_df), 1],
        y = returner_df[5:nrow(returner_df), i],
        col = "gray")
}


dev.off()

