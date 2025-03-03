#=== === === === === === === ===
# Script started by Rebekah Stiling, Bailey Keeler, and Jim Bower Spring 2024
# This is the Kokanee lifecycle model it goes through a status quo cycle then builds on that with addtional scenarios informed by managment goals
# rstiling@kingcounty.gov, bkeeler@kingcounty.gov
#=== === === === === === === ===

set.seed(9) #mia hamm's jersey number. So that plots remain the same

#List of scenarios
scenarios <- c("sc1.0", 
               "sc2.1", "sc2.2", "sc2.3", 
               "sc3.1", "sc3.2", 
               "sc4.1", "sc4.2",
               "sc5.1", "sc5.2",
               "A", "B", "C", "D", "E", "F")

#lists of estimated percent of NAT brood that returns as different spawning ages
portion_nat_brood_to_spawn_age_INPUT <- list(a= c(0, .563, .437, 0), b= c(0, .999, .001, 0), c= c(0.000,	0.545, 0.455, 0.000), d= c(0.000,	0.480, 0.520, 0.000), e= c(0.035,	0.955, 0.010, 0.000), f= c(0.338,	0.584, 0.078,	0.000), g= c(0.000,	1.000, 0.000,	0.000), h= c(0.006,	0.716, 0.278, 0.000), i= c(0.000,	0.914,	0.049, 0.037), j= c(0.000, 0.071,	0.929, 0.000), k= c(0.001, 0.999,	0.000, 0.000), l= c(0.000, 0.000,	1.000,	0.000))

#lists of estimated percent of HAT brood that returns as different spawning ages
portion_hat_brood_to_spawn_age_INPUT <- list(a= c(0.00,	1.00,	0.00,	0.00), b= c(0.00,	0.96,	0.04,	0.00), c= c(0.05,	0.77,	0.18,	0.00), d= c(0.23,	0.77,	0.00,	0.00), e= c(0.00, 1.00,	0.00,	0.00), f= c(0.19,	0.81,	0.00,	0.00), g= c(0.30,	0.13,	0.58,	0.00))

#model length and iterations
years = 50 #how many years would we like to run the model
runs = 1000 # a run is going through the cycle for the number of years desired. how many runs do we do Goal: 1000

#create a data frame that will store all the returner numbers as means
returner_df <- matrix(data = NA, nrow = years, ncol = length(scenarios)+1) #this is for storing the average output of each scenario, the +1 allows for a col named "years"
colnames(returner_df) <- c("year",scenarios) #name the columns
returner_df[,1] <- 1:years #populate the first column of the df with the number of years, plus

#create a data frame that will store all the returner numbers as medians
returner_df_median <- matrix(data = NA, nrow = years, ncol = length(scenarios)+1) #this is for storing the average output of each scenario, the +1 allows for a col named "years"
colnames(returner_df_median) <- c("year",scenarios) #name the columns
returner_df_median[,1] <- 1:years #populate the first column of the df with the number of years, plus

#create a data frame that will store all the returner numbers as the standard deviation for calculating 95% confidence interval
returner_df_sd <- matrix(data = NA, nrow = years, ncol = length(scenarios)+1) #this is for storing the average output of each scenario, the +1 allows for a col named "years"
colnames(returner_df_sd) <- c("year",scenarios) #name the columns
returner_df_sd[,1] <- 1:years #populate the first column of the df with the number of years, plus


for(k in 1:length(scenarios)) {
  scen <- scenarios[k]
  
  #lists based on observations ####
  hat_egg_surv_list <-c(.9, .9, .9, .9, .9, .5, .8) #Jim estimate. changed 1s to .9 
  nat_egg_surv_min <- 0.015 #lower bound of 95% CI #0.015
  nat_egg_surv_max <- 0.176 #upper bound of 95% CI #0.176
  n_nat_eggs_start <- 120000 #Estimate for how many eggs are available with 300 spawners
  n_hat_eggs_start <- 7500
  nat_fry_to_adult_survival <- .0197 #1.97% geometric mean from jim 2009-2019 updated 6/10/24
  hat_fry_to_adult_survival <- .0006 #0.06% geometric mean from jim 2009-2019 updated 6/10/24
 
  portion_spawner_to_hatch = NA
  max_num_spawners = 300
  maximum_spawner_capacity = 22500
  portion_spawner_to_hatch_low_year_list <- c(0, .05, .1, .15) #list probabilities to capture variability seen in data. Will randomly select from this list.  
  portion_spawner_to_hatch_high_year= .05 #when max_num_spawners >300, always pull 0.05 fish (5%). Jim pondering this to incorporate hatchery capacity. 
  percent_female_list <-c(0.29, 0.28, 0.49, 0.26, 0.39, 0.41, 0.34 ,0.36, 0.30, 0.32, 0.37, 0.34) #spreadsheet
  loss_to_disease = 0 #write the number of portion lost 
  fecundity_threshold = 1000 #if over this number, then average fecundity is lower
  high_fecundity = 1200
  low_fecundity = 900
  
  ## alterations due to scenarios ####
  if (scen == "sc2.1" | scen == "A" | scen == "B" | scen == "C"  ) {
    hat_fry_to_adult_survival <- nat_fry_to_adult_survival #as the numeric value for hatchery-only fry to adult survival (show improved lake survival rate for hatchery fry : adult equal to natural fry : adult survival rate). 
  } 
  if (scen == "sc2.2"){
    hat_fry_to_adult_survival <- nat_fry_to_adult_survival*.5 # the as the numeric value for hatchery-only fry to adult survival (show improved lake survival rate for hatchery fry : adult half the natural fry : adult survival rate). 
  }   
  if (scen == "sc2.3"){
    hat_fry_to_adult_survival <- nat_fry_to_adult_survival*2 # the as the numeric value for hatchery-only fry to adult survival (show improved lake survival rate for hatchery fry : adult double the natural fry : adult survival rate). 
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
    nat_fry_to_adult_survival <- nat_fry_to_adult_survival*2 #doubling status quo natural fry to adult survival
  }
  if (scen == "sc5.2"){
    nat_fry_to_adult_survival <- nat_fry_to_adult_survival*3 #triple status quo natural fry to adult survival
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
      n_nat_adult <- n_nat_fry*nat_fry_to_adult_survival
      
      #sample from list of value compositions that sum to 1. each sample gives a percent of brood that later returned as spawners in different age classes(2yr-5yr). Estimates from Jim Data. updated 7/24/2024. 
      portion_nat_brood_to_spawn_age <- sample(portion_nat_brood_to_spawn_age_INPUT, 1)
      #sample from list of value compositions that sum to 1. each sample gives a percent of brood that later returned as spawners in different age classes(2yr-5yr). Estimates from Jim Data. updated 7/24/2024. 
      portion_hat_brood_to_spawn_age <- sample(portion_hat_brood_to_spawn_age_INPUT, 1)
      
      n_nat_year_2_spawners<-n_nat_adult*portion_nat_brood_to_spawn_age[[1]][1]
      n_nat_year_3_spawners<-n_nat_adult*portion_nat_brood_to_spawn_age[[1]][2]
      n_nat_year_4_spawners<-n_nat_adult*portion_nat_brood_to_spawn_age[[1]][3]
      n_nat_year_5_spawners<-n_nat_adult*portion_nat_brood_to_spawn_age[[1]][4]
      
      nat_df[i,"year"] <- i
      nat_df[i+1,"2yo"] <- n_nat_year_2_spawners
      nat_df[i+2,"3yo"] <- n_nat_year_3_spawners
      nat_df[i+3,"4yo"] <- n_nat_year_4_spawners
      nat_df[i+4,"5yo"] <- n_nat_year_5_spawners
      nat_df[i,"sum_spawn_rets"] <- sum(nat_df[i,2:5], na.rm = TRUE)
      
      ## hatchery fish fry to spawn ####
      n_hat_adult <- n_hat_fry*hat_fry_to_adult_survival
      
      n_hat_year_2_spawners<-n_hat_adult*portion_hat_brood_to_spawn_age[[1]][1]
      n_hat_year_3_spawners<-n_hat_adult*portion_hat_brood_to_spawn_age[[1]][2]
      n_hat_year_4_spawners<-n_hat_adult*portion_hat_brood_to_spawn_age[[1]][3]
      n_hat_year_5_spawners<-n_hat_adult*portion_hat_brood_to_spawn_age[[1]][4]
      
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
        #floor takes any partial fish counted as a returner and rounds down to the nearest whole fish
        nat_spawner_rets <- floor(nat_df[i,"sum_spawn_rets"]) 
        
        #total the spawners returning for the next round
        hat_spawner_rets <- floor(hat_df[i,"sum_spawn_rets"])
        
        # spawner to egg ####
        #total spawners that returned
        total_spawners <- nat_spawner_rets + hat_spawner_rets
        
        #maxium number of spawners the system is capable of holding ####
        if (total_spawners > maximum_spawner_capacity) {
          total_spawners <- maximum_spawner_capacity
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
      
    } #end i loop (last year completed)
    
    #plot based on manually set y-axis range
    if (j == 1) {
      plot(x = grand_df[5:(years),1], 
           y = grand_df[5:(years),j+1], type = "l",
           xlab = "years", 
           ylab = "healthy spawners",
           ylim = c(0, 1400),   #set y-axis manually here
           col = rgb(.79,.79,.79,.1)) #4th term is alpha/transparency
    } else { 
      lines(x = grand_df[5:(years),1],
            y = grand_df[5:(years),1+j],
            col= rgb(.79,.79,.79,.1))
    } 
    
    
  }  #end j loop (last run of that scenario completed)
  
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
       col = rgb(.79,.79,.79,.1))
  for(i in c(3:dim(grand_df)[2])){
    lines(x = grand_df[5:nrow(grand_df)],
          y = grand_df[5:nrow(grand_df), i],
          col = rgb(.79,.79,.79,.1))
  }
  
  lines(x = grand_df[5:(years),1],
        y = rowMeans(grand_df[,-1], na.rm = TRUE)[5:(years)]) #solid line for mean
  lines(x = grand_df[5:(years),1],
        y = apply(grand_df[,-1], 1, median, na.rm = TRUE)[5:(years)],
        lty = 2) # dashed line for median
  
  dev.off()
  
  ##Quantile Plot
  grand_df_plot_filename <- paste0("Output/plot_quantiles_spawners_",scen, ".tiff")
  tiff(filename = grand_df_plot_filename, width = 7.5, height = 6, units = "in", pointsize = 10, res = 400, family = "sans", compression = "lzw")
  
  par(mar = c(5,5,2,12), xpd = TRUE) #add extra space to the right for the legend
  
  #plot based on model output range 
  plot(x = grand_df[5:(years),1],
       y = rowMeans(grand_df[,-1], na.rm = TRUE)[5:(years)], #solid line for mean
       type = "l",
       xlab = "years", 
       ylab = "healthy spawners",
       ylim = c(0, top_of_range))
  
  lines(x = grand_df[5:(years),1],
        y = apply(grand_df[,-1], 1, median, na.rm = TRUE)[5:(years)],
        lty = 2) # dashed line for median
  
  #2.5 c(0.025,0.25, 0.75, 0.975))
  lines(x = grand_df[5:(years),1],
        y = apply(grand_df[,-1], 1, quantile, probs = 0.025, na.rm = TRUE)[5:(years)],
        col = "#56BFC1",
        lty = 3) # dotted
  
  #97.5thth
  lines(x = grand_df[5:(years),1],
        y = apply(grand_df[,-1], 1, quantile, probs = 0.975, na.rm = TRUE)[5:(years)],
        col = "#56BFC1",
        lty = 3)
  
  #25thth
  lines(x = grand_df[5:(years),1],
        y = apply(grand_df[,-1], 1, quantile, probs = 0.25, na.rm = TRUE)[5:(years)],
        col = "#C8B7F3",
        lty = 1342)
  
  #75thth
  lines(x = grand_df[5:(years),1],
        y = apply(grand_df[,-1], 1, quantile, probs = 0.75, na.rm = TRUE)[5:(years)],
        col = "#C8B7F3",
        lty = 1342)
  
  legend(x = "topright",
         inset=c(-.35, .25),
         bty="n",
         legend = c("mean", "median", "2.5% and 97.5%", "25% and 75%"),
         lty = c(1,2,3,1342),
         col = c("black", "black", "#56BFC1", "#C8B7F3"))
  
  dev.off()
  ## end quantiles plot
  
  #save the grand_df
  grand_df_filename <- paste0("Output/mean_spawners_",scen, ".csv")
  write.csv(grand_df, grand_df_filename)
  
  returner_df[5:(years),k+1] <- rowMeans(grand_df[,-1], na.rm = TRUE)[5:(years)]
  returner_df_median[5:(years),k+1] <- apply(grand_df[,-1], 1, median, na.rm = TRUE)[5:(years)]
  returner_df_sd[5:(years),k+1] <- apply(grand_df[,-1], 1, sd, na.rm = TRUE)[5:(years)]
  
} #end k loop (last scenario completed)

#save the returner_df
returner_df_filename <- paste0("Output/overview_mean_spawners.csv")
write.csv(returner_df, returner_df_filename)

#save the returner_df_median
returner_df_median_filename <- paste0("Output/overview_median_spawners.csv")
write.csv(returner_df_median, returner_df_median_filename)

#save the returner_df_sd
returner_df_sd_filename <- paste0("Output/overview_sd_spawners.csv")
write.csv(returner_df_sd, returner_df_sd_filename)

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

