####Kokanee LCM Scenarios discussed####
##Run model with data through 2019; 

#Increased hatchery-only fry to adult survival = Scenario 1
#Increased natural fry to adult survival = Scenario 2
#Increased egg to fry survival = Scenario 3
#Increase hatchery-origin fry released = Scenario 4
#Decreased incidence of disease (cant turn this dial because this is folded in with the rest of lake survival) 
#Combinations 



#separately run model with data through 2023 to evaluate progress since implementing new recovery strategies 
#(e.g., new hatchery practices; new rearing strategies; passage improvements) 

####SCENARIO 1####
#improved hatchery survival 
#name of scenario: 

hat_fry_to_spawn_survival <- .0197 #scenario change: improve hatchery lake survival rate to mimic what we see in natural survival. original input: 0.06% geometric mean from jim 2009-2019 updated 5/29/24

####scenario 2####
#increased nat fry to adult survival: Double natural survival from .0197(staus quo/Jim data) to 0.0394
#name of scenario: 
nat_fry_to_spawn_survival <- .0394 #doubled natural survival
hat_fry_to_spawn_survival <- .0006 #0.06% geometric mean from jim 2009-2019 updated 5/29/24

####Scenario 3####
#Increased egg to fry survival
#name of scenario: 