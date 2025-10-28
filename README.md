# Lake Sammamish Kokanee life cycle model 

Code for a Kokanee Life Cycle Model that estimates future Kokanee returns based on the most recent observations of the Lake Sammamish Kokanee alongside estimates for future returns given several different types of management actions.
Contributers include Beka Stiling, Bailey Keeler, Jim Bower, and Alison Agness with input from Neala Kendall. 

## Contents
### Folder: R
Kok_LCM.R - Runs through each scenario creating individual plots for each run alongside an a csv file containing run data. Script also produces summary dataframes containing mean, median, and sd values for each year of each scenario.

Kok_LCM_visualizations.R - Produces visualizations used for communications and events and saves them in the Output folder.

Testing_Model.R - script to assess if early results reflect recent observations.

### Folder: Output
Plots and csv files from each scenario. Visualizations for communications and events.

File naming convention:
- "mean_spawners_[scenario name].csv" : output data of the number of returners for each year for each of the runs.
- "plot_mean_spawners_[scenario name].tiff" : plot of the number of returners for each year for each of the runs with a solid mean and dashed median line.
- "plot_quantile_spawners_[scenario name].tiff" : plots with a solid mean and dashed median line alongside dottent 2.5, 25, 75, and 97.5 percentiles.
- "allscenarios_zoom.tiff" : mean outcome from all runs, zoomed in a little more each panel to see additional detail in low numbers.
- "Kok_release_[scenario].tiff : visualizations for kokanee release poster
- "overview_[stat]_spawners.csv : files with the population mean, median, and sd for each year of the model for each scenario. The sd data frames were never used, rather confidence intervals were calculated with quantiles, calculating a 95% confidence interval with the 2.5-97.5 percentiles.
