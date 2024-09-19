#=== === === === === === === ===
# Script started by Rebekah Stiling, Bailey Keeler, and Jim Bower July 2024
# This creates additional figures and visualizations by resusing the scnario output csv files.
# rstiling@kingcounty.gov, bkeeler@kingcounty.gov
#=== === === === === === === ===

##transition to using ggplot
# load relevant packages
library(tidyverse) #for data minipulation and plotting
library(ggrepel) # for labeling
library(patchwork) #for assembling multipanel plots

#load mean counts for each returner scenario
returners_wide <-read_csv("Output/overview_mean_spawners.csv", 
                          col_names = TRUE, # the columns already have names
                          col_select = -1) # we don't need the first column because it is just a list of numbers from when the row names were turned into a column during the "write" csv step.

returners_long <- returners_wide %>% 
  pivot_longer(!year, names_to = "scenario", values_to = "mean_spawners") %>% #pivot to long format
  drop_na() #drop the NA values in years 1-5

#quick plot
returners_long %>% 
  mutate(label = if_else(year == max(year), as.character(scenario), NA_character_)) %>% 
  ggplot(aes(x = year, y = mean_spawners, group = scenario)) +
  geom_line(color = "gray") +
  geom_text(aes(label = label),
            nudge_x = 1,
            na.rm = TRUE) + 
  theme_classic()

#load median for each returner scenario
returners_median_wide <-read_csv("Output/overview_median_spawners.csv", 
                          col_names = TRUE, # the columns already have names
                          col_select = -1) # we don't need the first column because it is just a list of numbers from when the row names were turned into a column during the "write" csv step.

returners_median_long <- returners_median_wide %>% 
  pivot_longer(!year, names_to = "scenario", values_to = "median_spawners") %>% #pivot to long format
  drop_na() #drop the NA values in years 1-5

#quick plot
returners_median_long %>% 
  mutate(label = if_else(year == max(year), as.character(scenario), NA_character_)) %>% 
  ggplot(aes(x = year, y = median_spawners, group = scenario)) +
  geom_line(color = "gray") +
  geom_text(aes(label = label),
            nudge_x = 1,
            na.rm = TRUE) + 
  theme_classic()

#load median for each returner scenario
returners_sd_wide <-read_csv("Output/overview_sd_spawners.csv", 
                                 col_names = TRUE, # the columns already have names
                                 col_select = -1) # we don't need the first column because it is just a list of numbers from when the row names were turned into a column during the "write" csv step.

returners_sd_long <- returners_sd_wide %>% 
  pivot_longer(!year, names_to = "scenario", values_to = "sd_spawners") %>% #pivot to long format
  drop_na() #drop the NA values in years 1-5

returner_mids_long <-full_join(returners_long,returners_median_long, by = c("year", "scenario"))
returner_stats_long<- full_join(returner_mids_long, returners_sd_long, by = c("year", "scenario"))

#calculate the upper and lower bounds of the 95% CI using the sd - showing 2 standard deviations above and below mean
returner_stats_long$lowerCI <- returner_stats_long$mean_spawners - 2*returner_stats_long$sd_spawners
returner_stats_long$upperCI <- returner_stats_long$mean_spawners + 2*returner_stats_long$sd_spawners


## Plot for October 2024 Kokanee release poster ####
returner25 <-returner_stats_long %>% 
  filter(year < 25) %>% 
  mutate(label = if_else(year == max(year), as.character(scenario), NA_character_)) %>% 
  filter(scenario %in% c("sc1.0", "B", "C")) 

ggplot(data = returner25, aes(x = year, y = mean_spawners, group = scenario)) +
  geom_text(aes(label = label),
            nudge_x = 1,
            na.rm = TRUE) + 
  geom_line(color = "black") +
  scale_y_continuous(limits = c(0, 6500), breaks = c(0,1000, 2000, 3000, 4000, 5000, 6000)) +
  theme_classic()

ggsave(filename = "Output/Kok_release_1_B_C.tiff", width = 6, height = 6, units = "in")

### Just 1.0 ####
returner25 %>% filter(scenario == "sc1.0") %>% 
  ggplot(aes(x = year, y = mean_spawners, group = scenario)) +
  geom_text(aes(label = label),
            nudge_x = 1,
            na.rm = TRUE) + 
  geom_line(color = "black") +
  geom_line(data = returner25 %>% filter(scenario == "sc1.0"), aes(x = year, y = lowerCI), linetype = 2)+ 
  geom_line(data = returner25%>% filter(scenario == "sc1.0"), aes(x = year, y = upperCI), linetype = 2)+
  scale_y_continuous(limits = c(0, 6500), breaks = c(0,1000, 2000, 3000, 4000, 5000, 6000)) +
  theme_classic()

ggsave(filename = "Output/Kok_release_1.0.tiff", width = 6, height = 6, units = "in")

### Just C ####
returner25 %>% filter(scenario == "C") %>% 
  ggplot(aes(x = year, y = mean_spawners, group = scenario)) +
  geom_text(aes(label = label),
            nudge_x = 1,
            na.rm = TRUE) + 
  geom_line(color = "black") +
  geom_line(data = returner25 %>% filter(scenario == "C"), aes(x = year, y = lowerCI), linetype = 2)+ 
  geom_line(data = returner25%>% filter(scenario == "C"), aes(x = year, y = upperCI), linetype = 2)+
  scale_y_continuous(limits = c(0, 6500), breaks = c(0,1000, 2000, 3000, 4000, 5000, 6000)) +
  theme_classic()

ggsave(filename = "Output/Kok_release_C.tiff", width = 6, height = 6, units = "in")

### Just B ####
returner25 %>% filter(scenario == "B") %>% 
  ggplot(aes(x = year, y = mean_spawners, group = scenario)) +
  geom_text(aes(label = label),
            nudge_x = 1,
            na.rm = TRUE) + 
  geom_line(color = "black") +
  geom_line(data = returner25 %>% filter(scenario == "B"), aes(x = year, y = lowerCI), linetype = 2)+ 
  geom_line(data = returner25%>% filter(scenario == "B"), aes(x = year, y = upperCI), linetype = 2)+
  scale_y_continuous(limits = c(0, 5000), breaks = c(0,1000, 2000, 3000, 4000, 5000)) +
  theme_classic()

ggsave(filename = "Output/Kok_release_B.tiff", width = 6, height = 6, units = "in")

### Histogram for scenarios demonstrating population at end of run. ####
sc01 <-read_csv("output/mean_spawners_sc1.0.csv")
scC <-read_csv("output/mean_spawners_C.csv")
scB <-read_csv("output/mean_spawners_B.csv")

#write a function to plot the histogram of a scenario

hist_plot <- function(scenario) {
  ggplot(scenario %>% filter(V1 == 25) %>% select(V1:V1001) %>%
           pivot_longer(!V1, names_to = "run", values_to = "mean_spawners"), aes(x = mean_spawners )) +
  geom_histogram(binwidth = 50) +
    labs(x = "returning spawners") +
    coord_flip() +
    theme_minimal()

}

p1<-hist_plot(sc01)
p2<-hist_plot(scB)
p3<-hist_plot(scC)


dens_plot <- function(scenario) {
  ggplot(scenario %>% filter(V1 == 25) %>% select(V1:V1001) %>%
           pivot_longer(!V1, names_to = "run", values_to = "mean_spawners"), aes(x = mean_spawners )) +
    geom_density() +
    coord_flip() +
    theme_minimal()
}



p4<-dens_plot(sc01)
p5<-dens_plot(scB)
p6<-dens_plot(scC)


(p1+p4)/(p2+p5)/(p3+p6) + 
  plot_annotation(
    title = "Count of returners after 25 years" ,
    tag_levels = list(c( "scenario 01", "", "Combo B", ",", "Combo C", "")))

#What if I wanted to sum four years in a row to truly establish no fish returning at all? I need to reshape, then group by and summarize 
sc01 %>% 
  filter(V1 %in% c(22, 23, 24, 25)) %>% 
  select(V1:V1001) %>% 
  pivot_longer(!V1, names_to = "run", values_to = "mean_spawners") %>% 
  group_by(run) %>% 
  summarise(N = n(),
            sum_4yrs_0 = sum(mean_spawners)) %>% 
  ggplot(aes(x = sum_4yrs_0)) +
  geom_histogram(binwidth = 10) +
  theme_minimal()

sc01 %>% 
  filter(V1 %in% c(47, 48, 49, 50)) %>% 
  select(V1:V1001) %>% 
  pivot_longer(!V1, names_to = "run", values_to = "mean_spawners") %>% 
  group_by(run) %>% 
  summarise(N = n(),
            sum_4yrs_0 = sum(mean_spawners)) %>% 
  filter(sum_4yrs_0 <1)

sc01 %>% 
  filter(V1 %in% c(47, 48, 49, 50)) %>% 
  select(V1:V1001) %>% 
  pivot_longer(!V1, names_to = "run", values_to = "mean_spawners") %>% 
  group_by(run) %>% 
  summarise(N = n(),
            sum_4yrs_0 = sum(mean_spawners)) %>% 
  ggplot(aes(x = sum_4yrs_0)) +
  geom_histogram(binwidth = 1) +
  theme_minimal()

## End Plot for October 2024 Kokanee release poster 

## Three scenaro zoom in ####
p1 <-returners_long %>% 
  mutate(label = if_else(year == max(year), as.character(scenario), NA_character_)) %>% 
  ggplot(aes(x = year, y = mean_spawners, group = scenario)) +
  geom_line(color = "gray") +
  # scale_x_continuous(limits = c(0,60), breaks = c(10, 20, 30, 40, 50)) + 
  # geom_rect(xmin = 55,
  #           ymin =  0,
  #           xmax = 55.5,
  #           ymax = 5000) +
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) + 
  theme_classic()
p1

p2 <-returners_long %>%
  filter(!scenario %in%  c("sc5.2", "C", "F")) %>% 
  mutate(label = if_else(year == max(year), as.character(scenario), NA_character_)) %>% 
  ggplot(aes(x = year, y = mean_spawners, group = scenario)) +
  geom_line(color = "gray") +
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) + 
  theme_classic()
p2  
p3 <-returners_long %>%
  filter(!scenario %in%  c("sc5.2", "C", "sc5.1", "E", "sc2.3", "A", "B", "F")) %>% 
  mutate(label = if_else(year == max(year), as.character(scenario), NA_character_)) %>% 
  ggplot(aes(x = year, y = mean_spawners, group = scenario)) +
  geom_line(color = "gray") +
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) + 
  theme_classic()
p3

all_threepanel <-p1 + p2 + p3 +
  plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(0.1, 1))

ggsave(plot = all_threepanel, filename = "Output/allscenarios_zoom.tiff",
       width = 7.5, height = 5, units = "in")


#Bailey uses Beka code to compare scenarios with their combo ####
returners_wide <-read_csv("Output/overview_mean_spawners.csv", 
                          col_names = TRUE, # the columns already have names
                          col_select = -1) # we don't need the first column because it is just a list of numbers from when the row names were turned into a column during the "write" csv step.

returners_long <- returners_wide %>% 
  pivot_longer(!year, names_to = "scenario", values_to = "spawners") %>% #pivot to long format
  drop_na() #drop the NA values in years 1-5

returners_long %>% 
  mutate(label = if_else(year == max(year), as.character(scenario), NA_character_)) %>% 
  ggplot(aes(x = year, y = spawners, group = scenario)) +
  geom_line(color = "gray") +
  geom_text(aes(label = label),
            nudge_x = 1,
            na.rm = TRUE) + 
  theme_classic()


S21S31A <-returners_long %>%
  filter(scenario %in%  c("sc2.1", "sc3.1", "A")) %>% 
  mutate(label = if_else(year == max(year), as.character(scenario), NA_character_)) %>% 
  ggplot(aes(x = year, y = spawners, group = scenario)) +
  geom_line(color = "gray") +
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) + 
  geom_text_repel(aes(label=NA), max.overlaps = 15) +
  theme_classic()

S21S31A

s21s41B <-returners_long %>%
  filter(scenario %in%  c("sc2.1", "sc4.1", "B")) %>% 
  mutate(label = if_else(year == max(year), as.character(scenario), NA_character_)) %>% 
  ggplot(aes(x = year, y = spawners, group = scenario)) +
  geom_line(color = "gray") +
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) + 
  geom_text_repel(aes (label=NA),max.overlaps = 15)+
  theme_classic()

s21s41B

s21s51C <-returners_long %>%
  filter(scenario %in%  c("sc2.1", "sc5.1", "C")) %>% 
  mutate(label = if_else(year == max(year), as.character(scenario), NA_character_)) %>% 
  ggplot(aes(x = year, y = spawners, group = scenario, label = scenario)) +
  geom_line(color = "gray") +
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) +
  geom_text_repel(aes (label=NA),max.overlaps = 15)+
  theme_classic()

s21s51C

all_threepanelB <-S21S31A + s21s41B + s21s51C +
  plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(0.1, 1))
all_threepanelB

ggsave(plot = all_threepanelB, filename = "Output/allscenarios_zoom_Bailey.tiff",
       width = 7.5, height = 5, units = "in")
