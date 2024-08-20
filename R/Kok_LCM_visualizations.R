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

#load counts for each returner scenario
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

p1 <-returners_long %>% 
  mutate(label = if_else(year == max(year), as.character(scenario), NA_character_)) %>% 
  ggplot(aes(x = year, y = spawners, group = scenario)) +
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
  filter(!scenario %in%  c("sc5.2", "C")) %>% 
  mutate(label = if_else(year == max(year), as.character(scenario), NA_character_)) %>% 
  ggplot(aes(x = year, y = spawners, group = scenario)) +
  geom_line(color = "gray") +
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) + 
  theme_classic()
p2  
p3 <-returners_long %>%
  filter(!scenario %in%  c("sc5.2", "C", "sc5.1", "E", "sc2.3", "A")) %>% 
  mutate(label = if_else(year == max(year), as.character(scenario), NA_character_)) %>% 
  ggplot(aes(x = year, y = spawners, group = scenario)) +
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

#Bailey uses Beka code to compare scenarios with their combo 
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

ggsave(plot = all_threepanelB, filename = "Output/allscenarios_zoom.tiff",
       width = 7.5, height = 5, units = "in")
