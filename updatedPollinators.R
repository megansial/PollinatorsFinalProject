# load packages 
library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)

# import dataset
main_pollinator_plant <- read_csv("Desktop/main_pollinator_plant.csv")
# make a copy
pollinators <- main_pollinator_plant
View(pollinators)

## RESEARCH QUESTION ##
## which plant species receive the most pollinator visits? ##
## and do visit rates differ significantly among species? ##

## VISUALIZATION 1: vertical bar chart showing total visits per plant species ##
# remove all rows that dont have a plant species listed
plantspecies <- pollinators |>
  filter(!is.na(plant_species) & plant_species != "") # removes rows w/ NA and empty values

totalvisits <- plantspecies |>
  group_by(plant_species) |>
  summarise(visits = n())

totalvisits <- plantspecies |>
  group_by(plant_species) |>
  summarise(visits = n(), .groups = "drop") |> # ungroup data to keep as normal data frame 
  arrange(desc(visits))

top10visits <- totalvisits |>
  slice_head(n = 10)

top10visits |>
  ggplot(aes(x = reorder(plant_species, -visits), y = visits)) +
  geom_bar(stat = "identity", color = "black", fill = "white") +
  labs(x = "Plant Species",
       y = "Total Pollinator Visits",
       title = "Top 10 Plant Species Visited by Pollinators",
       subtitle = "In the Pacific Northwest from 2005 to 2017") +
  theme_classic() + 
  theme(plot.title = element_text(family = "Times New Roman",
                                  face = "bold.italic",
                                  size = 16),
        plot.subtitle = element_text(family = "Times New Roman",
                                     face = "italic",
                                     size = 10),
        axis.text.x = element_text(family = 'Times New Roman'),
        axis.title.x = element_text(family = 'Times New Roman', 
                                    face = 'bold'),
        axis.title.y = element_text(family = 'Times New Roman', 
                                    face = 'bold'))

## VISUALIZATION 2: heat map showing which pollinator families visit which plant species most frequently ##  
library(viridis) # colorblind friendly!

# we already founf top 10 plants with the most visits
# now we find top 10 pollinators making these visits
totalpollinators <- plantspecies |>
  filter(!is.na(pollinator_family) & pollinator_family != "") |> # removes rows w/ NA and empty values
  group_by(pollinator_family) |>
  summarise(visits = n(), .groups = "drop") |> # ungroup data to keep as normal data frame 
  arrange(desc(visits)) 

top10pollinators <- totalpollinators |>
  slice_head(n = 10)

pollheatmap <- plantspecies |>
  semi_join(top10visits, by = "plant_species") |> # only keeps top 10 plant species
  semi_join(top10pollinators, by = "pollinator_family") |> # only keeps top 10 pollinator families
  group_by(plant_species, pollinator_family) |>
  summarise(visits = n(), .groups = "drop") # count visits

pollheatmap |>
  ggplot(aes(x = pollinator_family, y = reorder(plant_species, -visits))) +
  geom_tile(aes(fill = visits), color = "white") +
  theme_classic() +
  scale_fill_viridis(option = "A") +
  labs(x = "Pollinator Family",
       y = "Plant Species",
       title = "Top 10 Pollinator Families Visiting the Top 10 Plant Species",
       subtitle = "In the Pacific Northwest from 2005 to 2017",
       fill = "Total Visits") +
  theme(
    plot.title = element_text(family = "Times New Roman",
                              face = "bold.italic",
                              size = 16),
    plot.subtitle = element_text(family = "Times New Roman",
                                 face = "italic",
                                 size = 10),
    axis.text.x = element_text(family = "Times New Roman",
                               angle = 45,
                               hjust = 1),
    axis.text.y = element_text(family = "Times New Roman"),
    axis.title.x = element_text(family = "Times New Roman",
                                face = "bold"),
    axis.title.y = element_text(family = "Times New Roman",
                                face = "bold"))

# VISUALIZATION 3: boxplot of showing visit distribution across plant species
