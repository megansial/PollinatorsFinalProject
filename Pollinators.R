library(readr)

# import dataset
main_pollinator_plant <- read_csv("Desktop/analytics day/main_pollinator_plant.csv")
# make a copy
pollinators <- main_pollinator_plant
View(pollinators)

# load packages 
library(tidyverse)
library(viridis) # colorblind friendly!

# count how many samples were collected for each method each year
poll_heat <- pollinators |>
  filter(between(year_collected, 2005, 2017)) |> # keeps only the rows where year falls in 2005 - 2017
  group_by(year_collected, collection_method) |> # groups data by year and method
  summarise(samples = n(), .groups = "drop") # counts each collection method instance and removes the group so we have regular data frame 

# to peek and check
poll_heat |>
  glimpse()

nleast |>
  ggplot(aes(x = yearID,y = teamID)) +
  geom_tile(aes(fill=W)) +
  theme_classic() +
  scale_fill_viridis(option="A")

# heat map
poll_heat |>
  ggplot(aes(x = year_collected, y = collection_method)) +
  geom_tile(aes(fill = samples), color = "white") + # color is for the borders
  scale_fill_viridis(option = "A") +
  theme_classic() +
  labs(
    x = "Year Collected",
    y = "Collection Method",
    fill = "Samples",
    title = "Pollinator Sampling by Collection Method and Year",
    subtitle = "Heatmap showing number of pollinator samples (2005–2017)"
  ) +
  theme(
    axis.title.x = element_text(family = "Times New Roman", face = "bold", size = 14),
    axis.title.y = element_text(family = "Times New Roman", face = "bold", size = 14),
    axis.text.x = element_text(family = "Times New Roman", angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(family = "Times New Roman", size = 11),
    plot.title = element_text(family = "Times New Roman", face = "bold", size = 16),
    plot.subtitle = element_text(family = "Times New Roman", size = 12),
    legend.title = element_text(family = "Times New Roman", size = 12),
    legend.text = element_text(family = "Times New Roman", size = 10)
  )
