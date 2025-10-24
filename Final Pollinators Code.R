# Research Question: 1. Are pollinator numbers changing over the years?
library(dplyr)

pollinators_data <- read.csv("main_pollinator_plant.csv")
View(pollinators_data)

summary(pollinators_data)

# i want to group by year_collected so we can see how many pollinators there are per year
# find the amount of pollinators per year
total_years <- pollinators_data |>
  distinct(year_collected) |>
  nrow()

poll_num <- pollinators_data |>
  group_by(year_collected) |>
  summarise(pollinators_per_year = n() / total_years, .groups = "drop")
View(poll_num)

library(ggplot2)

# Time-series plot
minmax <- poll_num |>
  filter(pollinators_per_year == min(pollinators_per_year) | pollinators_per_year == max(pollinators_per_year))

minmax |>
  glimpse()



# create labels with minmax
minmax <- minmax |>
  mutate(label = case_when(pollinators_per_year == min(pollinators_per_year) ~ paste("\nLowest Amounf of Pollen Collected \nin 2005-2017"),
                           pollinators_per_year == max(pollinators_per_year) ~ paste("\nHighest Amount of Pollen Collected \nin 2005-2017"))
  )

minmax |>
  glimpse()

library(ggrepel)
# install.packages("rcartocolor")
library(rcartocolor)
poll_num |>
  ggplot(aes(x = year_collected, y = pollinators_per_year, fill = year_collected)) +
  geom_line(linewidth = 2, color='goldenrod1') +
  geom_point(color='darkorange2', size = 3) +
  geom_label_repel(aes(label=label), # place the new label here equal to 'label'
                   family = 'serif',
                   color = 'chocolate4',
                   fontface= 'bold',
                   fill = alpha('white', 0.7),
                   size=3.5,
                   nudge_y = 1,
                   nudge_x = c(-3,-3),
                   alpha=1,
                   data = minmax) +
  labs(x = "Years",
       y = "Number of Pollinators",
       title = "How the Amount of Pollinators Has Changed Over the Years",
       subtitle = "From 2005 - 2017") +
  theme_linedraw() +
  theme(plot.title = element_text(family = "Times New Roman", face="bold", size = 18),
        plot.subtitle = element_text(family = "Times New Roman", size = 12)) +
  theme(axis.title.x = element_text(family = "Times New Roman", face = 'bold', size = 14),
        axis.text.x = element_text(family = "Times New Roman", size = 12),
        axis.title.y = element_text(family = "Times New Roman", face = 'bold', size = 14),
        axis.text.y = element_text(family = "Times New Roman", size = 12)) +
  scale_y_continuous(limits = c(0, 1700)) +
  scale_x_continuous(limits = c(2005, 2018))


# bar chart
poll_num |>
  ggplot(aes(x = year_collected, y = pollinators_per_year, fill = year_collected)) +
  geom_bar(stat = 'identity') + 
  geom_label_repel(aes(label=label), # place the new label here equal to 'label'
                   family = 'serif',
                   color = 'chocolate4',
                   fontface= 'bold',
                   fill = alpha('white', 0.7),
                   size=4,
                   nudge_y = c(300,1000),
                   nudge_x = 0,
                   alpha=1,
                   data = minmax) +
  labs(x = "Years",
       y = "Number of Pollinators",
       title = "How the Amount of Pollinators Has Changed Over the Years",
       subtitle = "From 2005 - 2017") +
  theme_light() +
  theme(plot.title = element_text(family = "Times New Roman", face="bold", size = 18),
        plot.subtitle = element_text(family = "Times New Roman", size = 12)) +
  theme(axis.title.x = element_text(family = "Times New Roman", face = 'bold', size = 14),
        axis.text.x = element_text(family = "Times New Roman", size = 12),
        axis.title.y = element_text(family = "Times New Roman", face = 'bold', size = 14),
        axis.text.y = element_text(family = "Times New Roman", size = 12)) +
  scale_x_continuous(limits = c(2004, 2017.5)) +
  scale_y_continuous(limits = c(0,2000)) +
  scale_fill_carto_c(palette = "OrYel", direction = -1)

# 2. Which plant species receive the most visits?


# 3. How does pollinator diversity vary among sites?
