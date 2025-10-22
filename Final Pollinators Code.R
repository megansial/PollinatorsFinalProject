# Research Question: Are pollinator numbers changing over the years?
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
poll_num |>
  ggplot(aes(x = year_collected, y = pollinators_per_year, fill = year_collected)) +
  geom_line() +
  geom_point() +
  labs(x = "Years",
       y = "Number of Pollinators",
       title = "How the Amount of Pollinators Has Changed Over the Years") +
  theme_linedraw() +
  scale_x_continuous(limits = c(2005, 2018))


# bar chart
poll_num |>
  ggplot(aes(x = year_collected, y = pollinators_per_year, fill = year_collected)) +
  geom_bar(stat = 'identity') + 
  labs(x = "Years",
       y = "Number of Pollinators",
       title = "How the Amount of Pollinators Has Changed Over the Years") +
  theme_light() +
  scale_x_continuous(limits = c(2004, 2018))


