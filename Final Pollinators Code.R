library(dplyr)
library(readr)
library(ggplot2)
library(ggrepel)
# install.packages("rcartocolor")
library(rcartocolor)

# Import Datatset and Rename
pollinators_data <- read.csv("main_pollinator_plant.csv")
View(pollinators_data)

summary(pollinators_data)

# Research Question: 1. Are pollinator numbers changing over the years?

# i want to group by year_collected so we can see how many pollinators there are per year
# find the amount of pollinators per year
total_years <- pollinators_data |>
  distinct(year_collected) |>
  nrow()

poll_num <- pollinators_data |>
  group_by(year_collected) |>
  summarise(pollinators_per_year = n() / total_years, .groups = "drop")
View(poll_num)

minmax <- poll_num |>
  filter(pollinators_per_year == min(pollinators_per_year) | pollinators_per_year == max(pollinators_per_year))

minmax |>
  glimpse()


# create labels with minmax
minmax <- minmax |>
  mutate(label = case_when(pollinators_per_year == min(pollinators_per_year) ~ paste("Lowest Amount of Pollinators \nin 2005-2017"),
                           pollinators_per_year == max(pollinators_per_year) ~ paste("Highest Amount of Pollinators \nin 2005-2017")))

minmax |>
  glimpse()


# Create a Time-series plot
poll_num |>
  ggplot(aes(x = year_collected, y = pollinators_per_year, fill = year_collected)) +
  geom_line(linewidth = 2, color='goldenrod1') +
  geom_point(color='darkorange2', size = 3) +
  geom_label_repel(aes(label=label), # place the new label here equal to 'label'
                   family = 'serif',
                   color = 'chocolate4',
                   fontface= 'bold',
                   fill = alpha('white', 0.7),
                   size=4,
                   nudge_y = 1,
                   nudge_x = c(-2,-2),
                   alpha=1,
                   data = minmax) +
  labs(x = "Years",
       y = "Number of Pollinators",
       title = "How the Amount of Pollinators Has Changed Over the Years",
       subtitle = "From 2005 - 2017") +
  theme_linedraw() +
  theme(legend.position = "none") +
  theme(plot.title = element_text(family = "Times New Roman", face="bold", size = 18),
        plot.subtitle = element_text(family = "Times New Roman", size = 12)) +
  theme(axis.title.x = element_text(family = "Times New Roman", face = 'bold', size = 15),
        axis.text.x = element_text(family = "Times New Roman", size = 14),
        axis.title.y = element_text(family = "Times New Roman", face = 'bold', size = 15),
        axis.text.y = element_text(family = "Times New Roman", size = 14)) +
  scale_y_continuous(limits = c(0, 1700),breaks = seq(0, 1700, by = 200)) +
  scale_x_continuous(limits = c(2005, 2017), breaks = seq(2005, 2017, by = 1))

# Create a Bar Chart
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
  theme(legend.position = "none") +
  theme(plot.title = element_text(family = "Times New Roman", face="bold", size = 18),
        plot.subtitle = element_text(family = "Times New Roman", size = 12)) +
  theme(axis.title.x = element_text(family = "Times New Roman", face = 'bold', size = 15),
        axis.text.x = element_text(family = "Times New Roman", size = 14.5),
        axis.title.y = element_text(family = "Times New Roman", face = 'bold', size = 15),
        axis.text.y = element_text(family = "Times New Roman", size = 14.5)) +
  scale_y_continuous(limits = c(0, 2000),breaks = seq(0, 2000, by = 200)) +
  scale_x_continuous(limits = c(2004, 2018), breaks = seq(2004, 2018, by = 2)) +
  scale_fill_carto_c(palette = "OrYel", direction = -1)

# 3. How does pollinator diversity vary among sites?
# Create Grouped Bar Chart

# Calculate pollinator diversity (unique genera per site)
pollinator_diversity <- pollinators_data |>
  group_by(location_name) |>
  summarise(unique_pollinator_genera = n_distinct(pollinator_genus, na.rm = TRUE)) %>%
  arrange(desc(unique_pollinator_genera))

# Identify top 10 most diverse sites
top10_sites <- pollinator_diversity |>
  slice_head(n = 10) |>
  pull(location_name)

# Filter dataset for only those sites
df_top10 <- pollinators_data |>
  filter(location_name %in% top10_sites)

# Count pollinator genus frequency per site
genus_freq <- df_top10 |>
  group_by(location_name, pollinator_genus) |>
  summarise(frequency = n(), .groups = "drop")

# Keep top 5 most frequent genera per site
genus_top5 <- genus_freq |>
  group_by(location_name) |>
  slice_max(order_by = frequency, n = 5) %>%
  ungroup()
View(genus_top5)

minmax <- genus_top5 |>
  filter(frequency == min(frequency) | frequency == max(frequency))

minmax |>
  glimpse()


# Create labels with minmax
minmax <- minmax |>
  mutate(label = case_when(frequency == min(frequency) ~ paste(pollinator_genus, ": ", frequency, "\nLowest Amount of Pollinators \nin ", location_name),
                           frequency == max(frequency) ~ paste(pollinator_genus, ": ", frequency, "\nHighest Amount of Pollinators \nin ", location_name)))

minmax |>
  glimpse()

# Create a grouped bar chart
genus_top5 |>
ggplot(aes(x = location_name, y = frequency, fill = pollinator_genus)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_label_repel(data = minmax,
    aes(x = location_name, y = frequency, label = label),
    family = "serif",
    color = "black",
    fontface = "bold",
    fill = alpha("white", 0.7),
    size = 4,
    nudge_y = c(100,250),
    nudge_x = 2,
    alpha = 1) +
  labs(title = "Top 5 Pollinator Genera per Site",
       subtitle = "In Top 10 Most Diverse Sites",
    x = "Site (Location Name)",
    y = "Frequency of Pollinator Genus",
    fill = "Pollinator Genus") +
  theme_light(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right") +
  theme(plot.title = element_text(family = "Times New Roman", face="bold", size = 18),
        plot.subtitle = element_text(family = "Times New Roman", size = 12)) +
  theme(axis.title.x = element_text(family = "Times New Roman", face = 'bold', size = 15),
        axis.text.x = element_text(family = "Times New Roman", size = 14),
        axis.title.y = element_text(family = "Times New Roman", face = 'bold', size = 15),
        axis.text.y = element_text(family = "Times New Roman", size = 14))

# Identify the top 10 locations with most specimens
top_locations <- pollinators_data |>
  count(location_name, sort = TRUE) |>
  head(10) |>
  pull(location_name)

# Filter data to only include top 10 locations
pollinator_top_locations <- pollinators_data |>
  filter(location_name %in% top_locations)

pollinator_top_locations |>
  glimpse()

library(see)
# Create half-violin plots for pollinator diversity
pollinator_top_locations |>
  ggplot(aes(x = location_name, y = as.numeric(factor(pollinator_species)))) +
  geom_violinhalf(fill = 'darkorange2', alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = 'white') +
  labs(x = "Location Site",
       y = "Frequency of Pollinator Genus",
       title = "Pollinator Diversity by Location Site",
       subtitle = "Top 10 Sites with Most Specimens - Distribution Shown") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(family = "Times New Roman", face="bold", size = 18),
        plot.subtitle = element_text(family = "Times New Roman", size = 12)) +
  theme(axis.title.x = element_text(family = "Times New Roman", face = 'bold', size = 15),
        axis.text.x = element_text(family = "Times New Roman", size = 14),
        axis.title.y = element_text(family = "Times New Roman", face = 'bold', size = 15),
        axis.text.y = element_text(family = "Times New Roman", size = 14))
