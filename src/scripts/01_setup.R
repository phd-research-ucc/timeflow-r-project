# Meta Data ---------------------------------------------------------------
#
# Timeflow Analytics Using R
# 01_setup.R
#




# Import Packages ----------------------------------------------------------------

library(tidyverse)
library(glue)




# Read Files ---------------------------------------------------------------

raw_df <- read.csv(
  'src/data/raw/2023-11-27_timeflow_main.csv', 
  header = TRUE, 
  skip = 1
) |>
  select(-X)




# Restore Missing Data ----------------------------------------

# restore dates
restored_df <- raw_df |>
  mutate( date = as.Date(date) ) |>
  fill(date, .direction = 'down') |>
  unite(tags, tag1, tag2, tag3, sep = '---') |>
  mutate(tags = ifelse(tags == '------', NA, tags)) |>
  fill(tags, .direction = 'down') |>
  separate(tags, into = c("tag1", "tag2", "tag3"), sep = "---", remove = FALSE) |>
  select(-tags)

# View(restored_df)





# Analytics ---------------------------------------------------------------

summary_df <- restored_df |>
  group_by(date) |>
  summarise(
    hours = sum(time_min) %/% 60,
    minutes = sum(time_min) %% 60
  )





# Charts ------------------------------------------------------------------

# Bar chart
bar_chart_df <- restored_df |>
  group_by(date, tag1) |>
  summarise( total_min = sum(time_min) )

ggplot(
  bar_chart_df, 
  aes(x = date, y = total_min, fill = tag1) ) +
  geom_col(position = "stack", width = 0.7) +
  geom_text(aes(label = total_min), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Total Time for Each Activity per Day",
       x = "Date",
       y = "Total Time (minutes)",
       fill = "Activity") +
  theme_minimal()


# Pie chart
pie_chart_df <- restored_df |>
  group_by(tag1) |>
  summarise( total_min = sum(time_min) )

ggplot(pie_chart_df, aes(x = "", y = total_min, fill = tag1)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_minimal() +
  labs(title = "Time spent distribution among activities",
       fill = "Category") +
  theme(legend.position = "right")


# Heat map
heat_map_df <- restored_df |>
  group_by(date) |>
  summarise( total_min = sum(time_min) )


# # Example data frame
# your_data <- data.frame(
#   Month = rep(c("Jan", "Feb", "Mar", "Apr", "May", "Jun"), each = 3),
#   Activity = rep(c("A", "B", "C"), times = 6),
#   Value = c(10, 15, 8, 20, 25, 18, 12, 8, 15, 22, 30, 25, 14, 18, 10, 12, 28, 20)
# )
# 
# your_data_wide <- your_data %>%
#   pivot_wider(names_from = Activity, values_from = Value)
# 
# ggplot(
#   your_data_wide, 
#   aes(x = Month, y = Activity, fill = A)
#   ) +
#   geom_tile(color = "white", linewidth = 0.5) +
#   scale_fill_viridis_c() +  # You can use other color scales from viridis package
#   theme_minimal() +
#   labs(title = "Monthly Activity Heatmap",
#        x = "Month",
#        y = "Activity",
#        fill = "Value")
