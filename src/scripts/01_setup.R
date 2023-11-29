# Meta Data ---------------------------------------------------------------
#
# Timeflow Analytics Using R
# 01_setup.R
#




# Import Packages ----------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(hms)
library(glue)




# Read Files ---------------------------------------------------------------

raw_df <- read.csv(
  'src/data/raw/2023-11-28_timeflow_main.csv', 
  header = TRUE, 
  skip = 1
) |>
  select(-X)




# Restore Missing Data ----------------------------------------

# restore dates
restored_df <- raw_df |>
  filter(time_min != 0) |> 
  mutate( date = as.Date(date) ) |>
  fill(date, .direction = 'down') |>
  unite(tags, tag1, tag2, tag3, sep = '---') |>
  mutate(tags = ifelse(tags == '------', NA, tags)) |>
  fill(tags, .direction = 'down') |>
  separate(tags, into = c("tag1", "tag2", "tag3"), sep = "---", remove = FALSE) |>
  select(-tags) |>
  mutate_at(vars(tag1:tag4), ~str_trim(.)) |> 
  filter(if_any(everything(), ~str_detect(., "UCC")))

# View(restored_df)





# Analytics ---------------------------------------------------------------

summary_df <- restored_df |>
  group_by(date) |>
  summarise(
    hours = sum(time_min) %/% 60,
    minutes = sum(time_min) %% 60
  )



# Functions ---------------------------------------------------------------

# TODO:Create a custom function to determine text color
get_text_color <- function(fill_color) {
  # Use the perceived luminance to determine text color
  luminance <- 0.299 * col2rgb(fill_color)[1, ] +
    0.587 * col2rgb(fill_color)[2, ] +
    0.114 * col2rgb(fill_color)[3, ]
  
  if (luminance > 128) {
    return("black")
  } else {
    return("white")
  }
}

# Charts ------------------------------------------------------------------

# Bar chart
bar_chart_df <- restored_df |>
  mutate(tag1 = factor(tag1, levels=c('Coding', 
                                      'Writing',
                                      'Reading',
                                      'Volunteering',
                                      'Organisation')),
         date = paste( format(date, "%b %d, %a") ) ) |>
  group_by(date, tag1) |>
  summarise( total_min = sum(time_min) )

# total_per_day <- bar_chart_df |> 
#   group_by(date) |> 
#   summarise(total_spent = sum(total_min))


activity_per_day_test <- ggplot(
  bar_chart_df, 
  aes(x = date, y = total_min, fill = tag1, label = total_min) ) +
  # geom_bar(stat = "identity") +
  geom_col(position = 'stack', width = 0.7) +
  geom_text(
    aes(label = total_min, group = tag1), 
    position = position_stack(vjust = 0.5), 
    size = 3,
    # fontface = "bold",
    family = "Roboto",
    col = c(
      rep('white', 3), 
      'black', 
      rep('white', 1), 
      'black', 
      rep('white', 1), 
      'black', 
      rep('white', 2),
      'black',
      rep('white', 3),
      'black'
    )
  ) +
  stat_summary(aes(label = ..y.., group = 1),
               fun = sum,
               geom = "text",
               position = position_stack(vjust = 1.04),
               color = "black", 
               fontface = "bold",
               family = "Roboto",
               size = 4) +
  labs(title = 'Total Time for Each Activity per Day',
       x = '',
       y = 'Total Time (minutes)',
       fill = '') +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(
    axis.text.y = element_blank(),  # Remove left side labels
    axis.ticks.y = element_blank(), # Remove left side ticks
    panel.grid.major.x = element_blank(),  # Remove horizontal grid lines
    # panel.grid.minor = element_blank(),  # Remove vertical grid lines
    # panel.background = element_blank(), # Remove panel background
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.title = element_text(hjust = 0.5),
    text = element_text(family = "Roboto")
  )

# save bar chart
ggsave(activity_per_day_test,
       filename = glue('src/plots/{ Sys.Date() }_activity_per_day_test.png'),
       width = 20,
       height = 15,
       units = 'cm',
       dpi = 300)

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
