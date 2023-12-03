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
library(patchwork)




# Read Files ---------------------------------------------------------------

raw_df <- read.csv('src/data/raw/2023-12-03_timeflow_main.csv',
                   header = TRUE,
                   skip = 1) |>
  select(-X)




# Restore Missing Data ----------------------------------------

# restore dates
restored_df <- raw_df |>
  filter(time_min != 0) |>
  mutate(date = as.Date(date)) |>
  fill(date, .direction = 'down') |>
  unite(tags, tag1, tag2, tag3, sep = '---') |>
  mutate(tags = ifelse(tags == '------', NA, tags)) |>
  fill(tags, .direction = 'down') |>
  separate(
    tags,
    into = c("tag1", "tag2", "tag3"),
    sep = "---",
    remove = FALSE
  ) |>
  select(-tags) |>
  mutate_at(vars(tag1:tag4), ~ str_trim(.)) |> 
  # filter(tag1 %in% c('Coding', 
  #                    'Reading', 
  #                    'Volunteering', 
  #                    'Organisation', 
  #                    'Teaching',
  #                    'Writing'))
  filter(if_any(everything(), ~ str_detect(., "UCC")))

# View(restored_df)



# Filtering ---------------------------------------------------------------

# Get the current date and calculate the start and end dates of the current week
current_date <- Sys.Date() - days(1)
start_of_week <-
  floor_date(current_date, "week") + days(1)  # Adjust by subtracting 1 day
end_of_week <- start_of_week + days(6)

# Create a complete set of dates for the current week
complete_dates <- data.frame(
  date = seq(start_of_week, end_of_week, by = "1 day"))


last_week_df <- restored_df |>
  filter(date >= Sys.Date() - 7)
# mutate(date = factor(date, levels = unique(date)))

last_week_df <- complete_dates |>
  left_join(last_week_df, by = "date") 



# Analytics ---------------------------------------------------------------

summary_df <- restored_df |>
  group_by(date) |>
  summarise(hours = sum(time_min) %/% 60,
            minutes = sum(time_min) %% 60)




# Functions ---------------------------------------------------------------

# TODO:Create a custom function to determine text color
get_text_color <- function(fill_color) {
  # Use the perceived luminance to determine text color
  luminance <- 0.299 * col2rgb(fill_color)[1,] +
    0.587 * col2rgb(fill_color)[2,] +
    0.114 * col2rgb(fill_color)[3,]
  
  if (luminance > 128) {
    return("black")
  } else {
    return("white")
  }
}



# Charts ------------------------------------------------------------------

# Bar chart
bar_chart_df <- last_week_df |>
  mutate(tag1 = ifelse(
    !is.na(tag1), 
    tag1, 
    'Coding') 
  ) |>
  group_by(date, tag1) |>
  # arrange(date) |>
  summarise(total_min = sum(time_min)) |>
  mutate(date = paste(format(date, "%b %d, %a"))) |> 
  mutate(total_hours = floor(total_min / 60),
         total_minutes = total_min %% 60,
         time_label = ifelse(
           total_hours > 0,
           ifelse(
             total_minutes > 0,
             sprintf("%2dh %2dm", total_hours, total_minutes),
             sprintf("%2dh", total_hours)
           ),
           sprintf("%2dm", total_minutes)
          ))
# mutate(date = factor(date, levels = unique(date)))

# total_per_day <- bar_chart_df |>
#   group_by(date) |>
#   summarise(total_spent = sum(total_min))


activity_per_day_test <- ggplot(bar_chart_df,
                                aes(
                                  x = factor(date, levels = unique(date)),
                                  y = coalesce(total_min, 0),
                                  fill = tag1,
                                  label = time_label,
                                )) +
  # geom_bar(stat = "identity") +
  geom_col(
    position = 'stack', 
    width = 0.75) +
  geom_text(
    aes(
      label = ifelse(total_min > 0, time_label, ""),
      group = tag1),
    position = position_stack(vjust = 0.5),
    size = 3,
    # fontface = "bold",
    family = "Roboto") +
  geom_hline(
    yintercept = 8 * 60,
    linetype = "dashed",
    linewidth = .5,
    color = "black") +
  annotate(
    geom = "text",
    x = 0, 
    y = 8 * 60,
    label = "Threshold: 8h",
    hjust = 0,
    vjust = -1,
    color = "black",
    size = 3
  ) +
  geom_hline(
    yintercept = sum(na.omit(bar_chart_df$total_min)) / 5,
    linetype = "dashed",
    linewidth = .5,
    color = "#4cc9f0") +
  annotate(
    geom = "text",
    x = 0, 
    y = sum(na.omit(bar_chart_df$total_min)) / 5,
    label = paste(
      'AVG hours per workday: ', 
      (sum(na.omit(bar_chart_df$total_min)) %/% 5) %/% 60, 
      'h ',
      (sum(na.omit(bar_chart_df$total_min)) %/% 5) %% 60, 
      'm',
      sep=''
    ),
    hjust = 0,
    vjust = -1,
    color = "#4cc9f0",
    size = 3
  ) +
  stat_summary(
    aes(
      label = ifelse(
        after_stat(y) %/% 60 == 0,
        sprintf(
          '%2dm', 
          after_stat(y)),
        sprintf(
          "%2dh %2dm", 
          after_stat(y) %/% 60, 
          after_stat(y) %% 60) ), 
      group = 1),
    fun = sum,
    geom = "text",
    vjust = -0.5,
    color = "#03254c",
    fontface = "bold",
    family = "Roboto",
    size = 4
  ) +
  labs(title = 'Weekly Activity Report',
       x = '',
       y = 'Time Dedicated',
       fill = '') +
  theme_minimal() +
  scale_y_continuous(
    breaks = seq(0, 540, 60),
    limits = c(0, 540) ) +
  scale_fill_brewer(type = 'seq') +
  theme(
    axis.text.y = element_blank(),
    # Remove left side labels
    axis.ticks.y = element_blank(),
    # Remove left side ticks
    panel.grid.major.x = element_blank(),
    # Remove horizontal grid lines
    panel.grid.minor.y = element_blank(),  # Remove minor horisontal grid lines
    panel.grid.minor.x = element_blank(),  # Remove vertical grid lines
    # panel.background = element_blank(), # Remove panel background
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.title = element_text(hjust = 0.5),
    text = element_text(family = "Roboto")
  )

# view plot
activity_per_day_test

# save bar chart
ggsave(
  activity_per_day_test,
  filename = glue('src/plots/{ Sys.Date() }_activity_per_day_test.jpg'),
  width = 20,
  height = 15,
  units = 'cm',
  dpi = 300
)

pie_chart_df <- restored_df |>
  group_by(tag1) |>
  summarise(total_min = sum(time_min))

# Create a pie chart using ggplot2
pie_chart <- ggplot(pie_chart_df, 
       aes(x = "", y = total_min, fill = tag1)) +
  geom_bar(
    stat = "identity",
    width = 1) +
  geom_text(
    aes(label = paste0(round(total_min / sum(total_min) * 100), "%")),
    position = position_stack(vjust = .5),
    color = "black",  # Set text color
    size = 3,
    fontface = "bold"
  ) +
  geom_text(
    aes(x = 1.65, label = paste0(total_min, " min")),
    position = position_stack(vjust = 0.5),
    color = "black",  # Set text color
    size = 3,
    fontface = "bold"
  ) +
  coord_polar(theta = "y") +
  theme_minimal() +
  scale_fill_brewer(type = 'seq') +
  labs(title = "Time Distribution Among Activities",
       fill = "Category") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
  
pie_chart <- pie_chart + 
  theme(legend.position = "bottom")

pie_chart

combined_plot <- activity_per_day_test + 
  pie_chart +
  plot_layout(ncol = 2, nrow = 2)

combined_plot


  
  # Heat map
  heat_map_df <- restored_df |>
  group_by(date) |>
  summarise(total_min = sum(time_min))


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
