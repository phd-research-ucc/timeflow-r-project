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
  'src/data/raw/2023-11-26_timeflow_main.csv', 
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
  group_by(date, tag1) |>
  summarise(total_time = sum(time_min))





# Charts ------------------------------------------------------------------

ggplot(
  summary_df, 
  aes(x = date, y = total_time, fill = tag1) ) +
  geom_col(position = "stack", width = 0.7) +
  geom_text(aes(label = total_time), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Total Time for Each Activity per Day",
       x = "Date",
       y = "Total Time (minutes)",
       fill = "Activity") +
  theme_minimal()
