library(tidyverse)
library(janitor)
library(here)

decathlon_data <- read_rds(here("raw_data/decathlon.rds"))

decathlon_cleaned <- decathlon_data %>%
  # Change row names to column called "name"
  rownames_to_column(var = "name") %>%
  # Create unique id for each athlete
  mutate(name = str_to_title(name)) %>%
  group_by(name) %>%
  mutate(athlete_id = cur_group_id()) %>%
  ungroup() %>%
  # rearrange columns and drop columns not needed for analysis
  select(athlete_id, name, Competition, Points, Rank, Long.jump, "100m", Shot.put, "400m") %>%
  # Convert into long format
  pivot_longer(Long.jump:"400m", names_to = "event", values_to = "score") %>%
  # Create id for each observation
  mutate(id = row_number()) %>%
  # Move id column to front
  select(id, everything()) %>%
  # clean column names
  clean_names() %>%
  # replace "." in event names with a space
  mutate(event = str_replace(event, "\\.", " "))

# Write data to .csv
write_csv(decathlon_cleaned, here("clean_data/decathlon_cleaned.csv"))
