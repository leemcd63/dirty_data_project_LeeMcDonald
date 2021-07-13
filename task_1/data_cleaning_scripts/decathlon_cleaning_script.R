library(tidyverse)
library(janitor)
library(here)

decathlon_data <- read_rds(here("raw_data/decathlon.rds"))

decathlon_cleaned <- decathlon_data %>%
  # Change row names to column called "name"
  rownames_to_column(var = "name") %>%
  # Create id for each observation, convert athlete names to same case
  mutate(id = row_number(),
         name = str_to_sentence(name)) %>%
  # Create unique id for each name
  group_by(name) %>%
  mutate(athlete_id = cur_group_id()) %>%
  # rearrange columns and drop columns not needed for analysis
  select(id, athlete_id, name, Competition, Points, Rank, Long.jump, "100m", Shot.put, "400m") %>%
  # Convert into long format
  pivot_longer(Long.jump:"400m", names_to = "event", values_to = "score") %>%
  # clean column names
  clean_names() %>%
  # replace "." in event names with a space
  mutate(event = str_replace(event, "\\.", " "))

# Write data to .csv
write_csv(decathlon_cleaned, here("clean_data/decathlon_cleaned.csv"))
