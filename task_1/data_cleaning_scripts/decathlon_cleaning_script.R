library(tidyverse)
library(janitor)
library(here)

decathlon_data <- read_rds(here("raw_data/decathlon.rds"))

decathlon_cleaned <- decathlon_data %>%
# Change row names to column called "name"
  rownames_to_column(var = "name") %>%
# Convert names to same case
  mutate(name = str_to_sentence(name)) %>%
# Create unique id for each name
  group_by(name) %>%
  mutate(id = cur_group_id()) %>%
# Clean names, rearrange columns and drop columns not needed for analysis
  clean_names() %>%
  select(id, name, competition, points, long_jump, x100m, shot_put, x400m)

# Write data to .csv
write_csv(decathlon_cleaned, here("clean_data/decathlon_cleaned.csv"))
