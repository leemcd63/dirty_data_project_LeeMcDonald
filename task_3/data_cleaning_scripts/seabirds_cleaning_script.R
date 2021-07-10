library(tidyverse)
library(janitor)
library(here)
library(readxl)

# Read in dirty data
ship_data <- read_excel(here("raw_data/seabirds.xls"), sheet = "Ship data by record ID")
bird_data <- read_excel(here("raw_data/seabirds.xls"), sheet = "Bird data by record ID")

# Keep common name, scientific name, species abbreviation, record id, count, latitude
# Clean bird data names, rename columns, select rows needed for analysis.
bird_data_tidy <- bird_data %>%
  clean_names() %>%
  rename("species_common_name" = starts_with("species_common"),
         "species_scientific_name" = starts_with("species_scientific")) %>%
  select(record_id, species_common_name, species_scientific_name, species_abbreviation, count)

# Clean ship data names, keep only record id for joining and latitude for analysis
ship_data_tidy <- ship_data %>%
  clean_names() %>%
  select(record_id, lat)

# Join tidy tables, drop NA values from count and latitude
seabirds_joined <- bird_data_tidy %>%
  inner_join(ship_data_tidy, by = "record_id") %>%
  drop_na(count, lat)

# Write to .csv
write_csv(seabirds_joined, here("clean_data/seabirds_cleaned.csv"))
  


