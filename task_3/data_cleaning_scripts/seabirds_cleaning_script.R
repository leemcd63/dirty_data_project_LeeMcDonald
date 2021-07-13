library(tidyverse)
library(janitor)
library(here)
library(readxl)
library(rlang)

# function to check for plummage code in columns and remove ----
remove_plummage <- function(x) {
  if (is.character(x)) {
    case_when(
      str_detect(x, " PL[\\d]*$") ~ str_remove(x, " PL[\\d]*$"),
      str_detect(x, " DRK$") ~ str_remove(x, " DRK$"),
      str_detect(x, " INT$") ~ str_remove(x, " INT$"),
      str_detect(x, " LGHT$") ~ str_remove(x, " LGHT$"),
      str_detect(x, " LIGHT$") ~ str_remove(x, " LIGHT$"),
      str_detect(x, " WHITE$") ~ str_remove(x, " WHITE$"),
      TRUE ~ x
    ) } else {
      x
  }
}

# function to create new age column if age code is present (THANKS DAVID) ----
create_age_column <- function(df,col) {
  col <- enquo(col)
  df %>%
    mutate(age = case_when(
      str_detect(!!col, " AD$") ~ "Adult",
      str_detect(!!col, " SUBAD$") ~ "Subadult",
      str_detect(!!col, " IMM$") ~ "Immature",
      str_detect(!!col, " JUV$") ~ "Juvenile",
      TRUE ~ "Not specified"
    )
    )
}

# function to check for age code in columns and remove ----
remove_ages <- function(x) {
  if (is.character(x)) {
    case_when(
      str_detect(x, " AD$") ~ str_remove(x, " AD$"),
      str_detect(x, " SUBAD$") ~ str_remove(x, " SUBAD$"),
      str_detect(x, " IMM$") ~ str_remove(x, " IMM$"),
      str_detect(x, " JUV$") ~ str_remove(x, " JUV$"),
      TRUE ~ x
    ) 
    } else {
      x
  }
}

# function to remove additional information ----
remove_additional <- function(x) {
  if (is.character(x)) {
    case_when(
      str_detect(x, " \\([uU]nidentified\\)$") ~ str_remove(x, " \\([uU]nidentified\\)$"),
      str_detect(x, " sensu lato$") ~ str_remove(x, " sensu lato$"),
      TRUE ~ x
    )
  } else {
    x
  }
}
  
# Read in dirty data ----
ship_data <- read_excel(here("raw_data/seabirds.xls"), sheet = "Ship data by record ID")
bird_data <- read_excel(here("raw_data/seabirds.xls"), sheet = "Bird data by record ID")

# Keep common name, scientific name, species abbreviation, record id, count, latitude ----
# Clean bird data names, rename columns, select rows needed for analysis.
bird_data_tidy <- bird_data %>%
  clean_names() %>%
  rename(common_name = starts_with("species_common"),
         scientific_name = starts_with("species_scientific"),
         abbreviation = species_abbreviation) %>%
  select(record_id, common_name, scientific_name, abbreviation, count)

# Clean ship data names, keep only record id for joining and latitude for analysis ----
ship_data_tidy <- ship_data %>%
  clean_names() %>%
  select(record_id, lat)

# Join tidy tables, drop NA values from count ----
seabirds_joined <- bird_data_tidy %>%
  inner_join(ship_data_tidy, by = "record_id") %>%
  drop_na(count) %>%
  # create unique id for each observation, then move to first column
  mutate(id = row_number()) %>%
  select(id, everything())

# use function to remove plummage codes ----
seabirds_no_plum_code <- as_tibble(lapply(seabirds_joined, remove_plummage))

# use function to create age column ----
seabirds_with_ages <- create_age_column(seabirds_no_plum_code, common_name)

# use function to remove age codes ---- 
seabirds_no_age_code <- as_tibble(lapply(seabirds_with_ages, remove_ages))

# use function to remove additional information ----
seabirds_cleaned<- as_tibble(lapply(seabirds_no_age_code, remove_additional))

# write clean data to .csv ----
write_csv(seabirds_cleaned, here("clean_data/seabirds_cleaned.csv"))
  
