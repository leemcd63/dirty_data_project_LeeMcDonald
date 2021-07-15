library(tidyverse)
library(janitor)
library(here)

# Read in dirty data
dog_survey <- read_csv(here("raw_data/dog_survey.csv"))

dog_survey_clean <- dog_survey %>%
  # remove two empty columns on the end.
  select(1:9) %>%
  # remove duplicate rows
  distinct() %>%
  # clean up names
  clean_names() %>%
  # rename amount_spent_on_dog_food
  rename(food_spend = amount_spent_on_dog_food) %>%
  # use separate_rows to split rows containing data for more than one dog
  separate_rows(dog_size, dog_gender, dog_age, sep = "and") %>%
  separate_rows(dog_size, dog_gender, dog_age, sep = ",") %>%
  mutate(
    # fix ".com" email addresses with multiple ms on the end,
    # check for actual email addresses, force NAs on non-valid entries
    email = case_when(
      str_detect(email, "\\.comm+$") ~ str_replace(email, "\\.comm+$", ".com"),
      str_detect(email, "@\\w+-*\\w*\\.") ~ email,
      TRUE ~ NA_character_
    ),
    # extract prices from food_spend column, convert to numeric type
    food_spend = as.numeric(
      str_extract(food_spend, "(?<=£)\\d+\\.?\\d*"), digits = 2),
    # recode dog_size column, non-valid entries to be "Not specified"
    dog_size = case_when(
      dog_size == "Smallish" ~ "S",
      dog_size == "Medium sized" ~ "M",
      dog_size == "large" ~ "L",
      str_detect(dog_size, "XS|S|M|L|XL") ~ dog_size,
      TRUE ~ "Not specified"
    ),
    # recode dog_gender column, non-valid entries to be "Not specified"
    dog_gender = str_to_lower(dog_gender),
    dog_gender = case_when(
      str_detect(dog_gender, "fem|^f$") ~ "Female",
      str_detect(dog_gender, "male|^m$") ~"Male",
      TRUE ~ "Not specified"
    ),
    # extract dog_age from strings, convert to integer
    dog_age = as.integer(
      str_extract(dog_age, "\\d+")
    ),
    # renew id column to make unique after splitting rows
    id = row_number()
  )

# write clean data to .csv
write_csv(dog_survey_clean, here("clean_data/dog_survey_clean.csv"))
