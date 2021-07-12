library(tidyverse)
library(janitor)
library(here)

dog_survey <- read_csv(here("raw_data/dog_survey.csv"))

# remove two empty columns on the end.

dog_survey_clean <- dog_survey %>%
  select(1:9) %>%
  distinct() %>%
  separate_rows(dog_size, dog_gender, dog_age, sep = "and") %>%
  separate_rows(dog_size, dog_gender, dog_age, sep = ",") %>%
  mutate(
    amount_spent_on_dog_food = as.numeric(
      str_extract(amount_spent_on_dog_food, "(?<=£)\\d+\\.?\\d*"), digits = 2),
    dog_size = case_when(
      dog_size == "Smallish" ~ "S",
      dog_size == "Medium sized" ~ "M",
      dog_size == "large" ~ "L",
      str_detect(dog_size, "XS|S|M|L|XL") ~ dog_size,
      TRUE ~ NA_character_
    ),
    dog_gender = str_to_lower(dog_gender),
    dog_gender = case_when(
      str_detect(dog_gender, "fem|^f$") ~ "Female",
      str_detect(dog_gender, "male|^m$") ~"Male",
      TRUE ~ NA_character_
    ),
    dog_age = as.integer(
      str_extract(dog_age, "\\d+")
    )
  ) %>%
  clean_names() %>%
  rename(food_spend = amount_spent_on_dog_food)

write_csv(dog_survey_clean, here("clean_data/dog_survey_clean.csv"))
