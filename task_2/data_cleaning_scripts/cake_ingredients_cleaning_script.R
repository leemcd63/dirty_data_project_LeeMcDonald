library(tidyverse)
library(janitor)
library(here)

# Read in cake ingredient data and cake code data
cake_ingredients <- read_csv(here("raw_data/cake-ingredients-1961.csv"))
ingredient_names <- read_csv(here("raw_data/cake_ingredient_code.csv"))

cake_ingredients_long <- cake_ingredients %>%
  # arrange cakes by alphabetical order
  arrange(Cake) %>%
  # create unique cake_id column
  mutate(cake_id = row_number()) %>%
  # Pivot into long format, drop NA values in quantity then clean names
  pivot_longer(-c(Cake, cake_id), names_to = "code", values_to = "quantity") %>%
  drop_na(quantity) %>%
  clean_names()

# Sour cream in cake_names has "cup" where it should be in the NA value in measure,
# remove and replace in correct column
ingredient_names_fixed <- ingredient_names %>%
  mutate(ingredient = str_remove(ingredient, " cup$"),
         measure = replace_na(measure, "cup"))

# Join tables by cake codes
cake_joined <- cake_ingredients_long %>%
  left_join(ingredient_names_fixed, by = "code") %>%
  # create id column for each observation
  mutate(id = row_number()) %>%
  # rearrange columns, remove code
  select(id, cake_id, everything(), -code)


# write cleaned data to .csv
write_csv(cake_joined, here("clean_data/cake_ingredients_cleaned.csv"))
          