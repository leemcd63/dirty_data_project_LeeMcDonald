library(tidyverse)
library(janitor)
library(here)

# Read in cake ingredient data and cake code data
cake_ingredients <- read_csv(here("raw_data/cake-ingredients-1961.csv"))
cake_ingredient_names <- read_csv(here("raw_data/cake_ingredient_code.csv"))

# Pivot into long format and drop NA values in quantity then clean names
cake_ingredients_long <- cake_ingredients %>%
  pivot_longer(-Cake, names_to = "code", values_to = "quantity") %>%
  drop_na(quantity) %>%
  clean_names()

# Join tables by cake codes
cake_joined <- cake_ingredients_long %>%
  left_join(cake_ingredient_names, by = c("code" = "code")) 

# There are some NA values by Sour cream where it should say "cup", replace the
# "Sour cream cup" strings with "Sour cream" and replace NA with "cup.
cake_tidy <- cake_joined %>%
  mutate(ingredient = str_replace_all(ingredient, "Sour cream cup", "Sour cream"),
         measure = replace_na(measure, "cup")) %>%
  # remove code from table
  select(-code)

# write cleaned data to .csv
write_csv(cake_tidy, here("clean_data/cake_ingredients_cleaned.csv"))
          