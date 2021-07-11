library(tidyverse)
library(janitor)
library(here)
library(readxl)

# Read in dirty data
candy_data_2015 <- read_xlsx(here("raw_data/boing-boing-candy-2015.xlsx"))
candy_data_2016 <- read_xlsx(here("raw_data/boing-boing-candy-2016.xlsx"))
candy_data_2017 <- read_xlsx(here("raw_data/boing-boing-candy-2017.xlsx"))

# Create function to check if column contains rating
is_rating <- function(col) {
  ratings <- list("JOY", "MEH", "DESPAIR")
  for (x in ratings) {
    if (x %in% col) {
      return(TRUE)
    }
    return(FALSE)
  }
}

# Cleaning 2015 data

candy_2015_trimmed <- candy_data_2015 %>%
  # rename age and trick or treat columns
  rename("age" = 2,
         "trick_or_treat" = 3) %>%
  # convert age to integer, coercing characters to NA
  mutate(age = as.integer(age),
  # add unique "id", combination of row number and year
          id = str_c("15-", row_number()),
          year = 2015) %>%
  # keep id, year, age and trick or treat columns, then remove
  # columns which don't contain ratings
  select(id,
         year,
         age,
         trick_or_treat,
         where(~is_rating(.x)),
         -starts_with("Please"))

candy_2015_long <- candy_2015_trimmed %>%
  # convert to long format
  pivot_longer(5:ncol(candy_2015_trimmed), 
               names_to = "candy_name", 
               values_to = "rating") %>%
  # drop NA values from rating
  drop_na(rating) %>%
  # remove brackets from candy names
  mutate(candy_name = str_sub(candy_name, 2, -2))
  
# Cleaning 2016 data

candy_2016_trimmed <- candy_data_2016 %>%
  # rename trick or treat, age, gender and country columns
  rename("trick_or_treat" = 2,
         "gender" = 3,
         "age" = 4,
         "country" = 5) %>%
  # convert age to integer, coercing characters to NA
  mutate(age = as.integer(age),
  # add unique "id" combination of row number and year
         id = str_c("16-", row_number()),
         year = 2016) %>%
  # keep id, year, age, trick or treat, gender and country columns, remove
  # columns which don't contain ratings
  select(id,
         year,
         age,
         country,
         gender,
         trick_or_treat,
         where(~is_rating(.x)),
         -starts_with("Please"))

candy_2016_long <- candy_2016_trimmed %>%
  # convert to long format
  pivot_longer(7:ncol(candy_2016_trimmed), 
               names_to = "candy_name", 
               values_to = "rating") %>%
  # drop NA values from rating
  drop_na(rating) %>%
  # remove brackets from candy names
  mutate(candy_name = str_sub(candy_name, 2, -2))

# Cleaning 2017 data
candy_2017_trimmed <- candy_data_2017 %>%
  # rename trick or treat, age, gender and country columns
  rename("trick_or_treat" = 2,
         "gender" = 3,
         "age" = 4,
         "country" = 5) %>%
  # convert age to integer, coercing characters to NA
  mutate(age = as.integer(age),
         # add unique "id" combination of row number and year
         id = str_c("17-", row_number()),
         year = 2017) %>%
  # keep id, year, age, trick or treat, gender and country columns, remove
  # columns which don't contain ratings
  select(id,
         year,
         age,
         country,
         gender,
         trick_or_treat,
         where(~is_rating(.x)))

candy_2017_long <- candy_2017_trimmed %>%
  # convert to long format
  pivot_longer(7:ncol(candy_2017_trimmed), 
               names_to = "candy_name", 
               values_to = "rating") %>%
  # drop NA values from rating
  drop_na(rating) %>%
  # remove "brackets"Q6 |" from candy names
  mutate(candy_name = str_sub(candy_name, 6))


# Join tables
candy_data_complete <- candy_2015_long %>%
  full_join(candy_2016_long) %>%
  full_join(candy_2017_long)

# Check distinct country names
distinct_countries <- candy_data_complete %>%
  distinct(country) %>%
  arrange(country)

# Create regex strings for recoding
usa_string <- "u.s.|us|u s|amer|states|united s|rica|murrika|new y|cali|alaska|trump|pittsburgh|carolina|cascadia|yoo ess|jersey"
uk_string <- "uk|united k|england|endland|scotland"
na_string <- "0|one|never|some|god|of|^a$|see|eua|denial|insanity|know|atlantis|fear|narnia|earth|europe"

candy_countries_recoded <- candy_data_complete %>%
  mutate(country = str_to_lower(country),
    country = case_when(
    str_detect(country, usa_string) ~ "United States",
    str_detect(country, uk_string) ~ "United Kingdom",
    str_detect(country, "can") ~ "Canada",
    str_detect(country, "neth") ~ "Netherlands",
    str_detect(country, "esp") ~ "Spain",
    str_detect(country, "uae") ~ "United Arab Emirates",
    str_detect(country, na_string) ~ NA_character_,
    TRUE ~ country),
    country = str_to_title(country)
  )

# Check distinct candy names, then recode
distinct_candy <- candy_countries_recoded %>%
  distinct(candy_name) %>%
  arrange(candy_name)

candy_names_recoded <- candy_countries_recoded %>%
  mutate(candy_name = case_when(
    str_detect(candy_name, "Anonymous brown") ~ "Mary Janes",
    str_detect(candy_name, "Raisins") ~ "Box 'o' Raisins",
    str_detect(candy_name, "the board game") ~ "Bonkers",
    str_detect(candy_name, "JoyJoy") ~ "JoyJoy",
    str_detect(candy_name, "yes black") ~ "Licorice",
    TRUE ~ candy_name)
  )

distinct_candy <- candy_names_recoded %>%
  distinct(candy_name) %>%
  arrange(candy_name)
