library(tidyverse)
library(janitor)
library(here)
library(readxl)

# Read in dirty data ----
candy_data_15 <- read_xlsx(here("raw_data/boing-boing-candy-2015.xlsx"))
candy_data_16 <- read_xlsx(here("raw_data/boing-boing-candy-2016.xlsx"))
candy_data_17 <- read_xlsx(here("raw_data/boing-boing-candy-2017.xlsx"))

# General function for candy data sets to select columns, clean column names and
# convert to long format ----
tidy_candy_data <- function(df, df_year) {
  df %>%
    # rename columns needed for analysis
    rename(age = matches(" old "),
           age = matches(" age"),
           trick_or_treat = matches("going"),
           gender = matches("gender"),
           country = matches("country"),
           other_joy = matches("JOY", ignore.case = FALSE),
           other_despair = matches("DESPAIR", ignore.case = FALSE)) %>%
    # convert age to integer, coercing characters to NA
    mutate(age = as.integer(age),
    # create unique rater_id based on year and row number
           rater_id = str_c(str_extract(df_year, "[0-9]{2}$"), "-", row_number()),
    # create year column
           year = df_year) %>%
    # pivot longer all candy rating columns
    pivot_longer(cols = c(starts_with("Q6"), starts_with("[") & ends_with("]")),
                 names_to = "candy_name",
                 values_to = "rating") %>%
    # select/rearrange columns
    select(rater_id, 
           year, 
           age,
           starts_with("gender"),
           starts_with("country"),
           trick_or_treat,
           candy_name,
           rating,
           other_joy,
           other_despair) %>%
    # remove any missing ratings
    drop_na(rating) %>%
    # remove "Q6 | " from candy names if it's 2017's data, otherwise remove brackets (2015, 2016)
    mutate(candy_name = if_else(year == 2017, 
                                str_remove(candy_name, "Q6 \\| "), 
                                str_remove_all(candy_name, "[\\[\\]]")))
}


# Use function to tidy candy datasets ----
candy_15_long <- tidy_candy_data(candy_data_15, 2015)
candy_16_long <- tidy_candy_data(candy_data_16, 2016)
candy_17_long <- tidy_candy_data(candy_data_17, 2017)

# Join tables ----
candy_joined <- candy_15_long %>%
  bind_rows(candy_16_long) %>%
  bind_rows(candy_17_long)

# Separate "other_x" columns from joined table ----
other_joy_ratings <- candy_joined %>%
  select(rater_id, other_joy) %>%
  drop_na() %>%
  distinct()
  
other_despair_ratings <- candy_joined %>%
  select(rater_id, other_despair) %>%
  drop_na() %>%
  distinct()

rater_data <- candy_joined %>%
  select(rater_id,
         year,
         age,
         gender,
         country,
         trick_or_treat) %>%
  distinct()

candy_ratings <- candy_joined %>%
  select(rater_id,
         candy_name,
         rating)

# Separate rows in other ratings, add ratings column then rejoin tables ----
other_joy_separated <- other_joy_ratings %>%
  mutate(other_joy = str_to_lower(other_joy)) %>%
  separate_rows(other_joy, sep = ",") %>%
  separate_rows(other_joy, sep = regex(" and ")) %>%
  separate_rows(other_joy, sep = regex(" or ")) %>%
  separate_rows(other_joy, sep = regex("(?<!mr|mrs|mt)\\.")) %>%
  separate_rows(other_joy, sep = regex("[0-9]\\)")) %>%
  separate_rows(other_joy, sep = regex("\\!+ (?!$)")) %>%
  mutate(other_joy = str_replace_all(other_joy, "[:punct:]", ""),
         other_joy = str_trim(other_joy, side = "both"),
         rating = "JOY") %>%
  rename(candy_name = other_joy)

other_despair_separated <- other_despair_ratings %>%
  mutate(other_despair = str_to_lower(other_despair)) %>%
  separate_rows(other_despair, sep = ",") %>%
  separate_rows(other_despair, sep = regex(" and ")) %>%
  separate_rows(other_despair, sep = regex(" or ")) %>%
  separate_rows(other_despair, sep = regex("(?<!mr|mrs|mt)\\.")) %>%
  separate_rows(other_despair, sep = regex("[0-9]\\)")) %>%
  separate_rows(other_despair, sep = regex("\\!+ (?!$)")) %>%
  mutate(other_despair = str_replace_all(other_despair, "[:punct:]", ""),
         other_despair = str_trim(other_despair, side = "both"),
         rating = "DESPAIR") %>%
  rename(candy_name = other_despair)

other_candy_ratings <- other_joy_separated %>%
  bind_rows(other_despair_separated) %>%
  distinct()

# Find distinct "candy names" ---
distinct_other_candy <- other_candy_ratings %>%
  select(candy_name) %>%
  distinct() %>%
  arrange(candy_name)

# Recode other_ratings ----
other_candy_recoded <- other_candy_ratings %>%
  mutate(candy_name = case_when(
    str_detect(candy_name, "100 dark|100 g|000 bar") ~ "100 Grand Bar",
    str_detect(candy_name, "7up") ~ "Seven Up Bar",
    str_detect(candy_name, "fifth|5th|avenue") ~ "5th Avenue Bar",
    str_detect(candy_name, "hersey dark|hersheys dark") ~ "Hershey's Dark Chocolate",
    str_detect(candy_name, "cookies n cr|hershey coo|heresys coo|hershey coo") ~ "Hershey's Cookies n Cream",
    str_detect(candy_name, "kisses") ~ "Hershey's Kisses",
    str_detect(candy_name, "hershey|hersey") ~ "Other Hershey's",
    str_detect(candy_name, "85|90|dark chocolate$| 60") ~ "Dark Chocolate",
    str_detect(candy_name, "ice cream") ~ "Ice Cream",
    str_detect(candy_name, "e pieces|s pieces") ~ "Reese's Pieces",
    str_detect(candy_name, "reece|reece") ~ "Reese's Peanut Butter Cups",
    str_detect(candy_name, "muffin") ~ "Muffin",
    str_detect(candy_name, "abazaba|abba |abbaz") ~ "Abba Zabba",
    str_detect(candy_name, "cookie dough") ~ "Cookie Dough",
    str_detect(candy_name, "aero") ~ "Aero",
    str_detect(candy_name, "after|andes") ~ "After Dinner Mints",
    str_detect(candy_name, "airheads|air heads|aid h") ~ "Airheads",
    str_detect(candy_name, "almond joy|almond despair|allman") ~ "Almond Joy",
    str_detect(candy_name, "snickers") ~ "Snickers",
    str_detect(candy_name, "kinder b") ~ "Kinder Bueno",
    str_detect(candy_name, "kinder") ~ "Kinder Surprise",
    str_detect(candy_name, "warheads|war h") ~ "Warheads",
    str_detect(candy_name, "mms|m m s|mm s|m ms|mm") ~ "M&M's",
    str_detect(candy_name, "smarties") ~ "Smarties",
    str_detect(candy_name, "cookie") ~ "Cookie",
    str_detect(candy_name, "tootsie|toot|toost") ~ "Tootsie Roll",
    str_detect(candy_name, "atomic|fireballs") ~ "Atomic Fireballs",
    str_detect(candy_name, "babe|baby r") ~ "Baby Ruth",
    str_detect(candy_name, "bounty") ~ "Bounty Bar",
    str_detect(candy_name, "mars ") ~ "Mars Bar",
    str_detect(candy_name, "nougat") ~ "Nougat",
    str_detect(candy_name, "butterfinger|butter finger") ~ "Butterfinger",
    str_detect(candy_name, "butterscotch") ~ "Butterscotch Candy",
    str_detect(candy_name, "dairy milk") ~ "Dairy Milk",
    str_detect(candy_name, "creme egg") ~ "Creme Egg",
    str_detect(candy_name, "cadbury") ~ "Cadbury Other",
    str_detect(candy_name, "cane") ~ "Candy Cane",
    str_detect(candy_name, "candy corn") ~ "Candy Corn",
    str_detect(candy_name, "floss") ~ "Candy Floss",
    str_detect(candy_name, "candied ap|candy ap|caramel ap|candy coated ap|taffy ap|toffee ap|carmel ap") ~ "Candy Apple",
    str_detect(candy_name, "popcorn") ~ "Popcorn",
    str_detect(candy_name, "caramilk") ~ "Caramilk",
    str_detect(candy_name, "charl") ~ "Charlston Chews",
    str_detect(candy_name, "jelly bean| jelly b") ~ "Jelly Beans",
    str_detect(candy_name, "chunky") ~ "Chunky Bar",
    str_detect(candy_name, "crunch") ~ "Crunch Bar",
    str_detect(candy_name, "wurly") ~ "Curly Wurly",
    str_detect(candy_name, "kit kat|kitkat") ~ "Kit Kat",
    str_detect(candy_name, "milky|mikly") ~ "Milky Way",
    str_detect(candy_name, "dove") ~ "Dove Bar",
    str_detect(candy_name, "haribo") ~ "Haribo",
    str_detect(candy_name, "gummi|gummy") ~ "Gummy Sweets",
    str_detect(candy_name, "gum") ~ "Bubble Gum",
    str_detect(candy_name, "easter") ~ "Easter Egg",
    str_detect(candy_name, "gobstop|gob stop") ~ "Gobstopper",
    str_detect(candy_name, "ferr") ~ "Ferrero Rocher",
    str_detect(candy_name, "flake") ~ "Flake",
    str_detect(candy_name, "fruit r") ~ "Fruit Roll-ups",
    str_detect(candy_name, "fudge") ~ "Fudge",
    str_detect(candy_name, "gingerbread") ~ "Gingerbread",
    str_detect(candy_name, "skittle") ~ "Skittles",
    str_detect(candy_name, "twink") ~ "Twinkies",
    str_detect(candy_name, "licorice|liquorice") ~ "Licorice",
    str_detect(candy_name, "life s|lifesa") ~ "Life Savers",
    str_detect(candy_name, "lindt") ~ "Lindt",
    str_detect(candy_name, "lion") ~ "Lion Bar",
    str_detect(candy_name, "marsh") ~ "Marshmallows",
    str_detect(candy_name, "marathon") ~ "Marathon Bar",
    str_detect(candy_name, "mento") ~ "Mentos",
    str_detect(candy_name, "mike") ~ "Mike and Ike",
    str_detect(candy_name, "mounds") ~ "Mounds Bar",
    str_detect(candy_name, "mr g|goodbar") ~ "Mr Goodbar",
    str_detect(candy_name, "necco") ~ "Necco Wafers",
    str_detect(candy_name, "nerd") ~ "Nerds",
    str_detect(candy_name, "nutella") ~ "Nutella",
    str_detect(candy_name, "oh henry|ohhenry|ohenry|ohenry") ~ "Oh Henry Bar",
    str_detect(candy_name, "pay day|payday") ~ "Payday Bar",
    str_detect(candy_name, "pumpkin") ~ "Pimpkin Flavoured Candy",
    str_detect(candy_name, "ritter") ~ "Ritter Bar",
    str_detect(candy_name, "saltwater|salt w") ~ "Saltwater Taffy",
    str_detect(candy_name, "sour pa") ~ "Sour Patch Kids",
    str_detect(candy_name, "sour") ~ "Other Sour Candy",
    str_detect(candy_name, "starbur") ~ "Starburst",
    str_detect(candy_name, "swedish f") ~ "Swedish Fish",
    str_detect(candy_name, "hard cand") ~ "Hard Candy",
    str_detect(candy_name, "twix") ~ "Twix",
    str_detect(candy_name, "twiz") ~ "Twizzlers",
    str_detect(candy_name, "werther") ~ "Werthers Originals",
    str_detect(candy_name, "white ch") ~ "White Chocolate",
    str_detect(candy_name, "zero ") ~ "Zero Bars",
    str_detect(candy_name, "milk cho") ~ "Milk Chocolate",
    str_detect(candy_name, "donut|doughnut") ~ "Doughnuts",
    str_detect(candy_name, "85|90|dark chocolate$| 60") ~ "Dark Chocolate",
    TRUE ~ NA_character_
    ) 
  ) %>%
  drop_na()

# Join with candy ratings table and rater_data ----
candy_ratings_complete <- candy_ratings %>%
  bind_rows(other_candy_recoded)
  
candy_ratings_complete <- rater_data %>%
  inner_join(candy_ratings_complete, by = "rater_id")

# Check distinct country names ----
distinct_countries <- candy_ratings_complete %>%
  distinct(country) %>%
  arrange(country)

# Create regex patterns for recoding, then recode ----
usa_pattern <- "u.s.|us|u s|amer|states|united s|rica|murrika|new y|cali|alaska|trump|pittsburgh|carolina|cascadia|yoo ess|jersey"
uk_pattern <- "uk|united k|england|endland|scotland"
na_pattern <- "0|one|never|some|god|of|^a$|see|eua|denial|insanity|know|atlantis|fear|narnia|earth|europe"

candy_countries_recoded <- candy_ratings_complete %>%
  mutate(country = str_to_lower(country),
    country = case_when(
    str_detect(country, usa_pattern) ~ "United States",
    str_detect(country, uk_pattern) ~ "United Kingdom",
    str_detect(country, "can") ~ "Canada",
    str_detect(country, "neth") ~ "Netherlands",
    str_detect(country, "esp") ~ "Spain",
    str_detect(country, "uae") ~ "United Arab Emirates",
    str_detect(country, na_pattern) ~ NA_character_,
    TRUE ~ country),
    country = str_to_title(country)
  )

# Check distinct candy names, then recode ----
distinct_candy <- candy_countries_recoded %>%
  distinct(candy_name) %>%
  arrange(candy_name)

candy_na_pattern <- "the board game|low stick|Bottle Caps|Brach products|Chardonnay|Chick-o|Religious|Peaches|Aceta|Hugs|Kale|DVD|Blue-Ray|BooBerry|Sea-salt|Dick|Those|Vicodin|Vials|White Bread|Cash|Dental|chips|Sidewalk|Wheat"

candy_cleaned <- candy_countries_recoded %>%
  mutate(candy_name = case_when(
    str_detect(candy_name, candy_na_pattern) ~ NA_character_,
    str_detect(candy_name, "Anonymous brown") ~ "Mary Janes",
    str_detect(candy_name, "Raisins") ~ "Box 'o' Raisins",
    str_detect(candy_name, "(the candy)") ~ "Bonkers",
    str_detect(candy_name, "JoyJoy") ~ "JoyJoy",
    str_detect(candy_name, "Licorice") ~ "Licorice",
    str_detect(candy_name, "Dark Chocolate Hershey") ~ "Hershey's Dark Chocolate",
    str_detect(candy_name, "Sweetums") ~ "Sweetums",
    str_detect(candy_name, "M&M") ~ "M&M's",
    str_detect(candy_name, "Mars") ~ "Mars Bar",
    str_detect(candy_name, "restaurants") ~ "Generic Candy",
    str_detect(candy_name, "Dove") ~ "Dove Bar",
    str_detect(candy_name, "Kissables") ~ "Hershey's Kisses",
    str_detect(candy_name, "Jolly") ~ "Jolly Rancher",
    str_detect(candy_name, "Lindt") ~ "Lindt",
    str_detect(candy_name, "Mars") ~ "Mars Bar",
    str_detect(candy_name, "Goodbar") ~ "Mr. Goodbar",
    str_detect(candy_name, "Nestle Crunch") ~ "Crunch Bar",
    str_detect(candy_name, "Smarties") ~ "Smarties",
    str_detect(candy_name, "Sourpatch") ~ "Sour Patch Kids",
    str_detect(candy_name, "Tolberone") ~ "Toblerone",
    str_detect(candy_name, "Gummy") ~ "Gummy Bears",
    str_detect(candy_name, "Gum") ~ "Bubble Gum",
    str_detect(candy_name, "Reese’s Peanut Butter Cups") ~ "Reese's Peanut Butter Cups",
    TRUE ~ candy_name)
  )

distinct_candy_check <- candy_cleaned %>%
  distinct(candy_name) %>%
  arrange(candy_name)

# write clean data to .csv ----
write_csv(candy_cleaned, here("clean_data/candy_data_cleaned.csv"))
