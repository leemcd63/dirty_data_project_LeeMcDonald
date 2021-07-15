library(tidyverse)
library(janitor)
library(here)

rwa_data <- read_csv(here("raw_data/rwa.csv"))

# Set variables of relevant questions for RWA score
rwa_score_qs <- c("Q3", "Q5", "Q7", "Q10", "Q12", "Q14", "Q16", "Q17", "Q19", "Q22")
rwa_score_qs_inv <- c("Q4", "Q6", "Q8", "Q9", "Q11", "Q13", "Q15", "Q18", "Q20", "Q21")


rwa_data_trimmed <- rwa_data %>%
  # filter out any rows where a question has a zero value (i.e, not answered)
  filter(!if_any(starts_with("Q"), ~ . == 0)) %>%
  # Calculate rwa_score for each row. mean of the sums of answers to each question,
  # Questions in "inv" variable are reverse scored, take the total away from 100 (max possible)
  rowwise() %>%
  mutate(rwa_score_a = sum(c_across(any_of(rwa_score_qs))),
         rwa_score_b = sum(c_across(any_of(rwa_score_qs_inv))),
         rwa_score = (rwa_score_a + (100 - rwa_score_b)) / 20) %>%
  ungroup() %>%
  # create unique id for each user
  mutate(id = rownames(.)) %>%
  # select columns for analysis
  select(id,
         rwa_score,
         testelapse,
         gender,
         age,
         hand,
         urban,
         familysize,
         education) %>%
  # rename a couple
  rename(time_secs = testelapse,
         family_size = familysize,
         childhood = urban)
  
# recode values according to rwa_codebook.txt
rwa_recoded <- rwa_data_trimmed %>%
  mutate(
    age = case_when(
      age <= 100 ~ age,
      TRUE ~ NA_real_
    ),
    gender = case_when(
      gender == 1 ~ "1. Male",
      gender == 2 ~ "2. Female",
      gender == 3 ~ "3. Other",
      TRUE ~ "Not specified"
    ),
    hand = case_when(
      hand == 1 ~ "1. Right",
      hand == 2 ~ "2. Left",
      hand == 3 ~ "3. Both",
      TRUE ~ "Not specified"
    ),
    childhood = case_when(
      childhood == 1 ~ "1. Rural",
      childhood == 2 ~ "2. Suburban",
      childhood == 3 ~ "3. Urban",
      TRUE ~ "Not specified"
    ),
    family_size = case_when(
      family_size <= 50 ~ family_size,
      TRUE ~ NA_real_
    ),
    education = case_when(
      education == 1 ~ "1. Less than high school",
      education == 2 ~ "2. High school",
      education == 3 ~ "3. University degree",
      education == 4 ~ "4. Graduate degree",
      TRUE ~ "Not specified"
    )
  )

# write clean data to .csv
write_csv(rwa_recoded, here("clean_data/rwa_data_cleaned.csv"))

