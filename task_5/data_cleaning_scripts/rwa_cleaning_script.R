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
  # Questions in "inv" variable are reverse scored, take the total away from 90 (max possible)
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
         family_size = familysize)
  


test %>%
  
