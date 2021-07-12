library(tidyverse)
library(janitor)
library(here)

rwa_data <- read_csv(here("raw_data/rwa.csv"))


calculate_rwa_score <- function(x) {
  rwa_score_qs <- c(2, 4, 5, 9, 11, 13, 15, 16, 18, 21)
  rwa_score_qs_inv <- c(3, 5, 7, 8, 10, 12, 14, 17, 19, 20)
  
  for (i in x) {
    rwa_count = 0
    for (y in rwa_score_qs) {
      rwa_count <- rwa_count + i[y]
    }
    for (y in rwa_score_qs_inv) {
      rwa_count <- rwa_count + (10 - i[y])
    }
    return(mean(rwa_count))
  }
}

rwa_data_trimmed <- rwa_data %>%
  mutate(id = rownames(rwa_data)) %>%
  select(id,
         "Q3":"Q22",
         testelapse,
         gender,
         age,
         hand,
         urban,
         familysize,
         education)

test <- rwa_data_trimmed %>%
   calculate_rwa_score()

rwa_data_trimmed %>% 
  sum(cols_only(rwa_score_qs = col_double()))

sum(rwa_data_trimmed[1,rwa_score_qs])

calculate_rwa_score(rwa_data_trimmed)
