# This code creates a plot comparing the rate of a specified substance 
# dependence diagnosis in ADHD cases vs controls without ADHD diagnosis.

library(tidyverse)

# Load data - ADHD_condition_df, NO_ADHD_condition_df, NO_ADHD_person_df

# Shorten data frames
ADHD_condition <- ADHD_condition_df %>% 
  select(person_id, standard_concept_name) %>%  # Select columms
  distinct()                 # Remove duplicate rows

NO_ADHD_condition <- NO_ADHD_condition_df %>%
  select(person_id, standard_concept_name) %>%
  distinct()

# Store total participant counts in n_ADHD and n_controls
n_ADHD <- n_distinct(ADHD_condition$person_id)
n_controls <- n_distinct(NO_ADHD_person_df$person_id)

# Specify condition of interest
dx <- "Alcohol dependence"                     # EDIT HERE!!!!

# Store count of participants in each group with a "dx" diagnosis
n_dx_ADHD <- ADHD_condition %>%
  filter(standard_concept_name == dx) %>%       # Filter for specified diagnosis
  n_distinct(.$person_id)                       # Count distinct person_ids

n_dx_controls <- NO_ADHD_condition %>%
  filter(standard_concept_name == dx) %>%
  n_distinct(.$person_id)

# Create table
dx_table <- data.frame(
  "Group" = c("ADHD","No ADHD diagnosis"),
  "n_dx" = c(n_dx_ADHD, n_dx_controls),
  "n_total" = c(n_ADHD, n_controls)
)
  # Add proportion column
  dx_table <- dx_table %>%
    mutate( Proportion_dx = n_dx/n_total )

#Plot!
dx_table %>% 
  ggplot(aes(x = Group, y = Proportion_dx, fill = Group, position = 'dodge')
  )+ geom_col(
    )+ labs( title = "Alcohol dependence in ADHD vs non-ADHD controls",
             y = "Proportion with documented 
             alcohol dependence")

