#### ---------------------------------------------------------------------- ####

# This script assumes that you've already run "01. filter ADHD_condition_df..."

#### ---------------------------------------------------------------------- ####

# Retain rows in ADHD_condition_df matching a person_id in ADHD_df;
  # store in ADHD_dependence_df
    ADHD_dependence_df <- ADHD_condition_df %>%
      select( person_id, dependence_dx = standard_concept_name ) %>%
      filter( person_id %in% ADHD_df$person_id) %>%
      
    # Remove rows with ADHD diagnosis observations
      filter( !(dependence_dx %in% ADHD_dx) )
  
# Aggregate diagnoses by substance and store data in binary variables

  ADHD_dependence_df <- ADHD_dependence_df %>% 
  
  # Create new binary columns for substance-specific diagnoses
  mutate(
    opioid_dx = grepl("opioid", dependence_dx, ignore.case = TRUE) |
      grepl("heroin", dependence_dx, ignore.case = TRUE),
    
    alcohol_dx = grepl("alcohol", dependence_dx, ignore.case = TRUE) &
      !grepl("nondependence", dependence_dx, ignore.case = TRUE),
    
    cocaine_dx = grepl("cocaine", dependence_dx, ignore.case = TRUE),
    
    cannabis_dx = grepl("cannabis", dependence_dx, ignore.case = TRUE),
    
    nicotine_dx = grepl("nicotine", dependence_dx, ignore.case = TRUE) |
      grepl("tobacco", dependence_dx, ignore.case = TRUE),
    
    methadone_dx = grepl("methadone", dependence_dx, ignore.case = TRUE)
  )%>%
  
  # Group by person and collapse to one row per person
  group_by(person_id) %>%
  summarize(
    opioid_dx = any(opioid_dx),
    alcohol_dx = any(alcohol_dx),
    cocaine_dx = any(cocaine_dx),
    cannabis_dx = any(cannabis_dx),
    nicotine_dx = any(nicotine_dx),
    methadone_dx = any(methadone_dx)
  )
  
# Create ADHD_conditions by joining ADHD_df and ADHD_dependence_df
  
  ADHD_conditions <- left_join(ADHD_df, ADHD_dependence_df, by = 'person_id')

# Replace NA values with "FALSE"
  
  ADHD_conditions <- ADHD_conditions %>%
    mutate( opioid_dx = replace_na(opioid_dx, FALSE),
            alcohol_dx = replace_na(alcohol_dx, FALSE),
            cocaine_dx = replace_na(cocaine_dx, FALSE),
            cannabis_dx = replace_na(cannabis_dx, FALSE),
            nicotine_dx = replace_na(nicotine_dx, FALSE),
            methadone_dx = replace_na(methadone_dx, FALSE)
            )

# Clean up workspace
  rm(ADHD_df, ADHD_dependence_df)
