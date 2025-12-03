#### ---------------------------------------------------------------------- ####

# This script assumes that you've already run "02. create ADHD_conditions.R"

#### ---------------------------------------------------------------------- ####

# Load person_df and zip_df (or specify an already-loaded df)
  person_df <- read_csv("ADHD cohort/ADHD_person_df.csv") # EDIT HERE
  zip_df <- read_csv("ADHD cohort/ADHD_zip_df.csv") # EDIT HERE

                     EDIT WITH CAUTION BELOW THIS LINE
#### ---------------------------------------------------------------------- ####

# Filter ADHD_person_df to retain ONLY person_ids present in ADHD_conditions
    person_df <- person_df %>%
      filter( person_id %in% ADHD_conditions$person_id ) %>%
      
  # Create birth_year column and remove date_of_birth
      mutate(birth_year = substr(date_of_birth, 1, 4)) %>%
      select(!date_of_birth)

# Filter ADHD_zip_df to retain ONLY person_ids present in ADHD_conditions
    zip_df <- zip_df %>%
      filter( person_id %in% ADHD_conditions$person_id )
    
# Create ADHD_demographics by joining person_df and zip_df
    ADHD_demographics <- left_join(person_df, zip_df, by = 'person_id') %>%
      mutate( zip_observation_date = substr(observation_datetime, 1, 10) ) %>%
      select( !observation_datetime )
    
# Clean up workspace - remove objects not needed for future analysis
    rm(zip_df, person_df)
