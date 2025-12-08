# Version 1.0

# This script assumes that you've already run '00. Setup.R'.

# This script assumes that the required All of Us data is already loaded. 

# Required data: person, condition and zip tables for ADHD and NO_ADHD cohorts

#### ------------------------------------   EDIT BELOW   ---------------------------------------- ####

person_ADHD <- df                    ## Replace 'df' with name of ADHD person df
condition_ADHD <- df                 ## Replace 'df' with name of ADHD condition df
zip_ADHD <- df                       ## Replace 'df' with name of ADHD zip df

person_control <- df                 ## etc
condition_control <- df              ## etc
zip_control <- df                    ## etc

#### ----------------------------EDIT WITH CAUTION BELOW THIS LINE------------------------------- ####

# Clean person_dfs
person_ADHD <- clean_person_df(person_ADHD)
person_control <- clean_person_df(person_control)

# Create list of individuals in ADHD cohort with any ADHD diagnosis recorded 2013 or later
  
  # Create dx_list for retaining ADHD diagnoses
    dx_list_ADHD <- c("Attention deficit hyperactivity disorder, combined type",
                      "Attention deficit hyperactivity disorder, predominantly inattentive type",
                      "Attention deficit hyperactivity disorder, predominantly hyperactive impulsive type",
                      "Attention deficit hyperactivity disorder",
                      "Adult attention deficit hyperactivity disorder",
                      "Child attention deficit disorder",
                      "Undifferentiated attention deficit disorder")

  # Filter for ADHD dx post-2013 and store person_ids in person_list_ADHD
    person_list_ADHD <- condition_ADHD %>%
      select(person_id, standard_concept_name, condition_start_date) %>%
      distinct() %>%
      filter(standard_concept_name %in% dx_list_ADHD) %>%
      mutate(condition_start_date = year(condition_start_date)) %>%
      filter(condition_start_date >= 2013) %>%
      select(person_id)

# Join person and zip tables for each cohort
  # ADHD
  demographic_ADHD <- person_ADHD %>%
    filter(person_id %in% person_list_ADHD) %>% # Filter and pipe into join
    left_join(zip_ADHD, by = 'person_id') %>% # Join with zip_ADHD, drop rows not matched in filtered person_ADHD
    mutate(exposure = 1) # Create exposure column for matching (exposure is ADHD dx)
  # Controls
  demographic_control <- person_control %>%
    left_join(zip_control, by = 'person_id') %>%
    mutate(exposure = 0) # Exposure column for matching (0 because no ADHD dx)

# Bind ADHD and NO_ADHD tables together for case-control matching
demographic_match <- bind_rows(demographic_ADHD, demographic_control)

# Perform case-control matching (1 case:4 controls) by sex_at_birth and self_reported_category
demographic <- matchit(
  exposure ~ sex_at_birth + birth_year + self_reported_category,
  data = demographic_match,
  method = "nearest",
  exact = ~sex_at_birth,
  ratio = 4,                       # 4 controls per case
  replace = FALSE
)

    # Plot case-control match diagnostics
    plot(demographic, type = "density")

    # Extract matched data
    demographic <- match.data(demographic)

# Filter condition_control and bind with condition_ADHD
  condition <- condition_control %>%
    filter(person_id %in% demographic$person_id) %>%
    bind_rows(condition_ADHD)
  
# Extract diagnoses from condition
  # Extract ADHD subtype
  subtype <- extract_ADHD_subtype(condition, person_id, standard_concept_name)
  # Extract dependence diagnoses
  dependence <- extract_substance_dependence(condition, person_id, standard_concept_name)

# Join diagnosis extract tables 
condition <- full_join(subtype, dependence, by = 'person_id')

# Join condition and demographic tables for COMPLETE TIDY DATA <3
ADHD_SUD_df <- left_join(demographic, condition, by = 'person_id')
