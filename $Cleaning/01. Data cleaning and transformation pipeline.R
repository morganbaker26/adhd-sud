# Version 1.1

# This script assumes that you've already run '00. Setup.R'.

# This script assumes that the required All of Us data is already loaded. 

# Required data: person, condition and zip tables for ADHD and NO_ADHD cohorts

# -------------------------------- REQUIRED INPUTS -----------------------------
# Edit the variable names below to match your loaded data

person_ADHD       <- ADHD_person_df          # ADHD person dataframe
condition_ADHD    <- ADHD_condition_df       # ADHD condition dataframe
zip_ADHD          <- ADHD_zip_df             # ADHD zip dataframe

person_control    <- NO_ADHD_person_df       # NO ADHD person dataframe
condition_control <- NO_ADHD_condition_df    # NO ADHD condition dataframe
zip_control       <- NO_ADHD_zip_df          # NO ADHD zip dataframe

# ----------------------- EDIT WITH CAUTION BELOW THIS LINE --------------------

# -------------------------------- DATA CLEANING -------------------------------

# Clean person dataframes using helper function
person_ADHD    <- clean_person_df(person_ADHD)
person_control <- clean_person_df(person_control)


# ------------------------- ADHD COHORT IDENTIFICATION -------------------------

# Define ADHD diagnosis codes to retain
dx_list_ADHD <- c(
  "Attention deficit hyperactivity disorder, combined type",
  "Attention deficit hyperactivity disorder, predominantly inattentive type",
  "Attention deficit hyperactivity disorder, predominantly hyperactive impulsive type",
  "Attention deficit hyperactivity disorder",
  "Adult attention deficit hyperactivity disorder",
  "Child attention deficit disorder",
  "Undifferentiated attention deficit disorder"
)

# Identify individuals with ADHD diagnosis in 2013 or later
person_list_ADHD <- condition_ADHD %>%
  select(person_id, standard_concept_name, condition_start_datetime) %>%
  distinct() %>%
  filter(standard_concept_name %in% dx_list_ADHD) %>%
  mutate(condition_start_date = year(condition_start_datetime)) %>%
  filter(condition_start_date >= 2013) %>%
  select(person_id) %>%
  distinct()


# ------------------------- DEMOGRAPHIC DATA PREPARATION -----------------------

# Create ADHD demographic dataset
demographic_ADHD <- person_ADHD %>%
  filter(person_id %in% person_list_ADHD$person_id) %>%
  left_join(zip_ADHD, by = "person_id") %>%
  mutate(exposure = 1L)  # Treatment group indicator

# Create control demographic dataset
demographic_control <- person_control %>%
  left_join(zip_control, by = "person_id") %>%
  mutate(exposure = 0L)  # Control group indicator

# Combine ADHD and control datasets for matching
demographic_match <- bind_rows(demographic_ADHD, demographic_control)


# --------------------------- PROPENSITY SCORE MATCHING ------------------------

# Perform 1:4 case-control matching
demographic <- matchit(
  exposure ~ sex_at_birth + birth_year + self_reported_category,
  data    = demographic_match,
  method  = "nearest",
  exact   = ~sex_at_birth,
  ratio   = 4,
  replace = FALSE
)

# Visualize matching quality
plot(demographic, type = "density")

# Extract matched dataset
demographic <- match.data(demographic)


# ----------------------------- CONDITION DATA PREP ----------------------------

# Filter control conditions to matched individuals only
condition <- condition_control %>%
  filter(person_id %in% demographic$person_id) %>%
  bind_rows(condition_ADHD)


# --------------------------- DIAGNOSIS EXTRACTION -----------------------------

# Extract ADHD subtype information
subtype <- extract_ADHD_subtype(
  condition, 
  person_id, 
  standard_concept_name
)

# Extract substance dependence diagnoses
dependence <- extract_substance_dependence(
  condition, 
  person_id, 
  standard_concept_name
)

# Combine diagnosis extracts
condition <- full_join(subtype, dependence, by = "person_id")


# --------------------------- FINAL DATA ASSEMBLY ------------------------------

# Create complete dataset
ADHD_SUD_df <- left_join(demographic, condition, by = "person_id")

# Replace NAs in ADHD_subtype column with "None"
ADHD_SUD_tidy_df <- ADHD_SUD_tidy_df %>%
  mutate(ADHD_subtype = replace_na(ADHD_subtype, "None"))
