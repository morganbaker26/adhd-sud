#### ---------------------------    README     ---------------------------- ####

# This script creates the function extract_ADHD_subtype().

# extract_ADHD_subtype() condenses ADHD diagnoses into a single subtype 
  # observation for each person_id. 

# ARGUMENTS
  # extract_ADHD_subtype( data = data_frame,
  #                       id_column = column_containing_unique_ids,
  #                       dx_column = column_containing_diagnoses )

# SUBTYPE ASSIGNMENT
  # "Combined" diagnoses are prioritized when "Combined" co-occurs with either
  #  "Hyperactive-impulsive" or "Inattentive".

  # "Combined" diagnosis is assigned when "Hyperactive-impulsive" co-occurs
  #  with "Inattentive" for a single individual.

  # Individuals with no ADHD diagnosis are assigned "None".

#### --------------------------------------------------------------------- ####

extract_ADHD_subtype <- function(data, id_column, dx_column) {

  # Define list of ADHD diagnoses
    dx_list_ADHD <- c("Attention deficit hyperactivity disorder, combined type",
                      "Attention deficit hyperactivity disorder, predominantly inattentive type",
                      "Attention deficit hyperactivity disorder, predominantly hyperactive impulsive type",
                      "Attention deficit hyperactivity disorder",
                      "Adult attention deficit hyperactivity disorder",
                      "Child attention deficit disorder",
                      "Undifferentiated attention deficit disorder")
  
  # Define buckets for ADHD subtypes  
      ADHD_H <- "Attention deficit hyperactivity disorder, predominantly hyperactive impulsive type"
      ADHD_U <- c("Attention deficit hyperactivity disorder",
                  "Adult attention deficit hyperactivity disorder",
                  "Child attention deficit disorder",
                  "Undifferentiated attention deficit disorder"
      )
      ADHD_PI <- "Attention deficit hyperactivity disorder, predominantly inattentive type"
      ADHD_C <- "Attention deficit hyperactivity disorder, combined type"
  
  data %>%
    distinct() %>%
    select({{id_column}},{{dx_column}}) %>%
    group_by({{id_column}}) %>%

    mutate( ADHD = 
      case_when(
        # ADHD = "None" if no ADHD diagnosis is present
        !any({{dx_column}} %in% dx_list_ADHD) ~ "None", 
        # ADHD = "Combined" if combined diagnosis is present OR
        any({{dx_column}} %in% ADHD_C) |
          # If hyperactive and inattentive diagnoses co-occur
          (any({{dx_column}} %in% ADHD_PI) & any({{dx_column}} %in% ADHD_H)) ~ "Combined",
        # ADHD = "Inattentive" if inattentive is present
        any({{dx_column}} %in% ADHD_PI) ~ "Inattentive",
        # ADHD = "Hyperactive" if hyperactive is present
        any({{dx_column}} %in% ADHD_H) ~ "Hyperactive",
        # ADHD = "Unspecified" if only unspecified diagnoses are present
        any({{dx_column}} %in% ADHD_U) ~ "Unspecified",
        TRUE ~ "None"
      )) %>%
   summarize(ADHD = first(ADHD), .groups = "drop")
}
