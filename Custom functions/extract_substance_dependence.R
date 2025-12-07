#### ------------------------------------------------------------------------ ####

# This script creates the function extract_substance_dependence().

# extract_substance_dependence() takes a dataframe that contains multiple diagnosis
  # observations for a single person_id and condenses substance dependence diagnoses
  # into a single row for each person_id.

# ARGUMENTS
#   extract_substance_dependence( data = data_frame,
#                                 id_column = column_containing_unique_ids,
#                                 dx_column = column_containing_diagnoses )

#### ------------------------------------------------------------------------ ####

extract_substance_dependence <- function(data, id_column, dx_column) {

  data %>%
    select({{id_column}}, {{dx_column}}) %>%
    distinct() %>%
    group_by({{id_column}}) %>%
    mutate(
      alcohol_dependence = str_detect({{dx_column}},
                              regex("alcohol dependence|alcoholism|alcohol abuse",
                              ignore_case = TRUE)
                            ),
      cannabis_dependence = str_detect({{dx_column}},
                              regex("cannabis dependence",
                              ignore_case = TRUE)
                            ),
      cocaine_dependence = str_detect({{dx_column}},
                              regex("cocaine dependence",
                              ignore_case = TRUE)
                            ),
      nicotine_dependence = str_detect({{dx_column}},
                              regex("nicotine dependence|tobacco dependence",
                              ignore_case = TRUE)
                            ),
      opioid_dependence = str_detect({{dx_column}},
                              regex("heroin dependence|opioid dependence",
                              ignore_case = TRUE)
                            )
    ) %>%
    select({{id_column}}, alcohol_dependence:opioid_dependence) %>%
    summarize(
      across(alcohol_dependence:opioid_dependence,
         ~any(.x)), 
         .groups = "drop")
}
