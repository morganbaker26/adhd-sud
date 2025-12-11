# This script creates a function to crosstabulate substance dependence diagnoses
# by a specified categorical demographic variable.

# Designed to work with ADHD_SUD_tidy_df.csv

xtab_dependence_by_demographic <- function(data, demographic_var) {

table_dependence_by_demographic <- data %>%
  group_by(group, {{demographic_var}}) %>%
  summarize(
    "alcohol" = sum(dependence_alcohol),
    "cannabis" = sum(dependence_cannabis),
    "cocaine" = sum(dependence_cocaine),
    "nicotine" = sum(dependence_nicotine),
    "opioid" = sum(dependence_opioid),
    n_grouptotal = n())%>%
  pivot_longer(cols = `alcohol`:`opioid`,
               names_to = "substance",
               values_to = "count") %>%
  mutate(prevalence_per_100 = (count/n_grouptotal)*100) %>%
  arrange(group, {{demographic_var}}, substance,
          count, n_grouptotal, prevalence_per_100)
}
