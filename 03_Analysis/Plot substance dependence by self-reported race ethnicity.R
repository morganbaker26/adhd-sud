## Summarize substance dependence diagnoses ADHD vs controls,
 # subsetted by self-reported race/ethnicity, faceted by substance.

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

# Create table for dependence by self-reported category
table_dependence_by_self_reported_category <- xtab_dependence_by_demographic(
  data = ADHD_SUD,
  demographic_var = self_reported_category)

# Plot
plot_dependence_by_self_reported_category <- table_dependence_by_demographic %>%
    group_by(self_reported_category) %>%
    filter(self_reported_category != 'Native Hawaiian/Pacific Islander',
           self_reported_category != 'Skipped') %>%
    ggplot(aes(x = self_reported_category,
               y = prevalence_per_100,
               fill = group))+
    geom_col(position = 'dodge')+
    coord_flip()+
    facet_grid(substance ~.)+
    labs(title = "Prevalence of substance use disorder",
         subtitle = "ADHD vs. non-ADHD individuals by self-reported race/ethnicity",
         x = "Self-reported race/ethnicity",
         y = "Prevalence per 100 individuals")+
    scale_fill_brewer(palette = "Set1")+
    theme(legend.position = "right",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
    

print(plot_dependence_by_self_reported_category)
  
