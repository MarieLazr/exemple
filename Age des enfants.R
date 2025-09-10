library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

age_enfants_concernes_count <- PAB %>%
  select(age_enfant1_concerné, age_enfant2_concerné, age_enfant3_concerné, age_enfant4_concerné, age_enfant5_concerné) %>%
  pivot_longer(everything(), names_to = "patient_col", values_to = "value") %>%
  filter(!is.na(value)) %>%
  count(value)

age_enfants_concernes_percent <- age_enfants_concernes_count %>%
  mutate(percent = n / sum(n) * 100)

age_enfants_accueillis_count <- PAB %>%
  select(age_enfant1_accueilli, age_enfant2_accueilli, age_enfant3_accueilli, age_enfant4_accueilli, age_enfant5_accueilli) %>%
  pivot_longer(everything(), names_to = "patient_col", values_to = "value") %>%
  filter(!is.na(value)) %>%
  count(value)

age_enfants_accueillis_percent <- age_enfants_accueillis_count %>%
  mutate(percent = n / sum(n) * 100)

merged_df_count <- left_join(age_enfants_concernes_count, age_enfants_accueillis_count, by = "value")

merged_df_percentage <- left_join(age_enfants_concernes_percent, age_enfants_accueillis_percent, by = "value")

## Graphique mis en forme
ggplot(merged_df, aes(x = value)) +
  labs(x = "Patient", y = "Count") +
  theme_minimal()+
  labs(
    title = NULL,
    x = NULL,
    y = NULL
  )+
  geom_text(aes(label = paste0(round(percent, 1), "%")), vjust = -0.5, family="Avenir", size=4) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text.x = element_text(size=16, angle = 45, hjust = 1),
    axis.text.y=element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )