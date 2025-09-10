logement_count <- Nettoyage_parents %>%
  select(cat_logement) %>%
  pivot_longer(everything(), names_to = "patient_col", values_to = "value") %>%
  filter(!is.na(value)) %>%
  count(value)

library(forcats)
logement_count_percent <- logement_count %>%
  mutate(percent = n / sum(n) * 100) %>%
  mutate(value_recode = recode(value,
                          "Vous habitez dans un appartement dont vous êtes propriétaire ou locataire" = "Appartement",
                          "Vous habitez dans une maison dont vous êtes propriétaire ou locataire" = "Maison"))

ggplot(logement_count_percent, aes(x = fct_reorder(value_recode, percent), y = percent)) +
  geom_col(fill="#F0A6C8") +
  labs(x = "Patient", y = "Count") +
  theme_minimal()+
  labs(
    title = NULL,
    x = NULL,
    y = NULL
  )+
  geom_text(aes(label = paste0(round(percent, 1), "%")), vjust = -0.5) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text.x = element_text(angle = 0, hjust = 1),
    axis.text.y=element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )
