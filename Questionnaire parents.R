install.packages("vroom")
library(vroom)

library(readr)

## Age des parents
age_parents <- Nettoyage_parents %>%
  select(age) %>%
  filter (!is.na(age))

age_count <- age_parents %>%
  pivot_longer(everything(), names_to = "patient_col", values_to = "value") %>%
  filter(!is.na(value)) %>%
  count(value)

  age_count_percent <- age_count %>%
  mutate(percent = n / sum(n) * 100)


ggplot(age_count_percent, aes(x = fct_reorder(value, percent), y = percent)) +
  geom_col(fill = "#F0A6C8") +
  labs(x = "Age", y = "Percentage") +
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
    axis.text.x = element_text(size=12, angle = 0, hjust = 1),
    axis.text.y=element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

library(forcats)

ggplot(age_count_percent, aes(x = fct_reorder(value, percent), y = percent)) +
  geom_col(fill = "#F0A6C8", width = 0.7) +  # ajuste la largeur des colonnes
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            vjust = -0.5, size = 4) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5),  # centre horizontalement
    axis.text.y = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

