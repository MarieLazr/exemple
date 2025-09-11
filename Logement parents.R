library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

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

### Ajout des sources de comparaiso

library(dplyr)
library(ggplot2)
library(forcats)

# Suppose your original dataset looks like:
# logement_count_percent$value_recode : "Maison", "Appartement"
# logement_count_percent$percent : percentages from your survey

# Create the second source dataset (2/3 Maison, 1/3 Appartement)
logement_second_source <- tibble(
  value_recode = c("Maison", "Appartement"),
  percent = c(66.7, 33.3),   # 2/3 and 1/3 in percentage
  source = "Comparaison"
)

# Add source to the first dataset
logement_first_source <- logement_count_percent %>%
  mutate(source = "Pros")

# Combine the two datasets
logement_plot_data <- bind_rows(logement_first_source, logement_second_source)

# Plot with dodged bars
ggplot(logement_plot_data, aes(x = fct_reorder(value_recode, percent), y = percent, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_dodge(width = 0.8),
            vjust = -0.5,
            color = "black",
            size = 5) +
  scale_fill_manual(
    values = c(
      "Pros" = "#F0A6C8", 
      "Comparaison" = "#FF4C4C"
    ),
    labels = c("Données pros", "Comparaison")
  ) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text.x = element_text(size = 14, hjust = 0.5, color = "black"),  # ✅ center under bars
    axis.text.y = element_blank(),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black"),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  ylim(0, max(logement_plot_data$percent, na.rm = TRUE) * 1.2)
