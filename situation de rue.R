rue_count <- PAB %>%
  select(rue) %>%
  pivot_longer(everything(), names_to = "patient_col", values_to = "value") %>%
  filter(!is.na(value)) %>%
  count(value)

library(forcats)
rue_count_percent <- rue_count %>%
  mutate(percent = n / sum(n) * 100) %>%
  mutate(value = recode(value,
                        "Oui"="En situation de rue",
                        "Non"="Logé.e.s"))
                      
ggplot(rue_count_percent, aes(x = fct_reorder(value, percent), y = percent)) +
  geom_col(fill="#F0A6C8") +
  labs(x = "Rue", y = "Count") +
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
    axis.text.x = element_text(size=14, angle = 0, hjust = 1),
    axis.text.y=element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

##
library(ggplot2)
library(forcats)

# Filtrer la catégorie à exclure
rue_count_percent_filtered <- rue_count_percent %>%
  dplyr::filter(value != "Je ne sais pas")

ggplot(rue_count_percent_filtered, aes(x = fct_reorder(value, percent), y = percent)) +
  geom_col(fill = "#F0A6C8", width = 0.7) +   # barre un peu plus large
  geom_text(aes(label = paste0(round(percent, 1), "%")), vjust = -0.5, size = 4) +
  labs(x = NULL, y = NULL, title = NULL) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.05))) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),  # centré sous chaque barre
    axis.text.y = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

### Ajout des données de comparaison
library(dplyr)
library(ggplot2)

# Données comparaison
rue_comparaison <- tibble(
  value = c("En situation de rue", "Logé.e.s"),
  percent = c(1, 99),
  source = "Comparaison"
)

# Ajouter source = Pros dans les données existantes
rue_pros <- rue_count_percent_filtered %>%
  mutate(source = "Pros")

# Fusionner les deux
rue_plot_data <- bind_rows(rue_pros, rue_comparaison)

# Graphique
ggplot(rue_plot_data, aes(x = fct_reorder(value, percent), y = percent, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_dodge(width = 0.8),
            vjust = -0.5,
            size = 4,
            color = "black") +
  labs(x = NULL, y = NULL, title = NULL) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(
    values = c("Pros" = "#F0A6C8", "Comparaison" = "#FF4C4C"),
    labels = c("Données pros", "Données de comparaison")
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 14, hjust = 0.5, color = "black"),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(color = "black"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

