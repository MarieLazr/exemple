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
ggplot(age_enfants_accueillis_count, aes(x = value)) +
  geom_bar()
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
  
## Proposition de correction
  
  # Graphique avec pourcentages
  ggplot(age_enfants_accueillis_percent, aes(x = value, y = n)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = paste0(round(percent, 1), "%")), 
              vjust = -0.5, family="Avenir", size=4) +
    theme_minimal() +
    labs(
      title = NULL,
      x = "Âge de l'enfant accueilli",
      y = "Nombre"
    ) +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "transparent", color = NA),
      axis.text.x = element_text(size=14, angle = 0, hjust = 1),
      axis.text.y = element_blank(),
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5)
    )
  
## Combiner les types d'enfants (venus et pas venus)
  
  # Préparer les données comparatives
  age_concernes <- age_enfants_concernes_percent %>%
    mutate(type = "Concernés")
  
  age_accueillis <- age_enfants_accueillis_percent %>%
    mutate(type = "Accueillis")
  
  df_comparatif <- bind_rows(age_concernes, age_accueillis)
  
  # Graphique comparatif
  ggplot(df_comparatif, aes(x = value, y = n, fill = type)) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_text(
      aes(label = NULL),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      family = "Avenir",
      size = 4
    ) +
    theme_minimal() +
    labs(
      title = "Répartition des âges des enfants",
      x = NULL,
      y = NULL,
      fill = "Type"
    ) +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "transparent", color = NA),
      axis.text.x = element_text(size = 10, angle = 0, hjust = 1),
      axis.text.y = element_text(size = 1),
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      legend.position = "top"
    )
  
## Correction résolue
  
  ggplot(df_comparatif, aes(x = value, y = n, fill = type)) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_text(
      aes(label = n),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      family = "Avenir",
      size = 3
    ) +
    theme_minimal() +
    labs(
      title = "Répartition des âges des enfants",
      x = NULL,
      y = NULL,
      fill = "Type"
    ) +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "transparent", color = NA),
      axis.text.x = element_text(size = 10, angle = 0, hjust = 1),
      axis.text.y = element_text(size = 8),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      legend.position = "top"
    )
  
  
## En pourcentages
  
  ggplot(df_comparatif, aes(x = value, y = percent, fill = type)) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_text(
      aes(label = paste0(round(percent, 1), "%")),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      family = "Avenir",
      size = 2.5
    ) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    theme_minimal() +
    labs(
      title = "Répartition des âges des enfants (%)",
      x = "Âge de l'enfant",
      y = "Pourcentage",
      fill = "Type"
    ) +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "transparent", color = NA),
      axis.text.x = element_text(size = 10, angle = 0, hjust = 1),
      axis.text.y = element_text(size = 9),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      legend.position = "top"
    )
  
## aJOUT DES COULEURS

  ggplot(df_comparatif, aes(x = value, y = percent, fill = type)) +
    geom_col(position = position_dodge(width = 0.8)) +
    scale_y_continuous(labels = function(x) paste0(x)) +
    scale_fill_manual(values = c("Concernés" = "#F0A6C8", "Accueillis" = "#EB4B3C")) +
    theme_minimal() +
    labs(
      title = "Répartition des âges des enfants (%)",
      x = "Âge de l'enfant",
      y = NULL,
      fill = "Type"
    ) +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "transparent", color = NA),
      axis.text.x = element_text(size = 10, angle = 0, hjust = 1),
      axis.text.y = element_text(size = 9),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      legend.position = "top"
    )
  
## Age des enfants
  
  ggplot(df_comparatif, aes(x = value, y = percent, fill = type)) +
    geom_col(position = position_dodge(width = 0.8)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_x_continuous(breaks = sort(unique(df_comparatif$value))) +
    scale_fill_manual(values = c("Concernés" = "#F0A6C8", "Accueillis" = "#EB4B3C")) +
    theme_minimal() +
    labs(
      title = "Répartition des âges des enfants (%)",
      x = "Âge de l'enfant",
      y = NULL,
      fill = "Type"
    ) +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "transparent", color = NA),
      axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5),
      axis.text.y = element_text(size = 9),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      legend.position = "top"
    )
  
### Correction
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(forcats)
  
  ggplot(age_enfants_accueillis_percent, aes(x = as.numeric(value), y = percent)) +
    geom_col(fill = "#F0A6C8", width = 0.7) +
    geom_text(aes(label = paste0(round(percent, 1), "%")), 
              vjust = -0.5, family = "Avenir", size = 3) +
    scale_x_continuous(breaks = sort(unique(as.numeric(age_enfants_accueillis_percent$value)))) +  # ✅ tous les âges
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "transparent", color = NA),
      axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5),
      axis.text.y = element_blank(),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  
  