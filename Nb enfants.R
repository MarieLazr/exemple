library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

nb_enfants_count <- PAB %>%
  select(nb_enfants) %>%
  pivot_longer(everything(), names_to = "patient_col", values_to = "value") %>%
  filter(!is.na(value)) %>%
  filter(value != "Je ne sais pas") %>%
  count(value)

nb_enfants_count_percent <- nb_enfants_count %>%
  mutate(percent = n / sum(n) * 100)

## Graphique mis en forme
ggplot(nb_enfants_count_percent, aes(x = value, y = percent)) +
  geom_col(fill="#F0A6C8") +
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
    axis.text.x = element_text(size=16, angle = 0, hjust = 1),
    axis.text.y=element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

## Ajout des données parents
nb_enfants_parents <- Nettoyage_parents %>% 
  select(nb_enfants_mineurs, nb_enfants_majeurs)

library(dplyr)

nb_enfants_parents <- nb_enfants_parents %>%
  mutate(nb_enfants_total = nb_enfants_mineurs + nb_enfants_majeurs)

library(dplyr)

# Comptage des occurrences et calcul du pourcentage
nb_enfants_stats <- nb_enfants_parents %>%
  count(nb_enfants_total) %>%               # compte les occurrences de chaque total
  mutate(pourcentage = n / sum(n) * 100)   # calcule le pourcentage

nb_enfants_stats

library(dplyr)

merged_data <- nb_enfants_count_percent %>%
  left_join(nb_enfants_stats, by = "nb_enfants_total")

merged_data

library(dplyr)
library(tidyr)

données_comparaison_nb_enfants_transposed <- données_comparaison_nb_enfants_clean %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(cols = -row_id, names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = row_id, values_from = value)


## Merge des 2 bases de données
merged_data <- nb_enfants_count_percent %>%
  mutate(value = as.numeric(value)) %>%
  left_join(nb_enfants_stats, by = c("value" = "nb_enfants_total"))

merged_data <- merged_data %>%
  rename(
    données_pros = percent,
    donnees_parents = pourcentage
  )


## Ajout des données de comparaison

library(dplyr)
library(readr)

# 1. Read the CSV
données_comparaison_type_famille <- read_delim(
  "données comparaison type famille.csv", 
  delim = ";", 
  escape_double = FALSE, 
  trim_ws = TRUE
)


library(dplyr)
library(readr)
library(janitor)
données_comparaison_type_famille_clean <- données_comparaison_type_famille %>%
  remove_empty(c("rows", "cols"))

# 4. Merge with previous merged_data by 'value' (or 'nb_enfants_total' if renamed)
final_merged <- merged_data %>%
  left_join(données_comparaison_type_famille_clean, 
            by = c("value" = "nb_enfants")) 
final_merged <- final_merged %>%
  rename(donnees_comparaison = percentage)

# 5. Inspect result
head(final_merged)

library(dplyr)

final_merged <- final_merged %>%
  mutate(donnees_comparaison = donnees_comparaison / 10)




#################
## GRAPH FINAL ##
#################


library(dplyr)
library(tidyr)
library(ggplot2)

# Ordre souhaité
#ordre_familles <- c

# Préparer les données pour le plot
plot_data <- merged_data %>%
  select(value, données_pros, donnees_parents) %>%
  pivot_longer(cols = c(données_pros, donnees_parents),
               names_to = "source",
               values_to = "percent")

  #mutate(
    #value_recode = factor(value_recode, levels = ordre_familles)
  #)

# Graphique horizontal
ggplot(plot_data, aes(x = as.numeric(value), y = percent, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(round(percent), "%")),
            position = position_dodge(width = 0.8),
            vjust = -0.5,
            color = "black",
            size = 5) +
  scale_fill_manual(
    values = c("données_pros" = "#F0A6C8",
               "donnees_parents" = "#F4813D")
  ) +
  scale_x_continuous(
    breaks = seq(0, max(as.numeric(plot_data$value), na.rm = TRUE), by = 1)
  ) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = NULL,
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black"),
    legend.position = "bottom"
  ) +
  ylim(0, max(plot_data$percent, na.rm = TRUE) * 1.2)


library(dplyr)
library(tidyr)
library(ggplot2)

# Préparer les données pour le plot avec les 3 sources
plot_data_final <- final_merged %>%
  select(value, données_pros, donnees_parents, donnees_comparaison) %>%
  pivot_longer(
    cols = c(données_pros, donnees_parents, donnees_comparaison),
    names_to = "source",
    values_to = "percent"
  )

# Graphique vertical avec toutes les sources
ggplot(plot_data_final, aes(x = as.numeric(value), y = percent, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(round(percent), "%")),
            position = position_dodge(width = 0.8),
            vjust = -0.5,
            color = "black",
            size = 5) +
  scale_fill_manual(
    values = c(
      "données_pros" = "#F0A6C8",
      "donnees_parents" = "#F4813D",
      "donnees_comparaison" = "#E03C3C"  # rouge pour comparaison
    )
  ) +
  scale_x_continuous(
    breaks = seq(0, max(as.numeric(plot_data_final$value), na.rm = TRUE), by = 1)
  ) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = NULL,
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black"),
    legend.position = "bottom"
  ) +
  ylim(0, max(plot_data_final$percent, na.rm = TRUE) * 1.2)

## Catégorie 4 et +

library(dplyr)

# Update first two datasets (données_pros and donnees_parents)
merged_data <- merged_data %>%
  mutate(
    value = as.numeric(value),
    value = ifelse(value >= 4, "4 et plus", as.character(value))
  ) %>%
  group_by(value) %>%
  summarise(
    données_pros = sum(as.numeric(données_pros), na.rm = TRUE),
    donnees_parents = sum(as.numeric(donnees_parents), na.rm = TRUE)
  ) %>%
  ungroup()

## New plot

library(dplyr)
library(tidyr)
library(ggplot2)

# Make sure all values are properly grouped
final_plot_data <- final_merged %>%
  mutate(
    value = as.character(value),
    value = ifelse(as.numeric(value) >= 4, "4 et plus", value)
  ) %>%
  group_by(value) %>%
  summarise(
    données_pros = sum(as.numeric(données_pros), na.rm = TRUE),
    donnees_parents = sum(as.numeric(donnees_parents), na.rm = TRUE),
    donnees_comparaison = sum(as.numeric(donnees_comparaison), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c(données_pros, donnees_parents, donnees_comparaison),
               names_to = "source",
               values_to = "percent")

# Plot
ggplot(final_plot_data, aes(x = value, y = percent, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(round(percent), "%")),
            position = position_dodge(width = 0.8),
            vjust = -0.5,
            color = "black",
            size = 5) +
  scale_fill_manual(
    values = c(
      "données_pros" = "#FF4C4C", 
      "donnees_parents" = "#F4813D", 
      "donnees_comparaison" = "#F0A6C8"  # same red as before
    ),
    labels = c("Données pros", "Données parents", "Comparaison")
  ) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black"),
    legend.position = "bottom"
  ) +
  ylim(0, max(final_plot_data$percent, na.rm = TRUE) * 1.2)

