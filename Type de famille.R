library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

famille_count <- PAB %>%
  select(type_famille) %>%
  pivot_longer(everything(), names_to = "patient_col", values_to = "value") %>%
  filter(!is.na(value)) %>%
  count(value)

## Pour la base de données parents

famille_count_percent_pros <- famille_count %>%
  mutate(percent = n / sum(n) * 100)

famille_count_percent_pros <- famille_count %>%
  mutate(percent = n / sum(n) * 100) %>%
  mutate(value_recode = recode(value,
                               "Une famille nucléaireUne famille composée d'un couple de parents et leur(s) enfant(s)" = "Une famille nucléaire",
                               "Autre (veuillez préciser)" = "Autre"))

## Pour la base de données professionnels 

library(dplyr)

famille_count_percent_parents <- Nettoyage_parents %>%
  select(type_famille) %>%
  mutate(
    value_recode = recode(type_famille,
                          "Une famille nucléaireUne famille composée d'un couple de parents et leur(s) enfant(s)" = "Une famille nucléaire",
                          "Autre (veuillez préciser)" = "Autre"
    )
  ) %>%
  count(value_recode) %>%
  mutate(percent = n / sum(n) * 100)


## Ajout des données de comparaison
library(readr)
library(readr)
library(dplyr)

données_comparaison_type_famille_clean <- tibble::tribble(
  ~value_recode,                 ~percent,
  "Une famille monoparentale",    9.5,
  "Une famille nucléaire",       23.4,
  "Une famille recomposée",       3.6   # 
)



library(dplyr)

# Parents
famille_count_percent_parents <- famille_count_percent_parents %>%
  select(value_recode, percent) %>%
  rename(Données_parents = percent)

# Pros
famille_count_percent_pro <- famille_count_percent_pro %>%
  select(value_recode, percent) %>%
  rename(Données_pros = percent)

# Comparaison
données_comparaison_type_famille_clean <- données_comparaison_type_famille_clean %>%
  rename(Données_comparaison = percent)

# Fusionner les 3 sur value_recode
merged_data_famille <- famille_count_percent_parents %>%
  full_join(famille_count_percent_pro, by = "value_recode") %>%
  full_join(données_comparaison_type_famille_clean, by = "value_recode")
merged_data_famille <- merged_data_famille %>%
  filter(!value_recode %in% c("Aucun des éléments ci-dessus", "Autre"))




######################
#### FINAL GRAPH #####
######################


library(dplyr)
library(tidyr)
library(ggplot2)

# Ordre souhaité
ordre_familles <- c("Autre", "Une famille recomposée", "Une famille séparée",
                    "Une famille monoparentale", "Une famille nucléaire")

# Préparer les données pour le plot
plot_data <- merged_data_famille %>%
  select(value_recode, Données_pros, Données_comparaison, Données_parents) %>%
  pivot_longer(cols = starts_with("Données"),
               names_to = "source",
               values_to = "percent") %>%
  mutate(
    value_recode = factor(value_recode, levels = ordre_familles)
  )

# Graphique horizontal
ggplot(plot_data, aes(x = value_recode, y = percent, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(round(percent), "%")),
            position = position_dodge(width = 0.8),
            hjust = -0.1,
            color = "black",   # texte des labels en noir
            size = 5) +
  scale_fill_manual(
    values = c("Données_pros" = "#F0A6C8",
               "Données_comparaison" = "#EB4B3C",
               "Données_parents" = "#F4813D")
  ) +
  labs(x = NULL, y = NULL, fill = NULL, title = NULL) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 12, color = "black"),  # texte axe x en noir
    axis.text.y = element_text(size = 14, color = "black"),  # texte axe y en noir
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "black"),
    legend.text = element_text(color = "black"),             # texte légende en noir
    legend.title = element_text(color = "black"),
    legend.position = "bottom"
  ) +
  ylim(0, max(plot_data$percent, na.rm = TRUE) * 1.2) +
  coord_flip()
