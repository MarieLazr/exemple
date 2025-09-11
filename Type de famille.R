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

famille_count_percent <- famille_count %>%
  mutate(percent = n / sum(n) * 100)

famille_count_percent <- famille_count %>%
  mutate(percent = n / sum(n) * 100) %>%
  mutate(value_recode = recode(value,
                               "Une famille nucléaireUne famille composée d'un couple de parents et leur(s) enfant(s)" = "Une famille nucléaire",
                               "Autre (veuillez préciser)" = "Autre"))

## Pour la base de données professionnels 
famille_count_percent <- famille_count %>%
  mutate(value_recode=recode(value,
                             "Aucun des éléments ci-dessus"="Autre")) %>%
  mutate(percent = n / sum(n) * 100)


## Graphique mis en forme
ggplot(famille_count_percent, aes(x = fct_reorder(value_recode, percent), y = percent)) +
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
    axis.text.x = element_text(size=16, angle = 45, hjust = 1),
    axis.text.y=element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )
ggsave("type de famille.png",
       plot = last_plot(),
       width = 12,   # largeur en pouces
       height = 8,   # hauteur en pouces
       dpi = 300,    # résolution
       bg = "transparent")  # fond transparent
library(ggplot2)
library(forcats)
library(stringr)

ggplot(famille_count_percent, aes(x = fct_reorder(value_recode, percent), y = percent)) +
  geom_col(fill="#F0A6C8", width = 0.6) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            hjust = -0.1, family="Avenir", size=4) +  # texte à gauche des barres
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.1))) +
  labs(x = NULL, y = NULL, title = NULL) +
  coord_flip() +  # bascule à l'horizontal
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),  # labels horizontaux sur l'axe y
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

## Ajout des données de comparaison
library(readr)
library(readr)
données_comparaison_type_famille <- read_delim("données comparaison type famille.csv", 
                                               delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(données_comparaison_type_famille)

données_comparaison_type_famille_clean <- données_comparaison_type_famille %>%
  filter(if_any(everything(), ~!is.na(.))) %>%
  # Remove columns that are completely NA
  select(where(~!all(is.na(.)))) %>%
  mutate(percent = value / sum(value) * 100)

## Merge des 2 bases de données
merged_data_famille <- famille_count_percent %>%
  left_join(données_comparaison_type_famille_clean,
            by = c("value_recode" = "type"))

## Ajout données parents
famille_parents <- Nettoyage_parents %>%
  select (type_famille) %>%
  mutate(value_recode = recode(type_famille,
                               "Une famille nucléaireUne famille composée d'un couple de parents et leur(s) enfant(s)" = "Une famille nucléaire",
                               "Autre (veuillez préciser)" = "Autre"))

merged_data_famille <- merged_data_famille %>%
  left_join(famille_parents_summary,
            by = c("value_recode" = "type_famille")) 

library(dplyr)

merged_data_famille <- merged_data_famille %>%
  rename(
    Données_pros=percent.x
  )


library(dplyr)

famille_parents_summary <- famille_parents %>%
  count(type_famille) %>%                      
  mutate(percent = n / sum(n) * 100)  



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
