pro_count <- Nettoyage_parents %>%
  select(cat_pro) %>%
  pivot_longer(everything(), names_to = "patient_col", values_to = "value") %>%
  filter(!is.na(value)) %>%
  count(value)

pro_count_percent <- pro_count %>%
  mutate(percent = n / sum(n) * 100)

ggplot(pro_count_percent, aes(x = fct_reorder(value, percent), y = percent)) +
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
    axis.text.x = element_text(size=14, angle = 45, hjust = 1),
    axis.text.y=element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

## Changement légende

library(ggplot2)
library(forcats)
library(stringr)

ggplot(pro_count_percent, aes(x = fct_reorder(value, percent), y = percent)) +
  geom_col(fill = "#F0A6C8", width = 0.4) +   # barres un peu plus fines
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            vjust = -0.5, size = 4) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +  # wrap automatique
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = NULL, y = NULL, title = "Répartition par patient (%)") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 0.5),  # centré sous les barres
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

ggplot(pro_count_percent, aes(x = fct_reorder(value, percent), y = percent)) +
  geom_col(fill = "#F0A6C8", width = 0.6) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            hjust = -0.1, size = 4) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = NULL, y = NULL, title = NULL) +
  coord_flip() +  # inverser axes
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 14),  # labels horizontaux sur l'axe Y
    axis.text.x = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )





library(ggplot2)
library(forcats)
library(stringr)

ggplot(pro_count_percent, aes(x = fct_reorder(value, percent), y = percent)) +
  geom_col(fill = "#F0A6C8", width = 0.7) +      # barres un peu plus larges
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            hjust = -0.1, size = 4) +            # texte à gauche des barres
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = NULL, title = NULL) +
  coord_flip() +                                  # axes inversés pour labels horizontaux
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),  # fond transparent
    plot.background = element_rect(fill = "transparent", color = NA),   # fond transparent
    panel.grid = element_blank(),                                        # pas de grille
    axis.text.x = NULL,
    axis.text.y = element_text(size = 18),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

ggsave("cat_sociopro.png",
       plot = last_plot(),
       width = 13,   # largeur en pouces
       height = 8,   # hauteur en pouces
       dpi = 300,    # résolution
       bg = "transparent")  # fond transparent


### Ajout des données de comparaison

library(tibble)
library(dplyr)

# Création du tableau
csp_data <- tribble(
  ~categorie, ~pourcentage,
  "Agriculteurs", 1.5,
  "Artisans, commerçants et chefs d'entreprise", 6.5,
  "Cadres", 21.6,
  "Professions intermédiaires", 24.7,
  "Employés", 26.2,
  "Employés qualifiés", 14.8,
  "Employés peu qualifiés", 11.4,
  "Ouvriers", 19.1,
  "Ouvriers qualifiés", 12.8,
  "Ouvriers peu qualifiés", 6.3,
  "Non renseigné", 0.5
)

# Affichage
csp_data

library(tibble)
library(dplyr)

# Données de départ
csp_data <- tribble(
  ~categorie, ~pourcentage,
  "Agriculteurs", 1.5,
  "Artisans, commerçants et chefs d'entreprise", 6.5,
  "Cadres", 21.6,
  "Professions intermédiaires", 24.7,
  "Employés", 26.2,
  "Employés qualifiés", 14.8,
  "Employés peu qualifiés", 11.4,
  "Ouvriers", 19.1,
  "Ouvriers qualifiés", 12.8,
  "Ouvriers peu qualifiés", 6.3,
  "Non renseigné", 0.5
)

# Regroupement
csp_grouped <- csp_data %>%
  mutate(
    categorie = case_when(
      grepl("Employés", categorie) ~ "Employés",
      grepl("Ouvriers", categorie) ~ "Ouvriers",
      TRUE ~ categorie
    )
  ) %>%
  group_by(categorie) %>%
  summarise(pourcentage = sum(pourcentage, na.rm = TRUE)) %>%
  ungroup()

# Résultat
csp_grouped


### Merged data

library(dplyr)
library(stringr)

# Harmonisation des catégories
pro_clean <- pro_count_percent %>%
  mutate(
    categorie = case_when(
      str_detect(value, "Agriculteur") ~ "Agriculteurs",
      str_detect(value, "Artisan") ~ "Artisans, commerçants et chefs d'entreprise",
      str_detect(value, "Cadre") ~ "Cadres",
      str_detect(value, "Employé") ~ "Employés",
      str_detect(value, "Ouvrier") ~ "Ouvriers",
      str_detect(value, "Profession intermédiaire") ~ "Professions intermédiaires",
      str_detect(value, "Sans activité") ~ "Non renseigné",
      str_detect(value, "Retraité") ~ "Non renseigné",
      TRUE ~ value
    )
  ) %>%
  rename(pros = percent)

# Fusion avec les CSP (et renommer pourcentage → comp)
csp_clean <- csp_grouped %>%
  rename(comp = pourcentage)

merged_data <- pro_clean %>%
  left_join(csp_clean, by = "categorie")

merged_data


### PLOT

library(dplyr)
library(tidyr)
library(ggplot2)

# Préparation des données au format long
plot_data <- merged_data %>%
  select(value, pros, comp) %>%
  group_by(value) %>%
  summarise(
    pros = sum(pros, na.rm = TRUE),
    comp = sum(comp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(pros, comp),
               names_to = "source",
               values_to = "percent")

# Plot
ggplot(plot_data, aes(x = value, y = percent, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_dodge(width = 0.8),
            vjust = -0.5,
            color = "black",
            size = 4) +
  scale_fill_manual(
    values = c(
      "pros" = "#F0A6C8", 
      "comp" = "#FF4C4C"
    ),
    labels = c("Données pros", "Comparaison")
  ) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 14, angle = 0, hjust = 1, vjust = 1, color = "black"), 
    axis.text.y = element_text(size = 14, color = "black"),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black"),
    legend.position = "bottom"
  ) +
  ylim(0, max(plot_data$percent, na.rm = TRUE) * 1.2)

## horizontal version

library(dplyr)
library(ggplot2)

# Créer un ordre des catégories selon la somme des pourcentages
value_order <- plot_data %>%
  group_by(value) %>%
  summarise(total_percent = sum(percent, na.rm = TRUE)) %>%
  arrange(desc(total_percent)) %>%
  pull(value)

# Transformer en facteur avec l'ordre correct
plot_data <- plot_data %>%
  mutate(value = factor(value, levels = value_order))

# Plot horizontal
ggplot(plot_data, aes(y = value, x = percent, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_dodge(width = 0.8),
            hjust = -0.2,
            color = "black",
            size = 4) +
  scale_fill_manual(
    values = c(
      "pros" = "#F0A6C8", 
      "comp" = "#FF4C4C"
    ),
    labels = c("Données pros", "Comparaison")
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
  coord_cartesian(xlim = c(0, max(plot_data$percent, na.rm = TRUE) * 1.2))

