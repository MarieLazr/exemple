install.packages("vroom")
library(vroom)

library(readr)

## Types de patients

## Données pro
type_patients <- PAB %>%
  select(patient1, patient2, patient3, patient4, patient5, patient6, patient7, patient8, patient9, patient10) %>%
  filter(if_any(everything(), ~ !is.na(.))) %>%
  filter(!is.na(value))


type_patients_count <- type_patients %>%
  pivot_longer(everything(), names_to = "patient_col", values_to = "value") %>%
  count(value) %>%
  filter(!is.na(value))


type_patients_count_percent <- type_patients_count %>%
  mutate(percent = n / sum(n) * 100) 

## Données parents
type_patients_parents <- Nettoyage_parents %>%
  filter(!is.na(type_patient)) %>%        # remove missing values
  count(type_patient) %>%                 # counts per type_patient
  mutate(percent = n / sum(n) * 100)     # percentage of total


## Merge des 2 bases de données

library(dplyr)

merged_type_patient <- type_patients_parents %>%
  left_join(type_patients_count_percent, by = c("type_patient" = "value"))

merged_type_patients <- merged_type_patient %>%
  rename(
    parents = percent.x,
    pros = percent.y
  )

### PLOT

library(ggplot2)
library(tidyr)
library(dplyr)

# Préparer les données pour le plot
plot_type_patient <- merged_type_patients %>%
  pivot_longer(
    cols = c(parents, pros),   # ou les noms exacts de tes colonnes après merge
    names_to = "source",
    values_to = "percent"
  )

library(dplyr)
library(ggplot2)

# Remove "Autre (veuillez préciser)"
plot_type_patient_clean <- plot_type_patient %>%
  filter(type_patient != "Autre (veuillez préciser)")

# Plot
ggplot(plot_type_patient_clean, aes(x = type_patient, y = percent, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_dodge(width = 0.8),
            vjust = -0.5,
            color = "black",
            size = 4) +
  scale_fill_manual(
    values = c(
      "pros" = "#F4813D", 
      "parents" = "#F0A6C8"
    ),
    labels = c("Données pros", "Données parents")
  ) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5, color = "black"), # centered
    axis.text.y = element_text(size = 14, color = "black"),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black"),
    legend.position = "bottom"
  ) +
  ylim(0, max(plot_type_patient_clean$percent, na.rm = TRUE) * 1.2)

accompagne_parents <- Nettoyage_parents %>%
  select(type_patient, acc_enfant)

accompagne_pros <- PAB %>%
  select(patient1, patient2, enfant)

library(dplyr)
library(tidyr)

# First, reshape accompagnes_pros so that patient1 and patient2 become a single column
accompagne_pros_long <- accompagne_pros %>%
  pivot_longer(
    cols = c(patient1, patient2),
    names_to = "parent_type",
    values_to = "patient_name"
  )

# Now map parent_type to the type_patient names in Nettoyage_parents
accompagne_pros_long <- accompagne_pros_long %>%
  mutate(type_patient = case_when(
    parent_type == "patient1" ~ "Mère",
    parent_type == "patient2" ~ "Père"
  ))

accompagne_summary <- accompagne_pros_long %>%
  filter(!is.na(enfant)) %>%  # remove missing values
  group_by(type_patient, enfant) %>%
  summarise(n = n(), .groups = "drop") %>%  # count occurrences
  group_by(type_patient) %>%
  mutate(percent = n / sum(n) * 100) %>%   # calculate percentages per parent type
  ungroup()

## pour les parents
library(dplyr)

accompagne_parents_clean <- accompagne_parents %>%
  filter(!is.na(acc_enfant), type_patient != "Autre (veuillez préciser)") %>%  # remove NA and 'Autre'
  mutate(acc_enfant = case_when(
    acc_enfant == "Sans enfant" ~ 0,
    acc_enfant == "Accompagné(e) d'un ou plusieurs enfant(s)" ~ 1,
    TRUE ~ NA_real_
  ))

# Count and percentage
accompagne_parents_summary <- accompagne_parents_clean %>%
  group_by(type_patient, acc_enfant) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(type_patient) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup() %>%
  arrange(type_patient, desc(acc_enfant))

accompagne_parents_summary



# Merge with parents data by type_patient

library(dplyr)

merged_accompagne <- accompagne_parents_summary %>%
  left_join(accompagne_summary,
            by = c("type_patient" = "type_patient", 
                   "acc_enfant" = "enfant"))

# Optional: rename columns for clarity
merged_accompagne <- merged_accompagne %>%
  rename(parents_n = n.x,
         parents_percent = percent.x,
         pros_n = n.y,
         pros_percent = percent.y)

merged_accompagne <- merged_accompagne %>%
  filter(type_patient != "Future mère")


merged_accompagne

## Plot

library(ggplot2)
library(tidyr)
library(dplyr)

library(dplyr)
library(tidyr)
library(ggplot2)

# Pivot the percentages for plotting
plot_accompagne <- merged_accompagne %>%
  pivot_longer(
    cols = c(parents_percent, pros_percent),
    names_to = "source",
    values_to = "percent"
  )

# Plot horizontal bar chart
ggplot(plot_accompagne, aes(y = type_patient, x = percent, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_dodge(width = 0.8),
            hjust = -0.2,
            color = "black",
            size = 4) +
  scale_fill_manual(
    values = c(
      "pros_percent" = "#F0A6C8", 
      "parents_percent" = "#F4813D"
    ),
    labels = c("Données pros", "Données parents")
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
  coord_cartesian(xlim = c(0, max(plot_accompagne$percent, na.rm = TRUE) * 1.2))

### Version verticale

library(dplyr)
library(tidyr)
library(ggplot2)

library(dplyr)
library(tidyr)
library(ggplot2)

# Filter for acc_enfant = 1
plot_accompagne_filtered <- merged_accompagne %>%
  filter(acc_enfant == 1)

# Pivot longer for plotting
plot_accompagne_long <- plot_accompagne_filtered %>%
  pivot_longer(
    cols = c(parents_percent, pros_percent),
    names_to = "source",
    values_to = "percent"
  )

# Plot
ggplot(plot_accompagne_long, aes(x = type_patient, y = percent, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_dodge(width = 0.8),
            vjust = -0.5,  # label above bar
            color = "black",
            size = 4) +
  scale_fill_manual(
    values = c(
      "pros_percent" = "#F0A6C8",
      "parents_percent" = "#F4813D"
    ),
    labels = c("Données pros", "Données parents")
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
  ylim(0, max(plot_accompagne_long$percent, na.rm = TRUE) * 1.2)
