library(dplyr)
library(ggplot2)
library(scales)

# Suppose your raw dataset has one row per respondent
# and columns patient1..6 = 1 (yes) / 0 (no)
# Example:
# respondents_df <- read.csv("your_file.csv")

# Create mutually exclusive categories
patients_long <- PAB %>%
  mutate(
    category = case_when(
      patient1 != "" & patient2 == "" ~ "Mère seule",
      patient2 != "" & patient1 == "" ~ "Père seul",
      patient1 != "" & patient2 != "" ~ "Mère & Père",
      patient5 != "" & patient6 == "" ~ "Belle mère seule",
      patient6 != "" & patient5 == "" ~ "Beau père seul",
      patient5 != "" & patient6 != "" ~ "Belle mère & Beau père",
      patient7 != "" & patient8 == "" ~ "Grand-mère seule",
      patient8 != "" & patient7 == "" ~ "Grand-père seul",
      patient8 != "" & patient7 != "" ~ "Grands-parents",
      patient10 != "" ~ "Professionnel",
      TRUE ~ NA_character_
    )
  )

# Calculate percentages
patients_percentage_df <- patients_long %>%
  count(category) %>%
  mutate(percentage = 100 * n / sum(n))

# Plot
ggplot(patients_percentage_df, aes(x = reorder(category, -percentage), y = percentage)) +
  geom_col(fill = "#4E79A7") +
  labs(
    title = "Pourcentage de répondants par type de patient",
    x = "Type de patient",
    y = "Pourcentage"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  scale_y_continuous(labels = percent_format(scale = 1))

library(dplyr)
library(ggplot2)
library(scales)

# Create the extra bar for "Père et Mère"
mere_pere_bar <- PAB %>%
  filter(somme_patients == 2) %>%              # keep rows where sum == 2
  summarise(n = n()) %>%                       # count respondents
  mutate(
    percentage = 100 * n / nrow(PAB),          # compute percentage
    patient_variable = "Père et Mère"
  )
# Bind to the original dataset
patients_plot_df <- bind_rows(patients_percentage_df, mere_pere_bar)


patients_plot_df <- patients_plot_df %>%
  mutate(fill_color = ifelse(patient_variable=="Mère seule", rgb(0.235, 0.75, 0.60), rgb(0.240, 0.166, 0.200)))  

# Plot
ggplot(patients_plot_df, aes(x = reorder(patient_variable, -percentage), y = percentage, fill=fill_color)) +
  geom_col(size=0.6) +
  labs(
    title = "Patients reçus",
    x = NULL,
    y=NULL
  ) +
  theme_minimal(base_family="Avenir") +
  theme(
    panel.background=element_blank(),
    panel.grid=element_blank(),
    plot.background=element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  scale_y_continuous(labels = percent_format(scale = 1))


patients_plot_df <- patients_plot_df %>%
  mutate(fill_color = ifelse(patient_variable == "Mère seule",
                             rgb(235, 75, 60, maxColorValue = 255),
                             rgb(240, 166, 200, maxColorValue = 255)))




ggplot(patients_plot_df, aes(x = reorder(patient_variable, -percentage), y = percentage)) +
  geom_col(aes(fill = fill_color), colour = "black", size = 0.6) +
  scale_fill_identity() +
  labs(
    title = "Patients reçus",
    x = "Type de patient",
    y = NULL
  ) +
  theme_minimal(base_family = "Avenir") +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

#### PLOT

patients <- PAB %>%
  select(patient1, patient2, patient5, patient6, patient7, patient8, enfant) %>%
  group_by(enfant)


patients_count <- patients %>%
  pivot_longer(everything(), names_to = "patient_col", values_to = "value") %>%
  filter(!is.na(value)) %>%
  count(value)

patients_count_percent <- patients_count %>%
  mutate(percent = n / sum(n) * 100)

ggplot(patients_count_percent, aes(x = fct_reorder(value, percent), y = percent, fill=enfant)) +
  geom_col(fill="#F0A6C8") +
  labs(x = "Patient", y = "Count") +
  theme_minimal()+
  labs(
    title = NULL,
    x = NULL,
    y = NULL
  )+
  geom_text(aes(label = paste0(round(percent, 1), "%")), vjust = -0.5, size=5, family="Avenir") +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text.x = element_text(size=12, angle = 0, hjust = 1),
    axis.text.y=element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    
## Plot by enfant
library(dplyr)
library(tidyr)
library(ggplot2)

patients_stack <- PAB %>%
  select(patient1, patient2, patient5, patient6, patient7, patient8, enfant) %>%
  pivot_longer(cols = starts_with("patient"),
               names_to = "patient",
               values_to = "present") %>%
  filter(!is.na(present)) %>%
  group_by(patient, enfant) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(patient) %>%
  mutate(percent = n / sum(n) * 100) %>%
  mutate(value_recode = recode(patient,
                               "patient1" = "Mère",
                               "patient2" = "Père",
                               "patient5"="Belle-mère",
                               "patient6"="Beau-père",
                               "patient7"="Grand-mère",
                               "patient8"="Grand-père"))

ggplot(patients_stack, aes(x = value_recode, y = percent, fill = factor(enfant))) +
  geom_col(position = "stack") +
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(values = c("0" = "#F0A6C8", "1" = "#EB4B3C"),
                    labels = c("0" = "Sans enfant", "1" = "Avec enfant"),
                    name = "Enfant") +
  labs(x = NULL, y = NULL, title = "Répartition avec / sans enfant par patient") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_blank(),
    plot.title = element_text(size = 14, hjust = 0.5)
  )
