## Importation de la base de données 
library(readr)
PAB <- read_delim("Pâtes au Beurre - questionnaire Professionnel_v2.xlsx2.csv", 
                                                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(PAB)

## Statistiques descriptives de la base de données
summary(PAB)
library(ggplot2)

#### Ville de provenance
library(ggplot2)
ggplot(PAB, aes(x = lieu_accueil)) +
  geom_bar(fill = "#4E79A7") +  # nice blue color
  labs(title = "Nombre de personnes par ville", x = "Ville", y = "Nombre") +
  theme_minimal() +  # clean background
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),  # remove vertical grid lines
    panel.grid.minor = element_blank()
  )


library(dplyr)
library(ggplot2)

## Count number of people per city and calculate percentage
df_percent <- PAB %>%
  count(lieu_accueil) %>%
  mutate(percentage = n / sum(n) * 100)

##### Create the plot - PLOT WE ARR KEEPING
ggplot(df_percent, aes(x = reorder(lieu_accueil, -percentage), y = percentage)) +
  geom_col(fill = rgb(240, 166, 200, maxColorValue = 255)
) +
  labs(title = "Répartition des patients reçus par ville",
       x = "Ville",
       y = NULL) +
  theme_minimal() +
  theme(
    panel.grid=element_blank(),
    plot.background=element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

#### 

library(tidyr)
library(dplyr)
library(ggplot2)

# Exemple : df contient les colonnes patient1 à patient6 et type_patient
patients_long <- PAB %>%
  pivot_longer(
    cols = starts_with("patient"),
    names_to = "patient_variable",
    values_to = "valeur"
  )

patients_percentage <- patients_long %>%
  count(patient_variable) %>%
  mutate(percentage = n / sum(n) * 100)


patients_percentage <- patients_long %>%
  group_by(patient_variable) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(patient_variable) %>%
  mutate(percentage = n / sum(n) * 100)

#### Again

# Total number of respondents
total_respondents <- nrow(PAB)

patient_vars <- paste0("patient", 1:6)

# Calculate % of respondents with a non-NA or non-empty response for each variable
patients_percentage <- sapply(PAB[patient_vars], function(col) {
  sum(!is.na(col) & col != "") / total_respondents * 100
})

# Convert to data frame for plotting
patients_percentage_df <- data.frame(
  patient_variable = names(patients_percentage),
  percentage = as.numeric(patients_percentage)
)

library(scales)
library(ggplot2)
library(dplyr)

patients_percentage_df <- patients_percentage_df %>%
  mutate(patient_variable = recode(patient_variable,
                                   "patient1" = "Mère",
                                   "patient2" = "Père",
                                   "patient5" = "Belle mère",
                                   "patient6" = "Beau père",
                                   "patient7"="Grand-mère",
                                   "patient8"="Grand-Père",
                                   "patient10"="Professionnel")) %>%
  filter(patient_variable %in% c("Mère", "Père", "Belle mère", "Beau père", "Grand-mère", "Grand-père", "Professionnel"))

ggplot(patients_percentage_df, aes(x = reorder(patient_variable, -percentage), y = percentage)) +
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

## Adding a row for both parents
mere_pere_pct <- patients_percentage_df %>%
  filter(patient_variable %in% c("Mère", "Père")) %>%
  summarise(percentage = sum(percentage)) %>%
  mutate(patient_variable = "Mère & Père")

# Add it to the dataset
patients_percentage_df <- bind_rows(patients_percentage_df, mere_pere_pct)

#Plot with the option of both parents
ggplot(patients_percentage_df, aes(x = reorder(patient_variable, -percentage), y = percentage)) +
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
