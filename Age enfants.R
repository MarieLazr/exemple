library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

age_enfants <- PAB %>%
  select(nb_enfants) %>%
  pivot_longer(everything(), names_to = "patient_col", values_to = "value") %>%
  filter(!is.na(value)) %>%
  filter(value != "Je ne sais pas") %>%
  count(value)

nb_enfants_count_percent <- nb_enfants_count %>%
  mutate(percent = n / sum(n) * 100)

### Ajout des données de comparaison


## Données INSEE

library(readr)
library(readr)
age_des_enfants_comparaison <- read_delim("age des enfants comparaison.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(age_des_enfants_comparaison)

## Données pros
  
library(dplyr)

nb_enfants_pro <- nb_enfants_count_percent %>%
  mutate(age_category = case_when(
    value >= 0 & value <= 2 ~ "0 à 2 ans",
    value >= 3 & value <= 5 ~ "3 à 5 ans",
    value %in% c(6, 7) | value == "Autre" ~ "6 à 17 ans",
    TRUE ~ "Autre"
  ))


age_enfants_pro <- nb_enfants_pro %>%
  group_by(age_category) %>%
  summarise(
    n = sum(n, na.rm = TRUE),
    percent = sum(percent, na.rm = TRUE)
  ) %>%
  ungroup()

## Données parents


library(dplyr)
library(tidyr)

# Select age columns
age_enfants_parents <- Nettoyage_parents %>%
  select(starts_with("age_enfant"))

# Convert to long format to have all ages in one column
age_long <- age_enfants_parents %>%
  pivot_longer(
    cols = everything(),
    values_to = "age"
  ) %>%
  select(age) %>%  # keep only the age column
  filter(!is.na(age)) %>%       # remove missing ages
  mutate(age = as.numeric(gsub("[^0-9]", "", age)))
# Count occurrences and calculate percentages
age_counts <- age_long %>%
  count(age) %>%
  mutate(percent = n / sum(n) * 100)
# Group into categories
age_grouped <- age_counts %>%
  mutate(age_category = case_when(
    age %in% 0:2 ~ "0 à 2 ans",
    age %in% 3:5 ~ "3 à 5 ans",
    age %in% 6:17 ~ "6 à 17 ans"
  )) %>%
  group_by(age_category) %>%
  summarise(
    n = sum(n),
    percent = sum(percent)
  ) %>%
  ungroup()

age_grouped

## Merge des 3 databases

library(dplyr)

age_des_enfants_comparaison_clean <- age_des_enfants_comparaison %>%
  select(`Âge révolu`, pourcentage) %>%
  rename(
    age_category = `Âge révolu`,
    percent_comparaison = pourcentage
  )
age_des_enfants_comparaison_clean <- age_des_enfants_comparaison_clean %>%
  mutate(percent_comparaison = percent_comparaison / 100000000)



# For the professional dataset
age_enfants_pro_clean <- age_enfants_pro %>%
  select(age_category, percent)%>%
  rename(percent_pros = percent)%>%
  filter(!is.na(age_category))


# For the parents dataset
age_grouped_clean <- age_grouped %>%
  select(age_category, percent) %>%
  rename(percent_parents = percent)%>%
  filter(!is.na(age_category))

# Merge all three datasets
age_final_merged <- age_des_enfants_comparaison_clean %>%
  full_join(age_enfants_pro_clean, by = "age_category") %>%
  full_join(age_grouped_clean, by = "age_category")

age_final_merged

library(dplyr)

# Merge parents and pros first
merged_age <- full_join(age_grouped, age_enfants_pro, by = "age_category", suffix = c("_parents", "_pros"))

# Merge with comparaison
final_age <- full_join(merged_age, age_des_enfants_comparaison_clean, by = "age_category")

# Rename columns consistently
final_age <- final_age %>%
  rename(
    percent_parents = percent_parents,
    percent_pros = percent_pros,
    percent_comparaison = percent_comparaison
  ) %>%
  select(age_category, percent_comparaison, percent_pros, percent_parents)

final_age <- final_age %>%
  filter (!is.na(age_category))

final_age <- final_age %>%
  group_by(age_category) %>%
  summarise(
    percent_comparaison = sum(percent_comparaison, na.rm = TRUE),
    percent_pros = sum(percent_pros, na.rm = TRUE),
    percent_parents = sum(percent_parents, na.rm = TRUE)
  ) %>%
  ungroup()

##### PLOT

library(dplyr)
library(tidyr)
library(ggplot2)

# Combine similar age categories by numeric ranges
final_age_combined <- final_age %>%
  mutate(age_group = case_when(
    grepl("0", age_category) ~ "0 à 2 ans",
    grepl("3", age_category) ~ "3 à 5 ans",
    grepl("6", age_category) ~ "6 à 17 ans"
  )) %>%
  group_by(age_group) %>%
  summarise(
    percent_pros = sum(percent_pros, na.rm = TRUE),
    percent_parents = sum(percent_parents, na.rm = TRUE),
    percent_comparaison = sum(percent_comparaison, na.rm = TRUE)
  ) %>%
  ungroup()

# Pivot to long format
final_age_plot_data <- final_age_combined %>%
  pivot_longer(
    cols = c(percent_pros, percent_parents, percent_comparaison),
    names_to = "source",
    values_to = "percent"
  ) %>%
  mutate(
    source = factor(source, levels = c("percent_pros", "percent_parents", "percent_comparaison"))
  )

# Plot
ggplot(final_age_plot_data, aes(x = age_group, y = percent, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(round(percent), "%")),
            position = position_dodge(width = 0.8),
            vjust = -0.5,
            color = "black",
            size = 5) +
  scale_fill_manual(
    values = c(
      "percent_pros" = "#F0A6C8", 
      "percent_parents" = "#F4813D", 
      "percent_comparaison" = "#FF4C4C"
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
  ylim(0, max(final_age_plot_data$percent, na.rm = TRUE) * 1.2)
