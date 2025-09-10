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
