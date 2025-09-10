library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)


raisons_long <- PAB %>%
  select(raison1, raison2, raison3, raison4) %>%
  pivot_longer(
    cols = everything(),
    names_to = "raison_variable",
    values_to = "valeur"
  ) %>%
  filter(!is.na(valeur) & valeur != "") 

raisons_count <- raisons_long %>%
  group_by(valeur) %>%
  summarise(count = n()) %>%
  filter(count > 15) 

library(stringr)

ggplot(raisons_count, aes(x = reorder(valeur, -count), y = count)) +
  geom_col(fill = rgb(240, 166, 200, maxColorValue = 255), width = 0.5) +
  labs(title = "Occurrence des raisons de venue aux Pâtes au beurre (N=207)", x = "Raison de la venue aux Pâtes au beurre", y = "Nombre d'occurrences") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid=element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  scale_x_discrete(
    labels = function(x) str_wrap(x, width = 15),
    expand = expansion(add = c(1, 1))  # add space at the edges
  )+
  coord_cartesian(expand = TRUE)
