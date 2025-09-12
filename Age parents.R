library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

age_parents <- Nettoyage_parents %>%
  select(age)

library(dplyr)

age_parents_summary <- age_parents %>%
  filter(!is.na(age)) %>%                     # remove missing values
  count(age) %>%                              # count occurrences of each age
  mutate(percent = n / sum(n) * 100)         # compute percentages

age_parents_summary

library(ggplot2)
library(dplyr)

# Assuming your tibble is called age_parents_summary
ggplot(age_parents_summary, aes(x = age, y = percent)) +
  geom_col(fill = "#F4813D", width = 0.7) +               # orange bars
  geom_text(aes(label = paste0(round(percent, 1), "%")),  # labels on top
            vjust = -0.5, color = "black", size = 4) +
  labs(x = NULL, y = NULL, title = NULL) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black")
  ) +
  ylim(0, max(age_parents_summary$percent) * 1.2)

### Horizontal version

library(ggplot2)
library(dplyr)

# Horizontal bar chart
ggplot(age_parents_summary, aes(y = age, x = percent)) +
  geom_col(fill = "#F4813D", width = 0.7) +                # orange bars
  geom_text(aes(label = paste0(round(percent, 1), "%")),   # labels at end of bars
            hjust = -0.1, color = "black", size = 4) +
  labs(x = NULL, y = NULL, title = NULL) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black")
  ) +
  coord_cartesian(xlim = c(0, max(age_parents_summary$percent) * 1.2))
