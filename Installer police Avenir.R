# Install if not already

install.packages("extrafont")
library(extrafont)

# Import system fonts (do this once)
# Replace "ExactFontName" with the font name found in fonts()
font_import(pattern = "Avenir", prompt = FALSE)
loadfonts(device = "win")


# Load fonts for Windows/Quartz
loadfonts(device = "win")  # use "quartz" on macOS

# Now specify in ggplot2
ggplot(patients_plot_df, aes(x = reorder(patient_variable, -percentage), y = percentage)) +
  geom_col(fill = "#4E79A7", colour = "black", size = 0.6) +
  labs(title = "Pourcentage de répondants par type de patient",
       x = "Type de patient", y = "Pourcentage") +
  theme_minimal(base_size = 12, base_family = "Avenir") +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

