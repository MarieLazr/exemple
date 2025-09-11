summary(PAB)

summary(nb_enfants_count)

# Count of respondents with enfant = 0
acc <- sum(PAB$enfant == 0, na.rm = TRUE)

# Total number of respondents (excluding NA)
n <- sum(!is.na(PAB$enfant))

# Percentage
perc_acc <- (acc / n) * 100

# Print results
acc
perc_acc
