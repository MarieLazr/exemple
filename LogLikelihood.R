library(dplyr)

# 1️⃣ Prepare the data: pivot long and remove NA
type_patients <- PAB %>%
  select(patient1:patient10) %>%  # adjust columns if necessary
  pivot_longer(everything(), names_to = "patient_col", values_to = "type") %>%
  filter(!is.na(type))

# 2️⃣ Count occurrences per type
type_patients_count <- type_patients %>%
  count(type, name = "count")

# 3️⃣ Total number of patients
N <- sum(type_patients_count$count)

# 4️⃣ Observed probabilities (MLE)
p_hat <- type_patients_count$count / N

# 5️⃣ Log-likelihood at MLE
log_likelihood_mle <- sum(type_patients_count$count * log(p_hat))

# 6️⃣ Null model: equal probability for all types
k <- nrow(type_patients_count)
p_null <- rep(1/k, k)
log_likelihood_null <- sum(type_patients_count$count * log(p_null))

# 7️⃣ Likelihood ratio test
LR_stat <- 2 * (log_likelihood_mle - log_likelihood_null)
p_value <- pchisq(LR_stat, df = k - 1, lower.tail = FALSE)

# 8️⃣ Output results
type_patients_count
log_likelihood_mle
log_likelihood_null
LR_stat
p_value
