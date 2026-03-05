# ==============================================================================
# Project: Prediction of Hip Fracture Risk in Older Chinese Adults (CHARLS Study)
# Script: Part 2 - Data Cleaning, Sample Flattening, and 1:1 Matching
# Purpose: Apply inclusion/exclusion criteria and perform Age-Sex matching.
# ==============================================================================

# 1. Load required libraries
if (!require(dplyr)) install.packages("dplyr")
if (!require(MatchIt)) install.packages("MatchIt")

library(dplyr)
library(MatchIt)

# ------------------------------------------------------------------------------
# 2. Apply Inclusion and Exclusion Criteria
# ------------------------------------------------------------------------------
# Criteria: 
#   - Exclude participants with missing key outcomes (hip fracture, age, gender)
#   - Exclude pathological fractures (history of cancer/malignancy)
#   - Focus on the elderly population (age >= 60)

charls_clean_step1 <- charls_data %>%
  # Remove records with missing values in primary variables
  filter(!is.na(hip) & !is.na(age) & !is.na(gender)) %>%
  # Exclude cases with a history of cancer to avoid pathological fractures
  # (Keeping those with cancre == 0 or NA, as per clinical protocol)
  filter(is.na(cancre) | cancre == 0) %>%
  # Include only individuals aged 60 years or older
  filter(age >= 60)

cat("--- Summary After Preliminary Cleaning ---\n")
cat("Total observations (person-sessions):", nrow(charls_clean_step1), "\n")
cat("Unique participants (Unique IDs):", length(unique(charls_clean_step1$ID)), "\n")

# ------------------------------------------------------------------------------
# 3. Identify Fracture Status (Longitudinal Logic)
# ------------------------------------------------------------------------------
# Define a participant as a 'fracture patient' if they reported a hip fracture 
# in any wave of the study.
fracture_status <- charls_clean_step1 %>%
  group_by(ID) %>%
  summarise(has_fracture = max(hip, na.rm = TRUE))

cat("\n--- Distribution of Hip Fracture Cases (0 = No, 1 = Yes) ---\n")
table(fracture_status$has_fracture)

# ------------------------------------------------------------------------------
# 4. Construct Cross-sectional Dataset (Flattening)
# ------------------------------------------------------------------------------
# Merge fracture status back to the cleaned dataset
charls_with_label <- charls_clean_step1 %>%
  left_join(fracture_status, by = "ID")

# CASE GROUP: Select the FIRST wave where a participant reported a hip fracture
cases_data <- charls_with_label %>%
  filter(has_fracture == 1 & hip == 1) %>%
  arrange(ID, wave) %>%
  group_by(ID) %>%
  slice(1) %>% 
  ungroup()

# CONTROL GROUP: Select the earliest available record for those never reporting a fracture
controls_data <- charls_with_label %>%
  filter(has_fracture == 0) %>%
  arrange(ID, wave) %>%
  group_by(ID) %>%
  slice(1) %>%
  ungroup()

# Combine cases and controls into a unified cross-sectional dataset
cross_sectional_data <- bind_rows(cases_data, controls_data)

cat("\n--- Cross-sectional Dataset Construction Complete ---\n")
cat("Total unique participants for matching:", nrow(cross_sectional_data), "\n")

# ------------------------------------------------------------------------------
# 5. 1:1 Age- and Sex-Matching
# ------------------------------------------------------------------------------
# We use Propensity Score Matching with exact matching for gender and 
# nearest neighbor matching for age to replicate the paper's methodology.

set.seed(2027) # For reproducibility
match_out <- matchit(has_fracture ~ age + gender, 
                     data = cross_sectional_data, 
                     method = "nearest",  # Nearest neighbor matching
                     exact = ~ gender,    # Enforce exact matching for Gender
                     ratio = 1)           # 1:1 ratio

# Extract the final matched dataset
matched_data <- match.data(match_out)

# ------------------------------------------------------------------------------
# 6. Post-Matching Validation
# ------------------------------------------------------------------------------
cat("\n--- Distribution After 1:1 Matching ---\n")
table(matched_data$has_fracture)

# Validate balance: Mean age by group
cat("\n--- Mean Age by Group Post-Matching ---\n")
tapply(matched_data$age, matched_data$has_fracture, mean)

# Validate balance: Gender distribution by group
cat("\n--- Gender Distribution by Group Post-Matching ---\n")
table(matched_data$gender, matched_data$has_fracture)

# ============================== End of Part 2 ================================