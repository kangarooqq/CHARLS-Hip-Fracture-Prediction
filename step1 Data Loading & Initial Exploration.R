# ==============================================================================
# Project: Prediction of Hip Fracture Risk in Older Chinese Adults (CHARLS Study)
# Script: Part 1 - Data Loading and Preliminary Exploration
# Purpose: Load the raw dataset and inspect key variables for data cleaning.
# ==============================================================================

# 1. Install and load necessary libraries
# Use 'haven' for reading Stata (.dta) files
if (!require(haven)) install.packages("haven")
if (!require(dplyr)) install.packages("dplyr")

library(haven)
library(dplyr)

# 2. Set file path (Note: Forward slashes '/' are used for cross-platform compatibility)
# Please update this path according to your local directory structure
file_path <- "charls.dta"

# 3. Load the dataset
# CHARLS raw data is typically provided in Stata format
charls_data <- read_dta(file_path)

# 4. Initial data inspection
# Check dimensions (number of observations and variables)
dim_info <- dim(charls_data)
cat("The dataset contains", dim_info[1], "observations (rows) and", dim_info[2], "variables (columns).\n")

# Overview of the first 5 rows and the first 5 variables
# This helps verify if the data was loaded correctly with proper headers
print("Preview of the first 5 rows and columns:")
print(head(charls_data[, 1:min(5, ncol(charls_data))]))

# Check all variable names to confirm core predictors are present
# CHARLS is a longitudinal panel study; an individual (ID) may have multiple records across waves (1 to 5).
colnames(charls_data)

# ------------------------------------------------------------------------------
# According to the Inclusion and Exclusion Criteria (Section 2.2 of the manuscript):
# Next steps will focus on:
#   1. Selecting participants aged ≥60 years.
#   2. Identifying the primary outcome variable (hip fracture status).
#   3. Excluding pathological fractures (e.g., those caused by cancer/malignancy).
#   4. Handling duplicates (ensuring only one record per unique Participant ID).
# ------------------------------------------------------------------------------

# 5. Descriptive check of the Outcome Variable (hip fracture)
# Includes checking for missing values (NAs)
cat("\n--- Distribution of Hip Fracture (Outcome Variable: 'hip') ---\n")
table(charls_data$hip, useNA = "always")

# 6. Descriptive check of Age distribution
# Essential for confirming the elderly population criteria
cat("\n--- Age Distribution Summary ---\n")
summary(charls_data$age)

# 7. Descriptive check of Gender distribution
# Essential for the subsequent 1:1 propensity score or case-control matching
cat("\n--- Gender Distribution ---\n")
table(charls_data$gender, useNA = "always")

# 8. Descriptive check of Cancer/Malignancy
# Used to exclude cases of pathological fractures
cat("\n--- Cancer Prevalence (Criteria for Exclusion) ---\n")
table(charls_data$cancre, useNA = "always")

# 9. Inspection of Unique Respondents
# To identify the number of unique participants before deduplication
cat("\n--- Number of Unique Participant IDs ---\n")
length(unique(charls_data$ID))

# ============================== End of Part 1 ================================