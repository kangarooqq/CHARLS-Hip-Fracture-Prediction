# ==============================================================================
# Project: Prediction of Hip Fracture Risk in Older Chinese Adults (CHARLS Study)
# Script: Part 4 - Data Partitioning (Stratified Sampling)
# Purpose: Split the dataset into Training, Validation, and Test sets 
#          while maintaining a 1:1 case-control ratio across all subsets.
# ==============================================================================

# 1. Load required library
# 'caret' is the gold standard for machine learning data partitioning in R
if (!require(caret)) install.packages("caret")
library(caret)

# 2. Check the total sample size before splitting
total_n <- nrow(final_model_data)
cat("Total sample size before partitioning:", total_n, "\n")

# ------------------------------------------------------------------------------
# 3. Step 1: Extract the Test Set (20% of the total data)
# ------------------------------------------------------------------------------
# As per Section 2.4 of the manuscript, 20% of participants are reserved 
# for final model testing (the "Hold-out" set).
# We use stratified sampling on the 'Hip_fracture' variable to ensure the 
# 1:1 ratio is preserved.

set.seed(2027) # Set seed for reproducibility
test_index <- createDataPartition(final_model_data$Hip_fracture, p = 0.20, list = FALSE)

test_set      <- final_model_data[test_index, ]
train_val_set <- final_model_data[-test_index, ]

# ------------------------------------------------------------------------------
# 4. Step 2: Split the remaining 80% into Training and Validation Sets (8:2)
# ------------------------------------------------------------------------------
# The remaining 80% of the data is further divided into:
#   - Training Set: 80% (used for model development and feature selection)
#   - Validation Set: 20% (used for hyperparameter tuning and early evaluation)
# Effectively: 64% of total data = Training; 16% = Validation.

set.seed(2027)
train_index <- createDataPartition(train_val_set$Hip_fracture, p = 0.80, list = FALSE)

train_set <- train_val_set[train_index, ]
val_set   <- train_val_set[-train_index, ]

# ------------------------------------------------------------------------------
# 5. Verify the Partitioning Results
# ------------------------------------------------------------------------------
cat("\n--- Data Partitioning Summary ---\n")
cat("Training Set:   ", nrow(train_set), " (", round(nrow(train_set)/total_n*100, 1), "%)\n")
cat("Validation Set: ", nrow(val_set),   " (", round(nrow(val_set)/total_n*100, 1), "%)\n")
cat("Test Set:       ", nrow(test_set),  " (", round(nrow(test_set)/total_n*100, 1), "%)\n")

# ------------------------------------------------------------------------------
# 6. Check Class Balance (Hip Fracture vs. Controls)
# ------------------------------------------------------------------------------
# In clinical prediction models, maintaining the original prevalence in 
# each split is critical to avoid bias in AUC and calibration metrics.

cat("\n--- Class Distribution in Training Set (0=Control, 1=Fracture) ---\n")
print(table(train_set$Hip_fracture))

cat("\n--- Class Distribution in Test Set (0=Control, 1=Fracture) ---\n")
print(table(test_set$Hip_fracture))

# ============================== End of Part 4 ================================
