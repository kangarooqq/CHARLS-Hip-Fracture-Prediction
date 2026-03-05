# ==============================================================================
# Project: Prediction of Hip Fracture Risk in Older Chinese Adults (CHARLS Study)
# Script: Part 6 - Model Parsimony & Variable Set Scoring Analysis
# Purpose: Final feature refinement using Stepwise Regression (AIC/BIC) and 
#          AUC-based elbow analysis to determine the optimal clinical predictors.
# ==============================================================================

# 1. Load required libraries
if (!require(MASS)) install.packages("MASS")
if (!require(car)) install.packages("car")
if (!require(pROC)) install.packages("pROC")

library(MASS)
library(car)
library(pROC)
library(dplyr)

# ------------------------------------------------------------------------------
# Stage 1: Exhaustive Variable Reduction (AIC & BIC)
# ------------------------------------------------------------------------------
# We start with the 28 LASSO-selected variables and perform Stepwise Logistic 
# Regression to eliminate redundant predictors and address potential collinearity.

# Prepare the data subset containing LASSO-selected candidates
train_data_final <- train_set %>%
  dplyr::select(Hip_fracture, dplyr::all_of(vars_1se))

# Fit the full multivariable model
full_model <- glm(Hip_fracture ~ ., data = train_data_final, family = binomial)

# Perform Bi-directional Stepwise Regression based on BIC (Bayesian Information Criterion)
# Note: BIC is more stringent than AIC, penalizing extra variables more heavily 
# to ensure a highly parsimonious clinical model.
cat("Executing strict Stepwise Regression based on BIC...\n")
step_model_bic <- stepAIC(full_model, direction = "both", 
                          k = log(nrow(train_data_final)), 
                          trace = FALSE)

# Check for Multicollinearity using Variance Inflation Factor (VIF)
# A VIF < 5 (ideally < 2) ensures stable Odds Ratio (OR) estimates.
cat("\n--- Multicollinearity Diagnosis (VIF) ---\n")
print(vif(step_model_bic))

# ------------------------------------------------------------------------------
# Stage 2: Variable Importance Ranking & AUC-based Scoring Analysis
# ------------------------------------------------------------------------------
# To find the 'Elbow Point' where model complexity meets clinical utility, 
# we rank variables by their Z-score importance and evaluate incremental AUC.

# Rank variables from the BIC model by absolute Z-value
coef_summary <- summary(step_model_bic)$coefficients
var_importance <- data.frame(
  Variable = rownames(coef_summary)[-1],
  Importance = abs(coef_summary[-1, "z value"])
) %>% arrange(desc(Importance))

ranked_vars <- var_importance$Variable
cat("\n--- Clinical Importance Ranking of Predictors ---\n")
print(var_importance)

# ------------------------------------------------------------------------------
# Stage 3: Incremental Feature Addition Analysis (Learning Curve)
# ------------------------------------------------------------------------------
# We iteratively add the top-ranked variables into the model and calculate 
# the AUC on the Validation Set to identify the optimal subset (e.g., Top 9).

results_df <- data.frame(Num_Vars = integer(), AUC_Train = numeric(), AUC_Val = numeric())

for (i in 1:length(ranked_vars)) {
  current_vars <- ranked_vars[1:i]
  formula_str <- as.formula(paste("Hip_fracture ~", paste(current_vars, collapse = " + ")))
  
  # Train model on Training Set
  temp_model <- glm(formula_str, data = train_set, family = binomial)
  
  # Evaluate on Training Set
  pred_train <- predict(temp_model, train_set, type = "response")
  auc_train <- as.numeric(auc(roc(train_set$Hip_fracture, pred_train, quiet = TRUE)))
  
  # Evaluate on Validation Set (Generalization Performance)
  pred_val <- predict(temp_model, val_set, type = "response")
  auc_val <- as.numeric(auc(roc(val_set$Hip_fracture, pred_val, quiet = TRUE)))
  
  results_df <- rbind(results_df, data.frame(Num_Vars = i, AUC_Train = auc_train, AUC_Val = auc_val))
}

# ------------------------------------------------------------------------------
# Stage 4: Visualization and Final Subset Selection
# ------------------------------------------------------------------------------



# Plot the 'Variable Count-Model Scoring' curve
plot(results_df$Num_Vars, results_df$AUC_Val, 
     type = "b", pch = 19, col = "#2E86C1", lwd = 2,
     xlab = "Number of Variables in Model", 
     ylab = "Validation Set AUC",
     main = "Variable Set Scoring Analysis (Model Parsimony)")

# Identify the optimal number of variables (e.g., Top 9 as per original study)
best_num <- 9 # Manually set to 9 based on original study design or elbow point
abline(v = best_num, col = "#E74C3C", lty = 2)
points(best_num, results_df$AUC_Val[best_num], col = "#E74C3C", pch = 19, cex = 1.5)

# Extract the final clinical predictor set
top_9_vars <- ranked_vars[1:9]
cat("\n--- Final Clinical Predictor Set (Top 9) ---\n")
print(top_9_vars)

# ============================== End of Part 6 ================================