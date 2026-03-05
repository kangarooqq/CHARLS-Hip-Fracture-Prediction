# ==============================================================================
# Project: Prediction of Hip Fracture Risk in Older Chinese Adults (CHARLS Study)
# Script: Part 5 - Feature Selection (Univariate Logistic & LASSO Regression)
# Purpose: Identify key predictors using a two-stage screening approach to 
#          ensure model parsimony and predictive power.
# ==============================================================================

# 1. Load required libraries
if (!require(glmnet)) install.packages("glmnet")
library(dplyr)
library(glmnet)

# ------------------------------------------------------------------------------
# Stage 1: Batch Univariate Logistic Regression
# ------------------------------------------------------------------------------
# We evaluate each of the 143 potential predictors individually in the Training Set 
# to identify variables significantly associated with hip fractures (P < 0.05).

predictors <- setdiff(names(train_set), "Hip_fracture")
univariate_results <- data.frame(
  Variable = character(),
  OR = numeric(),
  P_value = numeric(),
  stringsAsFactors = FALSE
)

cat("Running batch univariate logistic regression analysis...\n")

for (var in predictors) {
  # Construct formula dynamically (e.g., Hip_fracture ~ Age)
  formula_str <- paste0("Hip_fracture ~ `", var, "`")
  
  tryCatch({
    # Fit binary logistic model
    model <- glm(as.formula(formula_str), data = train_set, family = binomial)
    summary_mod <- summary(model)
    
    # Extract P-value and Odds Ratio (OR)
    p_val <- summary_mod$coefficients[2, 4]
    or_val <- exp(coef(model)[2])
    
    univariate_results <- rbind(univariate_results, data.frame(
      Variable = var,
      OR = or_val,
      P_value = p_val
    ))
  }, error = function(e) {
    # Skip variables that fail to converge (e.g., zero variance)
  })
}

# Filter significant variables (P < 0.05)
sig_vars <- univariate_results %>%
  filter(P_value < 0.05) %>%
  arrange(P_value)

cat("\n--- Univariate Analysis Summary ---\n")
cat("Total predictors analyzed:", length(predictors), "\n")
cat("Predictors with P < 0.05:", nrow(sig_vars), "\n")

# Store significant variable names for LASSO analysis
candidate_vars_for_lasso <- sig_vars$Variable

# ------------------------------------------------------------------------------
# Stage 2: Feature Selection via LASSO Regression
# ------------------------------------------------------------------------------
# Least Absolute Shrinkage and Selection Operator (LASSO) is used to handle 
# multicollinearity and perform feature reduction. We use 10-fold cross-validation.

# 1) Pre-LASSO Exclusion: Remove confounders with potential reverse causality
# (e.g., 'disability' or 'arthritis-related pain' might be the result, not the cause)
exclude_vars <- c("disability", "arthre") 
final_lasso_candidates <- setdiff(candidate_vars_for_lasso, exclude_vars)

# 2) Prepare data matrix for glmnet
X_train <- train_set %>% select(all_of(final_lasso_candidates))
X_matrix <- model.matrix(~ . - 1, data = X_train)
Y_vector <- train_set$Hip_fracture

# 3) Execute LASSO with 10-fold Cross-Validation
# alpha=1 denotes standard LASSO
cat("Starting 10-fold Cross-Validation for LASSO...\n")
set.seed(2027) 
cv_lasso <- cv.glmnet(
  x = X_matrix, 
  y = Y_vector, 
  family = "binomial", 
  alpha = 1,            
  nfolds = 10           
)

# 4) Extract Optimal Lambda Values
# lambda.min: The value that gives minimum mean cross-validated error.
# lambda.1se: The largest value of lambda such that the error is within 1 standard error of the minimum.
lambda_min <- cv_lasso$lambda.min
lambda_1se <- cv_lasso$lambda.1se

cat("\n--- LASSO Cross-Validation Results ---\n")
cat("Optimal Lambda (lambda.min):", round(lambda_min, 4), "\n")
cat("Parsimonious Lambda (lambda.1se):", round(lambda_1se, 4), "\n")

# 5) Identify selected variables
# Variables selected by lambda.min (more complex model)
coef_min <- coef(cv_lasso, s = "lambda.min")
vars_min <- rownames(coef_min)[which(coef_min != 0)] %>% setdiff("(Intercept)")

# Variables selected by lambda.1se (more robust, parsimonious model)
coef_1se <- coef(cv_lasso, s = "lambda.1se")
vars_1se <- rownames(coef_1se)[which(coef_1se != 0)] %>% setdiff("(Intercept)")

cat("\nNumber of variables selected by lambda.min:", length(vars_min), "\n")
cat("Number of variables selected by lambda.1se (Preferred for Nomogram):", length(vars_1se), "\n")

# 6) Display variables selected for the final model
print("Variables selected by lambda.min:")
print(vars_min)

# ============================== End of Part 5 ================================