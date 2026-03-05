# ==============================================================================
# Project: Prediction of Hip Fracture Risk in Older Chinese Adults (CHARLS Study)
# Script: Part 7 - Model Validation (ROC Curves & Calibration Plots)
# Purpose: Evaluate the discrimination (AUC) and calibration (Reliability) 
#          of the Top 9 "Golden Model" across Train, Val, and Test sets.
# ==============================================================================

# 1. Load required libraries
if (!require(pROC)) install.packages("pROC")
if (!require(rms)) install.packages("rms")

library(pROC)
library(rms)
library(dplyr)

# ------------------------------------------------------------------------------
# Stage 1: Fitting the Final "Golden" Model
# ------------------------------------------------------------------------------
# Define the Top 9 predictor set identified through our previous reduction steps.
# Note: Variables like 'fall_down' and 'adlab_c' are consistently strong predictors.

top_9_vars <- c("ADL_6", "Respiratory", "Cognitive_ability", "Self_pay_rate", 
                "Moderate_physical_activity", "Tooth_Loss", "Lumbago", 
                "History_of_falls", "Hospitalization")

# Fit the final logistic regression model on the Training Set
final_model_9 <- glm(
  as.formula(paste("Hip_fracture ~", paste(top_9_vars, collapse = " + "))),
  data = train_set,
  family = binomial
)

# ------------------------------------------------------------------------------
# Stage 2: Discrimination Analysis (ROC & AUC)
# ------------------------------------------------------------------------------

# Generate predicted probabilities for all three datasets
prob_train <- predict(final_model_9, newdata = train_set, type = "response")
prob_val   <- predict(final_model_9, newdata = val_set,   type = "response")
prob_test  <- predict(final_model_9, newdata = test_set,  type = "response")

# Compute ROC objects
roc_train <- roc(train_set$Hip_fracture, prob_train, quiet = TRUE)
roc_val   <- roc(val_set$Hip_fracture,   prob_val,   quiet = TRUE)
roc_test  <- roc(test_set$Hip_fracture,  prob_test,  quiet = TRUE)

# Print AUC with 95% Confidence Intervals
cat("--- Model Performance: Discrimination (AUC) ---\n")
cat(sprintf("Training Set AUC:   %.3f (95%% CI: %.3f-%.3f)\n", 
            auc(roc_train), ci(roc_train)[1], ci(roc_train)[3]))
cat(sprintf("Validation Set AUC: %.3f (95%% CI: %.3f-%.3f)\n", 
            auc(roc_val),   ci(roc_val)[1],   ci(roc_val)[3]))
cat(sprintf("Test Set AUC:       %.3f (95%% CI: %.3f-%.3f)\n", 
            auc(roc_test),  ci(roc_test)[1],  ci(roc_test)[3]))

# Plot Triple ROC Curves (Publication Quality)

plot(roc_train, col = "#2E7D32", lwd = 3, main = "Discrimination: ROC Curves") # Dark Green
plot(roc_val,   add = TRUE, col = "#1976D2", lwd = 3, lty = 2)                # Blue Dashed
plot(roc_test,  add = TRUE, col = "#D32F2F", lwd = 3, lty = 3)                # Red Dotted
legend("bottomright", 
       legend = c(paste0("Train AUC: ", round(auc(roc_train), 3)),
                  paste0("Val AUC:   ", round(auc(roc_val), 3)),
                  paste0("Test AUC:  ", round(auc(roc_test), 3))),
       col = c("#2E7D32", "#1976D2", "#D32F2F"), 
       lwd = 3, lty = c(1, 2, 3), bty = "n")

# ------------------------------------------------------------------------------
# Stage 3: Calibration Analysis (Reliability of Predictions)
# ------------------------------------------------------------------------------
# Calibration evaluates how close the predicted probabilities are to the 
# actual observed outcomes.

# Re-fitting with 'lrm' (rms package) to enable built-in calibration functions
dd <- datadist(train_set)
options(datadist = 'dd')

f_rms <- lrm(as.formula(paste("Hip_fracture ~", paste(top_9_vars, collapse = " + "))), 
             data = train_set, x = TRUE, y = TRUE)

# Set up side-by-side plotting
par(mfrow = c(1, 2))

# A. Internal Calibration (Bootstrap Resampling)
# 400 iterations of bootstrapping to correct for optimism bias
cal_val <- calibrate(f_rms, method = 'boot', B = 400) 
plot(cal_val, main = "Calibration: Internal (Bootstrap)")

# B. External/Independent Calibration (Test Set)
# Using val.prob for direct observation of predicted vs. observed

val.prob(p = prob_test, y = test_set$Hip_fracture, statloc = FALSE)
title("Calibration: Independent Test Set")

# Reset plotting parameters
par(mfrow = c(1, 1))

# ============================== End of Part 7 ================================