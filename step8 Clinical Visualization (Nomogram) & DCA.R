# ==============================================================================
# Project: Prediction of Hip Fracture Risk in Older Chinese Adults
# Script: Part 8 - Clinical Visualization (Nomogram)
# Purpose: Generate a high-resolution Nomogram for individualized risk assessment.
# ==============================================================================

library(rms)

# 1. Prepare Data Environment with Professional Labels
plot_data <- train_set

# Assign professional clinical labels for visual clarity in the figure
# 为变量赋予诺莫图上的专业全称（用于绘图显示）
label(plot_data$ADL_6)                  <- "ADL_6"
label(plot_data$Respiratory)            <- "Respiratory"
label(plot_data$Cognitive_ability)      <- "Cognitive ability"
label(plot_data$Self_pay_rate)          <- "Self_pay_rate"
label(plot_data$Moderate_physical_activity) <- "Moderate physical activity"
label(plot_data$Tooth_Loss)             <- "Tooth Loss"
label(plot_data$Lumbago)                <- "Lumbago"
label(plot_data$History_of_falls)       <- "History of falls"
label(plot_data$Hospitalization)        <- "Hospitalization"

# Initialize data distribution for rms package
dd <- datadist(plot_data)
options(datadist = 'dd')

# 2. Fit the Final Logistic Regression Model (L2 Regularized equivalent)
# Including the Top 10 predictors identified in previous steps
f_final <- lrm(Hip_fracture ~ fall_down + executive + da042s11 + memeory + 
                 pulse + wspeed2 + family_size + mwaist + teeth + adlab_c, 
               data = plot_data, x=TRUE, y=TRUE)

# 3. Configure Nomogram Parameters
# Define the inverse logit function to map scores to probabilities
prop_fun <- function(x) 1/(1 + exp(-x))

nom <- nomogram(f_final, 
                fun = prop_fun, 
                fun.at = c(0.01, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9), 
                funlabel = "Predicted Probability of Hip Fracture", 
                lp = FALSE, 
                conf.int = FALSE, 
                abbrev = FALSE)

# 4. High-Resolution Output (PDF format recommended for submission)
pdf("Figure6_Nomogram_Hip_Fracture.pdf", width = 12, height = 9)

plot(nom, 
     xfrac = 0.35,              # Margin for long variable labels
     cex.axis = 0.85,           # Font size for axis numbers
     cex.var = 1.1,             # Font size for variable names
     lmgp = 0.25,               # Label-to-tick spacing
     tcl = -0.3,                # Tick length
     label.every = 1,           # Ensure every tick label is displayed
     force.label = TRUE,        # Prevent overlapping label removal
     main = "Nomogram for Predicting Hip Fracture Risk in Older Adults")

dev.off()


# ==============================================================================
# Clinical Utility (DCA)
# Purpose: Calculate Net Benefit (NB) to demonstrate clinical value.
# ==============================================================================

if(!require(ggDCA)) install.packages("ggDCA")
library(ggDCA)
library(ggplot2)

# 1. Fit models for all three sets to demonstrate generalizable benefit
# This is a standard requirement for high-impact journals
fit_train <- lrm(Hip_fracture ~ fall_down + executive + da042s11 + memeory + 
                   pulse + wspeed2 + family_size + mwaist + teeth + adlab_c, 
                 data = train_set, x=TRUE, y=TRUE)

fit_val <- lrm(Hip_fracture ~ fall_down + executive + da042s11 + memeory + 
                 pulse + wspeed2 + family_size + mwaist + teeth + adlab_c, 
               data = val_set, x=TRUE, y=TRUE)

fit_test <- lrm(Hip_fracture ~ fall_down + executive + da042s11 + memeory + 
                  pulse + wspeed2 + family_size + mwaist + teeth + adlab_c, 
                data = test_set, x=TRUE, y=TRUE)

# 2. Run DCA and extract data frames
d_train <- dca(fit_train)
d_val   <- dca(fit_val)
d_test  <- dca(fit_test)

df_train <- as.data.frame(d_train)
df_val   <- as.data.frame(d_val)
df_test  <- as.data.frame(d_test)

# 3. Create Publication-Quality DCA Plot using ggplot2
ggplot() +
  # Model Curves: Net Benefit (NB)
  geom_line(data = df_train, aes(x = thresholds, y = NB, color = "Train Set"), size = 1) +
  geom_line(data = df_val,   aes(x = thresholds, y = NB, color = "Validation Set"), size = 1, linetype = "dashed") +
  geom_line(data = df_test,  aes(x = thresholds, y = NB, color = "Test Set"), size = 1, linetype = "dotted") +
  
  # Baseline Curves: Treat All vs. Treat None
  geom_line(data = subset(df_train, model == "All"), 
            aes(x = thresholds, y = NB), color = "gray60", linetype = "longdash") +
  geom_line(data = subset(df_train, model == "None"), 
            aes(x = thresholds, y = NB), color = "black", size = 0.8) +
  
  # Refinement and Themes
  theme_minimal() +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(-0.02, 0.4)) +
  labs(x = "Threshold Probability", 
       y = "Net Benefit (NB)", 
       title = "Decision Curve Analysis: Clinical Utility Score") +
  scale_color_manual(values = c("Train Set" = "#2E7D32", "Validation Set" = "#1976D2", "Test Set" = "#D32F2F")) +
  
  # Annotations for Baselines
  annotate("text", x = 0.9, y = 0.08, label = "Treat All", color = "gray60") +
  annotate("text", x = 0.9, y = 0.02, label = "Treat None", color = "black")