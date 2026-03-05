# Prediction of Hip Fracture Risk in Older Chinese Adults (CHARLS Study)

This repository contains the R implementation for developing and validating a clinical prediction model to estimate hip fracture risk among the elderly population in China, utilizing data from the China Health and Retirement Longitudinal Study (CHARLS).

## 📌 Project Overview
Hip fractures are a major cause of morbidity and mortality in the elderly. This project aims to provide a robust, evidence-based tool for clinicians to identify high-risk individuals through a simplified set of predictors.

**Key Features:**
* **Data Source:** CHARLS national longitudinal data.
* **Methodology:** Stratified sampling, Univariate screening, and LASSO regression for feature reduction.
* **Clinical Tool:** A high-accuracy Nomogram for individualized risk assessment.
* **Validation:** Comprehensive evaluation using ROC curves, Calibration plots, and Decision Curve Analysis (DCA).

---

## 🛠 Methodology
The model development followed a rigorous statistical pipeline:
1. **Data Preprocessing:** Handling missing values and class balancing (1:1 case-control ratio).
2. **Feature Selection:** Initial screening via univariate logistic regression followed by LASSO (Least Absolute Shrinkage and Selection Operator) to handle multicollinearity.
3. **Model Refinement:** Stepwise regression (BIC-based) and "Elbow Point" analysis to select the most parsimonious variable set.
4. **Validation:** Performance verification across Training (64%), Validation (16%), and Independent Test (20%) sets.

---

## 📊 Model Variables (Top 9)
The final model, visualized in the Nomogram, incorporates the following 9 clinical predictors:

| Variable | Description |
| :--- | :--- |
| **History of falls** | Previous incidents of falling |
| **Hospitalization** | Frequency or status of hospital stays |
| **Cognitive ability** | Evaluation of memory and cognitive function |
| **ADL_6** | Activities of Daily Living score |
| **Respiratory** | Lung function/Peak Expiratory Flow (PEF) |
| **Self_pay_rate** | Ratio of medical expenses paid out-of-pocket |
| **Moderate physical activity** | Engagement in exercise |
| **Tooth Loss** | Status of dental health/loss |
| **Lumbago** | History of lower back pain |

---

## 📈 Visual Results

### 1. The Nomogram
The Nomogram allows for the calculation of an individual's total points to map the specific probability of a hip fracture.

![Nomogram]

### 2. Model Performance
* **Discrimination:** The model achieved an AUC of approximately **0.82** across all datasets.
* **Clinical Utility:** Decision Curve Analysis (DCA) confirms significant net benefit within clinical threshold ranges.

---

## 💻 How to Run the Code

### Prerequisites
Ensure you have R installed along with the following libraries:
```R
install.packages(c("rms", "pROC", "glmnet", "dplyr", "ggplot2", "ggDCA", "MASS"))
