# malaria-ml-diagnosis
# Machine Learning Models for Malaria Diagnosis

This repository contains the R scripts used in the study:

**"Ensemble Machine Learning for Malaria Diagnosis in Resource-Limited Settings."**

The repository provides the full analytical workflow used to develop and evaluate machine learning models for malaria diagnosis using retrospective clinical data.

The aim of this repository is to support **transparency and reproducibility** of the analytical methods used in the manuscript.

---

# Study Overview

Malaria remains a major public health challenge in many endemic regions. Accurate and timely diagnosis is critical for effective treatment and control.

This study evaluates multiple machine learning algorithms for predicting malaria infection using routinely collected clinical and demographic variables. The study compares the predictive performance of several classification models and ensemble methods.

---

# Repository Structure

The repository is organised as follows:

```
malaria-ml-diagnosis
│
├── scripts
│   ├── 00_libraries.R             # Load all libraries
│   ├── 01_data_preprocessing.R
│   ├── 02_feature_selection.R
│   ├── 03_model_training.R
│   ├── 04_model_evaluation.R
│   └── 05_ensemble_models.R
│
├── environment
│   └── requirements.R             # Optional: library install script for reproducibility
│
└── README.md                       # Detailed project description and instructions
```

# Analytical Workflow

The analysis follows the steps below:

1. Data preprocessing

   * Cleaning and formatting the dataset
   * Handling missing values
   * Creation of derived variables (e.g., age groups)

2. Feature selection

   * Variance Inflation Factor (VIF) to assess multicollinearity
   * Recursive Feature Elimination (RFE)

3. Handling class imbalance

   * Synthetic Minority Over-sampling Technique (SMOTE)

4. Model development
   Multiple machine learning models were trained and tuned, including:

   * Logistic Regression (LASSO)
   * Random Forest
   * Decision Tree
   * k-Nearest Neighbors
   * Gradient Boosting Machine
   * Naïve Bayes
   * XGBoost

5. Model evaluation
   Models were evaluated using:

   * Accuracy
   * Precision
   * Recall
   * F1 Score
   * Area Under the ROC Curve (AUC)

6. Ensemble learning methods
   Additional ensemble approaches were explored to improve predictive performance:

   * Bagging
   * Stacking
   * Soft Voting
   * AdaBoost

---

# Reproducibility Instructions

To reproduce the analysis:

1. Install required R packages:

source("environment/requirements.R")

2. Run the scripts sequentially:

```
scripts/00_libraries.R 
scripts/01_data_preprocessing.R
scripts/02_feature_selection.R
scripts/03_model_training.R
scripts/04_model_evaluation.R
scripts/05_ensemble_models.R
```

Each script corresponds to a specific stage of the analysis pipeline.

---

# Data Availability

The dataset used in this study contains anonymized retrospective clinical data obtained from routine hospital records.

Due to ethical and privacy considerations, the full dataset cannot be publicly shared.

Researchers interested in accessing the full dataset for academic purposes may contact the corresponding author.

---

# Software and Environment

All analyses were conducted in **R**.

Key R packages used include:

* caret
* randomForest
* xgboost
* gbm
* naivebayes
* e1071
* ROCR
* pROC
* smotefamily

A full list of required packages is provided in:

environment/requirements.R

---

# Citation

If you use this repository or the code provided, please cite the associated manuscript:

Nyengera P., Takawira H.T., Mlambo F.F.
*Machine Learning Approaches for Malaria Diagnosis Using Clinical and Demographic Data.*

Submitted to **Infectious Disease Reports**.

---

# Contact

For questions regarding the repository or the study, please contact:

**Ms. Panashe Nyengera**
Email: [panashen24@gmail.com](mailto:panashen24@gmail.com)
