
# requirements.R
# Install required R packages for the malaria ML project

packages <- c(
  "readxl",
  "dplyr",
  "ggplot2",
  "caret",
  "car",
  "corrplot",
  "randomForest",
  "e1071",
  "xgboost",
  "gbm",
  "naivebayes",
  "ROCR",
  "pROC",
  "janitor",
  "smotefamily",
  "adabag",
  "glmnet",
  "rpart",
  "rpart.plot",
  "class",
  "MLmetrics"
)

install.packages(packages)
