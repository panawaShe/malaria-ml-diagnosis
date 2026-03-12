source("scripts/00_libraries.R")

# ***************************HANDLING CLASS IMBALANCE **********************************

# Check the class distribution of the target variable (Diagnosis)
table(malaria_dataset$Diagnosis)

# Convert other categorical variables (if any) to numeric
# Example: Convert Residence to numeric using one-hot encoding
malaria_dataset <- malaria_dataset %>%
  mutate(across(where(is.character), as.factor)) %>%  # Convert character columns to factors
  mutate(across(where(is.factor), as.numeric))    

# Convert the target variable to a factor (if not already)
malaria_dataset$Diagnosis <- as.factor(malaria_dataset$Diagnosis)

# Separate features (X) and target (y)
X <- malaria_dataset[, !names(malaria_dataset) %in% "Diagnosis"] # Features (all columns except Diagnosis)
y <- malaria_dataset$Diagnosis               # Target variable (Diagnosis)

str(X)

X <- X %>% mutate(across(everything(), as.numeric))
X <- X[, sapply(X, is.numeric)]

sapply(X, class)

# Apply SMOTE to balance the dataset
smote_result <- SMOTE(X = X, target = y, K = 5, dup_size = 0)

# Extract the balanced dataset
balanced_data <- smote_result$data

# Rename the target column to match the original dataset
colnames(balanced_data)[ncol(balanced_data)] <- "Diagnosis"

# Check the class distribution after SMOTE
table(balanced_data$Diagnosis)

# Class Distribution Before Balancing
ggplot(malaria_dataset, aes(x = as.factor(Diagnosis), fill = as.factor(Diagnosis))) +
  geom_bar() +
  labs(title = "Class Distribution Before Balancing", x = "Diagnosis", y = "Count") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "pink"), name = "Diagnosis") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  
        axis.line = element_line(color = "black")) 

# Class Distribution After Balancing
ggplot(balanced_data, aes(x = as.factor(Diagnosis), fill = as.factor(Diagnosis))) +
  geom_bar() +
  labs(title = "Class Distribution After Balancing", x = "Diagnosis", y = "Count") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "pink"), name = "Diagnosis") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove grid lines
        axis.line = element_line(color = "black"))  # Add axis lines

# 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌
# ******************************DATA SPLITTING *********************

# Set seed for reproducibility
set.seed(123)

# Step 1: Split the data into training (70%) and temporary (30%) sets
train_index <- createDataPartition(balanced_data$Diagnosis, p = 0.7, list = FALSE)
train_data <- balanced_data[train_index, ]
temp_data <- balanced_data[-train_index, ]

# Step 2: Split the temporary data into evaluation (20%) and testing (10%) sets
eval_index <- createDataPartition(temp_data$Diagnosis, p = 2/3, list = FALSE)  # 2/3 of 30% = 20%
eval_data <- temp_data[eval_index, ]
test_data <- temp_data[-eval_index, ]

# Check the class distribution in each split
cat("Training set class distribution:\n")
table(train_data$Diagnosis)

cat("\nEvaluation set class distribution:\n")
table(eval_data$Diagnosis)

cat("\nTesting set class distribution:\n")
table(test_data$Diagnosis)

# Check the size of each split
cat("\nTraining set size:", nrow(train_data))
cat("\nEvaluation set size:", nrow(eval_data))
cat("\nTesting set size:", nrow(test_data))

# 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌
# ********************** Model Development & Hyperparameter Tuning ********

train_data$Diagnosis <- as.factor(train_data$Diagnosis)
eval_data$Diagnosis <- as.factor(eval_data$Diagnosis)
test_data$Diagnosis <- as.factor(test_data$Diagnosis)

# 1.  Logistic Regression (LR) - LASSO Regularization
# Train Logistic Regression with LASSO
set.seed(123)
lr_model <- train(
  Diagnosis ~ ., 
  data = train_data, 
  method = "glmnet", 
  trControl = trainControl(method = "cv", number = 5, savePredictions = TRUE),
  tuneGrid = expand.grid(alpha = 1, lambda = seq(0.001, 0.1, length = 10))
)

# 2.  Random Forest (RF) - Grid Search
# Train Random Forest with Grid Search
set.seed(123)
rf_model <- train(
  Diagnosis ~ ., 
  data = train_data, 
  method = "rf",
  tuneGrid = expand.grid(.mtry = c(1,2,3,4)),
  trControl = trainControl(method = "cv", number = 5, savePredictions = TRUE),
  tuneLength = 10,
  metric = "Accuracy"
)

# 3.  Decision Tree (DT) - Cross-Validation Based Pruning
# Train Decision Tree with Cross-Validation Based Pruning
set.seed(123)
dt_model <- train(
  Diagnosis ~ ., 
  data = train_data, 
  method = "rpart", 
  trControl = trainControl(method = "cv", number = 5, savePredictions = TRUE),
  tuneLength = 10
)

# 4.  k-Nearest Neighbors (KNN) - Random Search
# Train KNN with Random Search
set.seed(123)
knn_model <- train(
  Diagnosis ~ ., 
  data = train_data, 
  method = "knn", 
  trControl = trainControl(method = "cv", number = 5, savePredictions = TRUE),
  tuneLength = 10
)



# 5.  Gradient Boosting (GB) - Bayesian Optimization
# Train Gradient Boosting with Bayesian Optimization
set.seed(123)
gb_model <- train(
  Diagnosis ~ ., 
  data = train_data, 
  method = "gbm",
  trControl = trainControl(method = "cv", number = 10, savePredictions = TRUE),
  verbose = FALSE
)

#  6.  Naïve Bayes (NB) - Cross-Validation Based Tuning
# Train Naïve Bayes with Cross-Validation Based Tuning
set.seed(123)
nb_model <- train(
  Diagnosis ~ ., 
  data = train_data, 
  method = "nb", 
  trControl = trainControl(method = "cv", number = 5, savePredictions = TRUE),
  tuneLength = 10
)

# 7.  XGBoost - Grid Search
# Train XGBoost with Grid Search
set.seed(123)
xgb_model <- train(
  Diagnosis ~ ., 
  data = train_data, 
  method = "xgbTree", 
  trControl = trainControl(method = "cv", number = 5, savePredictions = TRUE),
  tuneLength = 10,
  objective = "binary:logistic"
)


