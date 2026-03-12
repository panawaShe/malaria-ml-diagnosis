source("scripts/00_libraries.R")

# 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌
# ********************** MODEL EVALUATION *********************************
# Example evaluation predicted probabilities 
lr_pred_prob <- predict(lr_model, newdata = eval_data, type = "prob")
rf_pred_prob <- predict(rf_model, newdata = eval_data, type = "prob")
dt_pred_prob <- predict(dt_model, newdata = eval_data, type = "prob")
knn_pred_prob <- predict(knn_model, newdata = eval_data, type = "prob")
gb_pred_prob <- predict(gb_model, newdata = eval_data, type = "prob")
xgb_pred_prob <- predict(xgb_model, newdata = eval_data, type = "prob")

# Ensure we are working with numerical values for the metrics calculations
evaluate_model <- function(model, data) {
  preds <- predict(model, newdata = data, type = "prob")[, 2]  # Get probability of the positive class
  actuals <- as.numeric(as.factor(data$Diagnosis)) - 1  # Convert factors to numeric (0 and 1)
  return(list(predictions = preds, actuals = actuals))
}

# Evaluate each model using your custom evaluate_model function
lr_eval <- evaluate_model(lr_model, eval_data)
rf_eval <- evaluate_model(rf_model, eval_data)
dt_eval <- evaluate_model(dt_model, eval_data)
knn_eval <- evaluate_model(knn_model, eval_data)
gb_eval <- evaluate_model(gb_model, eval_data)
nb_eval <- evaluate_model(nb_model, eval_data)
xgb_eval <- evaluate_model(xgb_model, eval_data)

# Function to calculate evaluation metrics
calculate_metrics <- function(actuals, preds, threshold = 0.5) {
  # Convert probabilities to predicted classes based on a threshold
  predicted_classes <- ifelse(preds > threshold, 1, 0)
  
  # Confusion matrix
  confusion <- confusionMatrix(as.factor(predicted_classes), as.factor(actuals))
  
  # Extract metrics
  metrics <- list(
    Accuracy = confusion$overall['Accuracy'],
    Precision = confusion$byClass['Precision'],
    Recall = confusion$byClass['Recall'],
    F1_Score = confusion$byClass['F1'],
    AUC = roc(actuals, preds)$auc  # Area Under Curve for ROC
  )
  
  return(metrics)
}

# For Logistic Regression
lr_metrics <- calculate_metrics(lr_eval$actuals, lr_eval$predictions)
print(lr_metrics)

# For Random Forest
rf_metrics <- calculate_metrics(rf_eval$actuals, rf_eval$predictions)
print(rf_metrics)

# For Decision Tree
dt_metrics <- calculate_metrics(dt_eval$actuals, dt_eval$predictions)
print(dt_metrics)

# For KNN
knn_metrics <- calculate_metrics(knn_eval$actuals, knn_eval$predictions)
print(knn_metrics)

# For Gradient Boosting
gb_metrics <- calculate_metrics(gb_eval$actuals, gb_eval$predictions)
print(gb_metrics)

# For Naive Bayes
nb_metrics <- calculate_metrics(nb_eval$actuals, nb_eval$predictions)
print(nb_metrics)

# For XGBoost
xgb_metrics <- calculate_metrics(xgb_eval$actuals, xgb_eval$predictions)
print(xgb_metrics)

# Create a data frame with the model performance metrics
data <- data.frame(
  Model = c("LR", "RF", "DT", "GB", "KNN", "NB", "XGBoost"),
  Accuracy = c(0.7788018, 0.9585253, 0.8940092, 0.9354839, 0.6728111, 0.7465438, 0.9447005),
  Precision = c(0.8076923, 0.9327731, 0.8617886, 0.9224138, 0.6752137, 0.7614679, 0.9310345),
  Recall = c(0.75, 0.9910714, 0.9464286, 0.9553571, 0.7053571, 0.7410714, 0.9642857),
  F1_Score = c(0.7777778, 0.961039, 0.9021277, 0.9385965, 0.6899563, 0.7511312, 0.9473684),
  AUC_ROC = c(0.844, 0.9829, 0.9456, 0.9784, 0.7219, 0.8577, 0.9749)
)

# Reshape the data for plotting
data_melted <- melt(data, id.vars = "Model")

# Generate the bar plot
ggplot(data_melted, aes(x = Model, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.7) +  # Adjust bar width and spacing
  labs(title = "Model Performance Evaluation",
       x = "Model",
       y = "Evaluation Score") +
  scale_fill_manual(values = c("Accuracy" = "blue", 
                               "Precision" = "green", 
                               "Recall" = "orange",
                               "F1_Score" = "purple",
                               "AUC_ROC" = "red")) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +  # Ensure bars start from 0
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.border = element_rect(color = "black", fill = NA),  # Add panel border
        axis.line.x = element_line(color = "black"),  # Restore x-axis line
        legend.title = element_blank())  # Remove legend title

# 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌
# ********************** MODEL TESTING *********************************

# Logistic Regression Predictions
lr_test_probs <- predict(lr_model, newdata = test_data, type = "prob")[, 2]
lr_test_preds <- ifelse(lr_test_probs > 0.5, 1, 0)

# Random Forest Predictions
rf_test_probs <- predict(rf_model, newdata = test_data, type = "prob")[, 2]
rf_test_preds <- ifelse(rf_test_probs > 0.2, 1, 0)

# Decision Tree Predictions
dt_test_probs <- predict(dt_model, newdata = test_data, type = "prob")[, 2]
dt_test_preds <- ifelse(dt_test_probs > 0.5, 1, 0)

# KNN Predictions
knn_test_probs <- predict(knn_model, newdata = test_data, type = "prob")[, 2]
knn_test_preds <- ifelse(knn_test_probs > 0.5, 1, 0)

# Gradient Boosting Predictions
gb_test_probs <- predict(gb_model, newdata = test_data, type = "prob")[, 2]
gb_test_preds <- ifelse(gb_test_probs > 0.5, 1, 0)

# Naive Bayes Predictions
nb_test_probs <- predict(nb_model, newdata = test_data, type = "prob")[, 2]
nb_test_preds <- ifelse(nb_test_probs > 0.5, 1, 0)

# XGBoost Predictions
xgb_test_probs <- predict(xgb_model, newdata = test_data, type = "prob")[, 2]
xgb_test_preds <- ifelse(xgb_test_probs > 0.5, 1, 0)

# 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌
# ********************** MODEL Performance ON TESTING DATASET *********************************

# Replace 'actual_diagnosis_column_name' with the actual name of your target variable in the test_data
test_actuals <- test_data$Diagnosis

# Ensure the actual labels are numeric, converting from factors if necessary
test_actuals <- as.numeric(as.factor(test_data$Diagnosis)) - 1  # Convert to 0 and 1

# Logistic Regression Metrics
lr_test_metrics <- calculate_metrics(test_actuals, lr_test_preds, lr_test_probs)
print("Logistic Regression Test Metrics:")
print(lr_test_metrics)

# Repeat for other models
rf_test_metrics <- calculate_metrics(test_actuals, rf_test_preds, rf_test_probs)
print("Random Forest Test Metrics:")
print(rf_test_metrics)

dt_test_metrics <- calculate_metrics(test_actuals, dt_test_preds, dt_test_probs)
print("Decision Tree Test Metrics:")
print(dt_test_metrics)

knn_test_metrics <- calculate_metrics(test_actuals, knn_test_preds, knn_test_probs)
print("KNN Test Metrics:")
print(knn_test_metrics)

gb_test_metrics <- calculate_metrics(test_actuals, gb_test_preds, gb_test_probs)
print("Gradient Boosting Test Metrics:")
print(gb_test_metrics)

nb_test_metrics <- calculate_metrics(test_actuals, nb_test_preds, nb_test_probs)
print("Naive Bayes Test Metrics:")
print(nb_test_metrics)

xgb_test_metrics <- calculate_metrics(test_actuals, xgb_test_preds, xgb_test_probs)
print("XGBoost Test Metrics:")
print(xgb_test_metrics)

# ***********BAR CHARTS VISUALIZATION FOR TEST DATASET METRICS*********************************

# Prepare data
performance_data <- data.frame(
  Model = c("LR", "RF", "DT", "GB", "KNN", "NB", "XGBoost"),
  Accuracy = c(0.83, 0.94, 0.89, 0.94, 0.69, 0.82, 0.95),
  Precision = c(0.85, 0.91, 0.87, 0.92, 0.68, 0.89, 0.93),
  Recall = c(0.82, 0.99, 0.93, 0.98, 0.79, 0.77, 0.98),
  F1_Score = c(0.84, 0.95, 0.89, 0.95, 0.73, 0.82, 0.96)
)

# Convert to long format
plot_data <- performance_data %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Score") %>%
  mutate(Metric = factor(Metric, levels = c("Accuracy", "Precision", "Recall", "F1_Score")))

# Custom color palette
model_colors <- c(
  "LR" = "#1f77b4",
  "RF" = "#ff7f0e",
  "DT" = "#999999",
  "GB" = "#FFD700",
  "KNN" = "#6baed6",
  "NB" = "#31a354",
  "XGBoost" = "#084594"
)

# Create labels for the facets
labels <- c("a) Accuracy", "b) Precision", "c) Recall", "d) F1 Score")

# Create the bar plot
bar_plot <- ggplot(plot_data, aes(x = Model, y = Score, fill = Model)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.2f", Score)), vjust = -0.5, size = 3.5) +
  facet_wrap(~Metric, ncol = 2, labeller = labeller(Metric = setNames(labels, c("Accuracy", "Precision", "Recall", "F1_Score")))) +
  scale_fill_manual(values = model_colors) +
  labs(
    title = "Model Performance Comparison",
    subtitle = "Bar Plots Showing Performance of Test Data Across Evaluation Metrics",
    x = "Model",
    y = "Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.spacing = unit(1.5, "lines")  # Increase facet spacing
  ) +
  ylim(0, 1.05)

# Print the plot
print(bar_plot)
