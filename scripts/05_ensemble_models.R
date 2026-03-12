source("scripts/00_libraries.R")

# 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌
# ************************ ENSEMBLE MODEL BUILDING **********************

#1.  BAGGING

# Train a bagging model using the Random Forest algorithm
set.seed(123)
bagging_model <- randomForest(Diagnosis ~ ., data = train_data, mtry = 4, ntree = 500)

#2.  STACKING

# Get predictions from each individual model
pred_lr <- predict(lr_model, newdata = eval_data)
pred_rf <- predict(rf_model, newdata = eval_data)
pred_dt <- predict(dt_model, newdata = eval_data)
pred_knn <- predict(knn_model, newdata = eval_data)
pred_gb <- predict(gb_model, newdata = eval_data)
pred_nb <- predict(nb_model, newdata = eval_data)
pred_xgb <- predict(xgb_model, newdata = eval_data)

# Create a data frame with the predictions
predictions_stacked <- data.frame(
  lr = pred_lr,
  rf = pred_rf,
  dt = pred_dt,
  knn = pred_knn,
  gb = pred_gb,
  nb = pred_nb,
  xgb = pred_xgb,
  Diagnosis = eval_data$Diagnosis
)

# Train a meta-learner, for example, logistic regression on predictions from the base models
set.seed(123)
stacking_model <- train(Diagnosis ~ ., data = predictions_stacked, method = "glm", family = "binomial")
#3. Soft Voting
pred_proba_hard <- (
  rowMeans(as.data.frame(cbind(predict(lr_model, newdata = eval_data, type = "prob")[,2],
                               predict(rf_model, newdata = eval_data, type = "prob")[,2],
                               predict(dt_model, newdata = eval_data, type = "prob")[,2],
                               predict(knn_model, newdata = eval_data, type = "prob")[,2],
                               predict(gb_model, newdata = eval_data, type = "prob")[,2],
                               predict(nb_model, newdata = eval_data, type = "prob")[,2],
                               predict(xgb_model, newdata = eval_data, type = "prob")[,2]
  )))
)

# Create final predictions based on soft voting
final_predictions_soft <- ifelse(pred_proba_hard > 0.5, 1, 0)

# 4. ADABOOST
# Fit an AdaBoost Model
set.seed(123)
adaboost_model <- ada(Diagnosis ~ ., data = train_data, iter = 100)

# Predictions from AdaBoost
ada_preds <- predict(adaboost_model, newdata = eval_data)

# 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌
# ************************ ENSEMBLE MODEL EVALUATION **********************

# Ensure the target variable in eval_data is a factor
eval_data$Diagnosis <- as.factor(eval_data$Diagnosis)

# Modified calculate_metrics function
calculate_metrics <- function(actuals, preds, probs, threshold = 0.5) {
  # Ensure 'actuals' is a factor with specific levels
  actuals <- factor(actuals)  # Convert to factor if not already
  
  # Confusion matrix
  confusion <- confusionMatrix(as.factor(predicted_classes), actuals)
  
  # AUC calculation
  auc_value <- roc(actuals, probs)$auc  # Area Under Curve for ROC
  
  # Extract metrics
  metrics <- list(
    Accuracy = confusion$overall['Accuracy'],
    Precision = confusion$byClass['Precision'],
    Recall = confusion$byClass['Recall'],
    F1_Score = confusion$byClass['F1'],
    AUC = auc_value
  )
  
  return(metrics)
}

# Evaluating Soft Voting Model
soft_voting_metrics <- calculate_metrics(eval_data$Diagnosis, final_predictions_soft, pred_proba_hard)
print("Soft Voting Model Metrics:")
print(soft_voting_metrics)

# Evaluating Bagging Model
bagging_preds <- predict(bagging_model, newdata = eval_data)
bagging_prob <- predict(bagging_model, newdata = eval_data, type = "prob")[, 2]  # Probability for positive class
bagging_metrics <- calculate_metrics(eval_data$Diagnosis, bagging_preds, bagging_prob)
print("Bagging Model Metrics:")
print(bagging_metrics)

# Evaluating Stacking Model
stacking_preds <- predict(stacking_model, newdata = predictions_stacked, type = "raw")
stacking_prob <- predict(stacking_model, newdata = predictions_stacked, type = "prob")[, 2]  # Probability for positive class
stacking_metrics <- calculate_metrics(eval_data$Diagnosis, stacking_preds, stacking_prob)
print("Stacking Model Metrics:")
print(stacking_metrics)

# Evaluate AdaBoost Model
ada_prob <- predict(adaboost_model, newdata = eval_data, type = "prob")[,2] # Get probability for positive class
ada_metrics <- calculate_metrics(eval_data$Diagnosis, ada_preds, ada_prob)
print("AdaBoost Model Metrics:")
print(ada_metrics)

# Create a data frame with the ensemble model performance metrics
data <- data.frame(
  Model = c("Soft Voting", "Bagging", "Stacking", "AdaBoost"),
  Accuracy = c(0.9262673, 0.9493088, 0.9631336, 0.9493088),
  Precision = c(0.9444444, 0.9243697, 0.9482759, 0.9243697),
  Recall = c(0.9107143, 0.9821429, 0.9821429, 0.9821429),
  F1_Score = c(0.9272727, 0.952381, 0.9649123, 0.952381),
  AUC_ROC = c(0.9812, 0.9829, 0.9836, 0.9828)
)

# Reshape the data for plotting
data_melted <- melt(data, id.vars = "Model")

# Generate the bar plot
ggplot(data_melted, aes(x = Model, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +  # Adjust bar width and spacing
  labs(title = "Ensemble Model Performance",
       x = "Model",
       y = "Evaluation Score") +
  scale_fill_manual(values = c("Accuracy" = "blue", 
                               "Precision" = "orange", 
                               "Recall" = "green",
                               "F1_Score" = "purple",
                               "AUC_ROC" = "red")) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +  # Ensure bars touch the x-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add a box around the plot area
        axis.line = element_line(color = "black"),  # Restore both x and y-axis lines
        legend.title = element_blank())  # Remove legend title

# 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌 📌
# ************************ ENSEMBLE MODEL performance ON TESTING DATASET **********************

# Soft Voting Predictions
final_proba_soft_test <- rowMeans(as.data.frame(
  cbind(lr_test_probs, 
        rf_test_probs, 
        dt_test_probs, 
        knn_test_probs, 
        gb_test_probs, 
        nb_test_probs, 
        xgb_test_probs)
))

final_predictions_soft_test <- ifelse(final_proba_soft_test > 0.5, 1, 0)
soft_voting_test_metrics <- calculate_metrics(test_actuals, final_predictions_soft_test, final_proba_soft_test)
print("Soft Voting Model Test Metrics:")
print(soft_voting_test_metrics)

# For Bagging Model
bagging_test_preds <- predict(bagging_model, newdata = test_data)
bagging_prob_test <- predict(bagging_model, newdata = test_data, type = "prob")[, 2]
bagging_test_metrics <- calculate_metrics(test_actuals, bagging_test_preds, bagging_prob_test)
print("Bagging Model Test Metrics:")
print(bagging_test_metrics)

# For AdaBoost Model
ada_test_probs <- predict(adaboost_model, newdata = test_data, type = "prob")[, 2]
ada_test_metrics <- calculate_metrics(test_actuals, ada_preds, ada_test_probs)
print("AdaBoost Model Test Metrics:")
print(ada_test_metrics)
                    
                     # Prepare ensemble performance data
                     ensemble_data <- data.frame(
                       Model = c("Soft Voting", "Bagging", "Stacking", "AdaBoost"),
                       Accuracy = c(0.94, 0.94, 0.96, 0.94),
                       Precision = c(0.93, 0.90, 0.95, 0.90),
                       Recall = c(0.95, 0.98, 0.98, 0.98),
                       F1_Score = c(0.94, 0.94, 0.96, 0.94)
                     )
                     
                     # Convert to long format
                     ensemble_plot_data <- ensemble_data %>%
                       pivot_longer(cols = -Model, names_to = "Metric", values_to = "Score") %>%
                       mutate(Metric = factor(Metric, levels = c("Accuracy", "Precision", "Recall", "F1_Score")))
                     
                     # Custom color palette for ensemble models
                     ensemble_colors <- c(
                       "Soft Voting" = "#1f77b4",
                       "Bagging" = "darkorange",
                       "Stacking" = "brown",
                       "AdaBoost" = "darkgreen"
                     )
                     
                     # Create labels for the facets
                     ensemble_labels <- c("a) Accuracy", "b) Precision", "c) Recall", "d) F1 Score")
                     
                     # Create the bar plot
                     ensemble_bar_plot <- ggplot(ensemble_plot_data, aes(x = Model, y = Score, fill = Model)) +
                       geom_col(width = 0.7, alpha = 0.9) +
                       geom_text(aes(label = sprintf("%.2f", Score)), vjust = -0.5, size = 3.5) +
                       facet_wrap(~Metric, ncol = 2, 
                                  labeller = labeller(Metric = setNames(ensemble_labels, 
                                                                        c("Accuracy", "Precision", "Recall", "F1_Score")))) +
                       scale_fill_manual(values = ensemble_colors) +
                       labs(
                         title = "Ensemble Model Performance Comparison",
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
                     print(ensemble_bar_plot)
