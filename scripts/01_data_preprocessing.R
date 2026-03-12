source("scripts/00_libraries.R")

#  EXPLORATORY DATA ANALYSIS (EDA)
# Define age groups
malaria_dataset <- malaria_dataset %>%
  mutate(Age.Group = case_when(
    Age >= 0 & Age <= 5   ~ "0-5",
    Age > 5 & Age <= 15   ~ "6-15",
    Age > 15 & Age <= 30  ~ "16-30",
    Age > 30 & Age <= 45  ~ "31-45",
    Age > 45 & Age <= 60  ~ "46-60",
    Age > 60              ~ "61+"
  ))

# View updated dataset with Age Groups
head(malaria_dataset)

# Calculating the Mean and Standard Deviation
mean_age <- mean(malaria_dataset$Age)
sd_age <- sd(malaria_dataset$Age)

cat("Mean Age:", mean_age, "\n")
cat("Standard Deviation:", sd_age, "\n")


# Function to calculate frequency and percentage by diagnosis
calculate_freq_percentage <- function(data, variable) {
  data %>%
    group_by(Diagnosis, !!sym(variable)) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    group_by(Diagnosis) %>%
    mutate(Percentage = Count / sum(Count) * 100) %>%
    ungroup()
}

# List of variables to analyze
variables <- c("Gender", "Residence", "Fever", "Chills", "Headache", "Diarrhea", "Abdominal.Pain", "Travel.History", "Age.Group")

# Calculate frequency and percentage for each variable by diagnosis
results <- list()
for (var in variables) {
  results[[var]] <- calculate_freq_percentage(malaria_dataset, var)
}

# Print results
for (var in variables) {
  cat("\nVariable:", var, "\n")
  print(results[[var]])
}

# Function to calculate chi-squared p-value for a variable
calculate_chi_squared <- function(data, variable) {
  # Create a contingency table
  contingency_table <- table(data[[variable]], data$Diagnosis)
  
  # Perform chi-squared test
  chi_test <- chisq.test(contingency_table)
  
  # Return the p-value
  return(chi_test$p.value)
}

# Calculate chi-squared p-values for each variable
chi_squared_results <- sapply(variables, function(var) {
  calculate_chi_squared(malaria_dataset, var)
})

# Create a data frame to store the results
chi_squared_df <- data.frame(
  Variable = variables,
  Chi_Squared_p_Value = chi_squared_results
)

# Print the results
print(chi_squared_df)

# *****************CHECKING FOR OUTLIERS **************************************

# Compute Z-score per group
malaria_dataset <- malaria_dataset %>%
  group_by(Diagnosis) %>%
  mutate(Z_score = (Age - mean(Age, na.rm = TRUE)) / sd(Age, na.rm = TRUE))

# Identify outliers (|Z| > 3) per group
outliers_by_diagnosis <- malaria_dataset %>%
  filter(abs(Z_score) > 3)

# Summary statistics
summary_table <- malaria_dataset %>%
  group_by(Diagnosis) %>%
  summarise(
    Count = n(),
    Mean_Age = round(mean(Age, na.rm = TRUE), 2),
    SD_Age = round(sd(Age, na.rm = TRUE), 2),
    Outliers = sum(abs(Z_score) > 3, na.rm = TRUE),
    Outlier_Percentage = round((Outliers / Count) * 100, 2)
  )

# View summary table
print(summary_table)


