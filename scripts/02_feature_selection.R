source("scripts/00_libraries.R")

# ****************** FEATURE SELECTION **************************

# Convert categorical variables to numeric
malaria_dataset <- malaria_dataset %>%
  mutate(Gender = as.numeric(as.factor(Gender)),  
         Residence = as.numeric(as.factor(Residence)),  
         `Travel.History` = as.numeric(as.factor(`Travel.History`)),  
         Diagnosis = as.numeric(as.factor(Diagnosis)) - 1)  # Ensure binary coding (0,1)

# Select independent variables for VIF analysis (exclude Patient_Id, Test Date, and Health Facility)
independent_vars <- malaria_dataset %>%
  select(Age, Gender, Residence, Fever, Chills, Headache, Diarrhea, `Abdominal.Pain`, `Travel.History`)

# Fit a multiple logistic regression model
vif_model <- glm(Diagnosis ~ ., data = independent_vars, family = binomial)

# Compute VIF values
vif_values <- vif(vif_model)

# Convert VIF results into a clean table
vif_results <- data.frame(Variable = names(vif_values), VIF = round(vif_values, 2)) %>%
  arrange(desc(VIF))  # Sort by highest VIF

# Create table for image output
table_plot <- tableGrob(vif_results)

# Save table as a high-resolution image
png("VIF_Table.png", width = 900, height = 500, res = 150)  # Adjust size and resolution
grid.draw(table_plot)
dev.off()

# ***************** DATA ENCODING ***************************************

# Convert categorical variables to factors (important for model building)
malaria_dataset <- malaria_dataset %>%
  mutate(across(c(Health.Facility, Gender, Residence, Travel.History, Diagnosis), as.factor)) # Includes Diagnosis

# Convert numerical data if needed
malaria_dataset$Age <- as.numeric(malaria_dataset$Age)
# if the diagnosis column is not numerical, let's turn it numerical with 1 and 0, for our algorithms to work
malaria_dataset$Diagnosis <- ifelse(malaria_dataset$Diagnosis == "Positive", 1, 0)

# 2. Gender: Transform to numerical (e.g., F=0, M=1)
malaria_dataset$Gender <- ifelse(malaria_dataset$Gender == "F", 0, 1) # Assuming F is female, M is male

# 4. Fever, Chills, Headache, Diarrhea, Abdominal Pain, Travel History: Transform to numerical (Yes/No to 1/0)
malaria_dataset$Fever <- ifelse(malaria_dataset$Fever == "Yes", 1, 0)
malaria_dataset$Chills <- ifelse(malaria_dataset$Chills == "Yes", 1, 0)
malaria_dataset$Headache <- ifelse(malaria_dataset$Headache == "Yes", 1, 0)
malaria_dataset$Diarrhea <- ifelse(malaria_dataset$Diarrhea == "Yes", 1, 0)
malaria_dataset$Abdominal.Pain <- ifelse(malaria_dataset$Abdominal.Pain == "Yes", 1, 0)
malaria_dataset$Travel.History <- ifelse(malaria_dataset$Travel.History == "Yes", 1, 0)

# 3. Residence: Transform to numerical - One-Hot Encoding (Similar to Health.Facility)
# You have two values "Gutu" and "Gweru"
malaria_dataset$Residence.Gutu <- ifelse(malaria_dataset$Residence == "Gutu", 1, 0)
malaria_dataset$Residence.Gweru <- ifelse(malaria_dataset$Residence == "Gweru", 1, 0)

# 1. Health Facility: Transform to numerical - One-Hot Encoding
# You have two values "Gutu Mission Hospital" and "Gweru Provincial Hospital"
# Create new columns for each hospital, encoding as 0 or 1.
malaria_dataset$Health.Facility.Gutu <- ifelse(malaria_dataset$Health.Facility == "Gutu Mission Hospital", 1, 0)
malaria_dataset$Health.Facility.Gweru <- ifelse(malaria_dataset$Health.Facility == "Gweru Provincial Hospital", 1, 0)


# ************************ Feature Selection 
# Recursive Feature Elimination (RFE) - for Feature Selection

# Prepare predictors (Convert categorical variables to numeric)
predictors <- malaria_dataset %>%
  select(Age, Gender, Residence, Fever, Chills, Headache, Diarrhea, `Abdominal.Pain`, `Travel.History`) %>%
  mutate(across(everything(), as.numeric)) %>%
  drop_na()  # Remove any missing values

# Convert Diagnosis (Outcome) to a factor
outcome <- as.factor(malaria_dataset$Diagnosis)

# Set up RFE control with Random Forest
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)  # 10-fold cross-validation

# Run Recursive Feature Elimination (RFE)
set.seed(123)
rfe_results <- rfe(x = predictors, y = outcome, sizes = seq(1, ncol(predictors)), rfeControl = control)

# Print RFE-selected features
print(rfe_results)

# Plot feature importance
plot(rfe_results, type = c("g", "o"))

