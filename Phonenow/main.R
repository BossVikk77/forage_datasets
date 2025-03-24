# Load necessary libraries
library(tidyverse)
library(caret)

dataset <- read.csv("Churn-Dataset.csv")

# Remove missing values
dataset <- dataset[complete.cases(dataset), ]

# Convert categorical columns to factors
dataset$Churn <- as.factor(dataset$Churn)
dataset$Contract <- as.factor(dataset$Contract)
dataset$PaymentMethod <- as.factor(dataset$PaymentMethod)
dataset$PaperlessBilling <- as.factor(dataset$PaperlessBilling)

# Split data into Training (80%) & Testing (20%)
set.seed(123)
index <- createDataPartition(dataset$Churn, p = 0.8, list = FALSE)
train_data <- dataset[index, ]
test_data <- dataset[-index, ]

# Train logistic regression model
model <- glm(Churn ~ tenure + MonthlyCharges + TotalCharges + Contract + PaymentMethod + PaperlessBilling,
             data = train_data, family = binomial)

# Predict probabilities on the entire dataset
dataset$Churn_Prob <- predict(model, newdata = dataset, type = "response")

# Create a new column "Churn_Risk_Group" based on probability thresholds
dataset$Churn_Risk_Group <- cut(dataset$Churn_Prob, 
                                breaks = c(-Inf, 0.25, 0.50, 0.75, Inf), 
                                labels = c("Low Risk", "Moderate Risk", "High Risk", "Very High Risk"))

# Save the updated dataset as a CSV file
write.csv(dataset, "Churn_Dataset_Updated.csv", row.names = FALSE)
