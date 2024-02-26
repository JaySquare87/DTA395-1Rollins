# Load necessary libraries
library(tidyverse)
library(caret)
library(pls)
library(ISLR2)
library(corrplot)

# Load the synthetic dataset
data <- Hitters
data |> head()

data <- na.omit(data)
data |> is.na() |> sum()

# Relocate Salary to be first column
data <- data |> relocate(Salary, .before = 1)

# Remove non numeric columns
data <- data |> select(where(is.numeric))

# Scale the data in the dataframe
data <- as.data.frame(scale(data))

# Calculate correlation between variables except Salary
cor(data[, 2:ncol(data)]) |> round(2)

# Plot correlation matrix
cor(data[, 2:ncol(data)]) |> round(2) |> corrplot()

# Split the data into training and testing sets correctly
set.seed(123)  # For reproducibility
training_rows <- createDataPartition(data$Salary, p = 0.8, list = FALSE)
train_data <- data[training_rows, ]
test_data <- data[-training_rows, ]

# Fit a linear regression model
lm_model <- lm(Salary ~ ., data=train_data)

# Predict on the test set with correct variable names
predictions_lm <- predict(lm_model, newdata=test_data)

# Calculate RMSE for Linear Regression correctly
rmse_lm <- sqrt(mean((test_data$Y - predictions_lm)^2))
cat("Linear Regression RMSE:", rmse_lm, "\n")

# Perform Principal Component Analysis
pca_model <- prcomp(data[, -1], scale=TRUE)
plot(pca_model, type='l')

# Fit a PCR model with correct training data
pcr_model <- pcr(Salary ~ ., data=train_data, scale. =TRUE, validation="CV")

# Predict on the test set
predictions_pcr <- predict(pcr_model, newdata=test_data, ncomp=10)

# Calculate RMSE for PCR correctly
rmse_pcr <- sqrt(mean((test_data$Y - predictions_pcr)^2))
cat("PCR RMSE:", rmse_pcr, "\n")

# Doing it manually

# Scale the data
data_scaled <- scale(data)

# Compute the covariance matrix
cov_matrix <- cov(data_scaled)

# Compute the eigenvalues and eigenvectors
eigen <- eigen(cov_matrix)
eigen_values <- eigen$values
eigen_vectors <- eigen$vectors

# Sort the eigenvalues and eigenvectors
eigen_values_order <- order(eigen_values, decreasing = TRUE)
eigen_values <- eigen_values[eigen_values_order]
eigen_vectors <- eigen_vectors[, eigen_values_order]

# Compute the principal components
pca_manual <- data_scaled %*% eigen_vectors
