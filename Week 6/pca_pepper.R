library(readr)
library(corrplot)
pepper <- read_delim("https://raw.githubusercontent.com/JaySquare87/DTA395-1Rollins/main/Week%206/pepper.csv", 
                     delim = "\t", escape_double = FALSE, 
                     trim_ws = TRUE)

# Calculate correlation between variables
cor(pepper[, 2:ncol(pepper)]) |> round(2)

# Plot correlation matrix
cor(pepper[, 2:ncol(pepper)]) |> round(2) |> corrplot::corrplot()

pepper <- pepper[, c("Size", "Color", "Hotness")]

pca_result <- prcomp(pepper, scale. = TRUE)
summary(pca_result)

# Plot the PCA result
plot(pca_result)
plot(pca_result$x[,1], pca_result$x[,2], xlab = "PC1", ylab = "PC2", pch = 19)

# Doing it manually

# Scale the data
pepper_scaled <- scale(pepper)

# Compute the covariance matrix
cov_matrix <- cov(pepper_scaled)

# Compute the eigenvalues and eigenvectors
eigen <- eigen(cov_matrix)
eigen_values <- eigen$values
eigen_vectors <- eigen$vectors

# Sort the eigenvalues and eigenvectors
eigen_values_order <- order(eigen_values, decreasing = TRUE)
eigen_values <- eigen_values[eigen_values_order]
eigen_vectors <- eigen_vectors[, eigen_values_order]

# Compute the principal components
pca_manual <- pepper_scaled %*% eigen_vectors



