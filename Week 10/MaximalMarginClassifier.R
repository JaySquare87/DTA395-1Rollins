# Install the e1071 package
install.packages("e1071")
library(e1071)

# Load necessary package
library(ggplot2)

# Generate synthetic data
set.seed(123) # For reproducibility
x1 <- rnorm(20, mean=2)
y1 <- rnorm(20, mean=2)
class1 <- rep(1, 20) # Class 1

x2 <- rnorm(20, mean=4)
y2 <- rnorm(20, mean=4)
class2 <- rep(-1, 20) # Class 2

# Combine the data
data <- data.frame(x = c(x1, x2), y = c(y1, y2), class = factor(c(class1, class2)))

# Plot the data
ggplot(data, aes(x=x, y=y, color=class)) + geom_point() + ggtitle("Synthetic Data for Maximal Margin Classifier")

# Train the Maximal Margin Classifier using a linear SVM
model <- svm(class ~ ., data = data, type = 'C-classification', kernel = 'linear', scale = FALSE)

# Extract the model information for plotting
model_info <- model$SV
support_vectors <- data[model$index,]

# Create a grid
grid <- expand.grid(x=seq(min(data$x), max(data$x), length.out = 200), 
                    y=seq(min(data$y), max(data$y), length.out = 200))

# Predict grid points
grid$predict <- predict(model, newdata = grid)

# Convert predict into numeric
grid$predict <- as.numeric(as.character(grid$predict))

# Plot the data and the decision boundary
ggplot() + 
  geom_point(data = data, aes(x=x, y=y, color=class)) + 
  geom_point(data=support_vectors, aes(x=x, y=y, color = class), shape=3, size=2, stroke=1) + # Highlight support vectors
  stat_contour(data = grid, aes(x = x, y = y, z = predict), binwidth = 1, color = 'black', alpha=0.3) + 
  ggtitle("Maximal Margin Classifier with Support Vectors")
