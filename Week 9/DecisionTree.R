# Load the rpart library
library(rpart)

# Load the rpart.plot library
library(rpart.plot)

# Load vip library
library(vip)

# Load tidymodels
library(tidymodels)

# Load the iris dataset
data(iris)
head(iris)

# Create decision tree model
tree_model <- rpart(Species ~ ., data=iris, method="class")

# Plot the decision tree using rpart.plot
rpart.plot(tree_model, type=4, extra=101, under=TRUE, fallen.leaves=TRUE, cex=0.8, tweak=1.2)

# Print the most important variables
vip(tree_model)

# Create tree model based on Petal.Length and Petal.Width
tree_model <- rpart(Species ~ Petal.Length + Petal.Width, data=iris, method="class")

# Get the rules
rules <- rpart.rules(tree_model)
rules

# Scatter plot of Petal.Width and Petal.Length
# Add lines based on rules
ggplot(iris, aes(x=Petal.Width, y=Petal.Length, color=Species)) +
  geom_point() +
  geom_hline(yintercept = 2.5, color="red") + 
  geom_segment(aes(x=1.8, y=2.5, xend=1.8, yend=7), color="blue")

# The grid approach
# Create a grid based on Petal.Width and Petal.Length
Petal.Width.min <- min(iris$Petal.Width)
Petal.Width.max <- max(iris$Petal.Width)
Petal.Length.min <- min(iris$Petal.Length)
Petal.Length.max <- max(iris$Petal.Length)

grid <- expand.grid(
  Petal.Width = seq(Petal.Width.min, Petal.Width.max, length.out = 100),
  Petal.Length = seq(Petal.Length.min, Petal.Length.max, length.out = 100)
)

# Predict the grid
grid$Species <- predict(tree_model, grid, type="class")

# Scatter plot of Petal.Width and Petal.Length
# Add grid as a geom_tile layer
ggplot() +
  geom_point(data=iris, aes(x=Petal.Width, y=Petal.Length, color=Species)) +
  geom_tile(data=grid, aes(x=Petal.Width, y=Petal.Length, fill=Species), alpha=0.5) +
  labs(title="Scatter plot of Petal.Width and Petal.Length", x="Petal.Width", y="Petal.Length")

# Calculate the accuracy of the model
predictions <- predict(tree_model, iris, type="class")
mean(predictions == iris$Species)

# Using C50
# Install the C50 package
install.packages("C50")

# Load the C50 library
library(C50)

# Create decision tree model
tree_model <- C5.0(Species ~ ., data=iris)

# Plot the decision tree
plot(tree_model)
