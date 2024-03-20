# Load the MASS library
library(MASS)

# Load the Boston Housing dataset
data(Boston)

# Load tidymodels
library(tidymodels)

# Prepare the dataset for ggplot2
boston_data_long <- Boston |>
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value")

# Create a histogram of all numeric variables in one plot
ggplot(boston_data_long, aes(x = value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Histogram of Boston Housing Dataset",
       x = "Value",
       y = "Frequency")

# Split the data into training and testing sets
set.seed(123)

data_split <- initial_split(Boston, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)

# Simple Decision Tree

# Create a decision tree model specification
tree_spec <- decision_tree() |>
  set_engine("rpart") |>
  set_mode("regression")

# Fit the model to the training data
tree_fit <- tree_spec |>
  fit(medv ~ ., data = train_data)

# Evaluating the Decision Tree Performance

# Make prediction on the testing set
predictions <- tree_fit |>
  predict(new_data = test_data) |>
  pull(.pred)

# Calculate RMSE and R-squared
metric <- metric_set(rmse, rsq)
model_performance <- test_data |>
  mutate(predictions = predictions) |>
  metrics(truth = medv, estimate = predictions)

model_performance

# Model accuracy
(1 - model_performance |> filter(.metric == "rmse") |> pull(.estimate) / mean(Boston$medv)) * 100

# Analyzing results

# Predicting outcomes
# Make prediction on new data
new_data <- tribble(
  ~crim, ~zn, ~indus, ~chas, ~nox, ~rm, ~age, ~dis, ~rad, ~tax, ~ptratio, ~black, ~lstat,
  0.00632, 0, 2.31, 0, 0.538, 6.575, 65.2, 4.09, 3, 296, 15.3, 396.9, 2.94
)
predictions <- tree_fit |>
  predict(new_data = new_data)
print(predictions)

# Load rpart.plot
library(rpart.plot)

# Plot the decision tree
rpart.plot(tree_fit$fit, type=4, extra=101, under=T, cex=0.8, box.palette = "auto")

# Extracting the rules from the decision tree
rules <- rpart.rules(tree_fit$fit, roundint = F)
print(rules)

# Finding the important variables
# Load the vip library
library(vip)

# Plot the variable importance
vip(tree_fit$fit, roundint=F)
