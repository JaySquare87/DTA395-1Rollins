# Load the tidyverse package
library(tidyverse)

# Load the ISLR2 library
library(ISLR2)

# set the seed to 1
set.seed(1)

# Load the Auto data
data(Auto)

# Check the number of rows in the Auto data
nrow(Auto)
N <- nrow(Auto)

# Create a random sample of a specific percentage of the total data for training
training_percentage = 0.05
train <- sample(N, floor(training_percentage*N))

# fit the model using linear regression with mpg as the response and horsepower as the predictor
# Use the train subset
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)

# Check accuracy of the model
MSE <- mean((Auto$mpg - predict(lm.fit, Auto))[-train]^2)
#MSE <- mean((Auto$mpg - predict(lm.fit, Auto))^2)

# Calculate RSE
RSE <- sqrt(MSE)

# Calculate accuracy
accuracy <- 1 - RSE / mean(Auto$mpg)

# Plot mpg vs horsepower
plot(Auto$horsepower, Auto$mpg, xlab = "Horsepower", ylab = "MPG")

# Create a function with a loop to calculate the accuracy of the model
# for different training percentages as input
accuracy_function <- function(training_percentage) {
  N <- nrow(Auto)
  accuracies <- c()
  for (i in 1:1000) {
    train <- sample(N, floor(training_percentage*N))
    lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
    if(training_percentage == 1) {
      MSE <- mean((Auto$mpg - predict(lm.fit, Auto))^2)
    } else {
      MSE <- mean((Auto$mpg - predict(lm.fit, Auto))[-train]^2)
    }
    RSE <- sqrt((MSE * N) / (N - 2))
    accuracy <- 1 - RSE / mean(Auto$mpg)
    accuracies <- c(accuracies, accuracy)
  }
  return(accuracies)
}

test1 <- accuracy_function(0.5)
test2 <- accuracy_function(0.7)
test3 <- accuracy_function(1)

tests <- c(rep("test1", 1000), rep("test2", 1000), rep("test3", 1000))
df <- data.frame(tests = tests, accuracy = c(test1, test2, test3))

# density plot of the accuracy for different training percentages
ggplot(df, aes(x = accuracy, fill = tests)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density plot of accuracy for different training percentages",
       x = "Accuracy",
       y = "Density") +
  theme_minimal()

var(test1)
var(test2)
var(test3)

mean(test1)
mean(test2)
mean(test3)

# Repeat the same test with horsepower, weight as predictors

training_percentage = 0.05
train <- sample(N, floor(training_percentage*N))

# fit the model using linear regression with mpg as the response and horsepower as the predictor
# Use the train subset
lm.fit <- lm(mpg ~ horsepower + weight, data = Auto, subset = train)

# Check accuracy of the model
MSE <- mean((Auto$mpg - predict(lm.fit, Auto))[-train]^2)
#MSE <- mean((Auto$mpg - predict(lm.fit, Auto))^2)

# Calculate RSE
RSE <- sqrt(MSE)

# Calculate accuracy
accuracy <- 1 - RSE / mean(Auto$mpg)

# Load the iris dataset
data(iris)

# get subset of iris data with only virginica and versicolor species
iris_sub <- iris[iris$Species != "setosa",]

# Convert the Species column to a binary variable
iris_sub$Species <- ifelse(iris_sub$Species == "virginica", 1, 0)

# Check the number of rows in the iris_sub data
nrow(iris_sub)
N <- nrow(iris_sub)

# Create a random sample of a specific percentage of the total data for training
training_percentage = 0.5
train <- sample(N, floor(training_percentage*N))

# Fit the model using logistic regression with Species as the response and Sepal.Length as the predictors
# Use the train subset
glm.fit <- glm(Species ~ Sepal.Length, data = iris_sub, family = binomial, subset = train)

# Check accuracy of the model
accuracy <- mean((iris_sub$Species == ifelse(predict(glm.fit, iris_sub, type = "response") > 0.5, 1, 0))[-train])

# for loop for accuracy
training_percentage = 0.5
accuracies0.5 <- c()
for (i in 1:1000) {
  train <- sample(N, floor(training_percentage*N))
  glm.fit <- glm(Species ~ Sepal.Length, data = iris_sub, family = binomial, subset = train)
  accuracies0.5 <- c(accuracies, mean((iris_sub$Species == ifelse(predict(glm.fit, iris_sub, type = "response") > 0.5, 1, 0))[-train]))
}

# create a function that takes in the training percentage and returns the accuracy
get_accuracy <- function(training_percentage) {
  accuracies <- c()
  for (i in 1:1000) {
    train <- sample(N, floor(training_percentage*N))
    glm.fit <- glm(Species ~ Sepal.Length, data = iris_sub, family = binomial, subset = train)
    if(training_percentage == 1) {
      accuracies <- c(accuracies, mean((iris_sub$Species == ifelse(predict(glm.fit, iris_sub, type = "response") > 0.5, 1, 0))))
    } else {
      accuracies <- c(accuracies, mean((iris_sub$Species == ifelse(predict(glm.fit, iris_sub, type = "response") > 0.5, 1, 0))[-train]))
    }
  }
  return(accuracies)
}

test1 <- get_accuracy(0.5)
test2 <- get_accuracy(0.7)
test3 <- get_accuracy(1)

tests <- c(rep("test1", 1000), rep("test2", 1000), rep("test3", 1000))

# combine the accuracies into a dataframe
accuracies <- data.frame(test= tests, accuracy = c(test1, test2, test3))

# Plot histogram of accuracies
ggplot(accuracies, aes(x=accuracy, fill=test)) + 
  geom_density(alpha=0.5) +
  labs(title="Histogram of Accuracies", x="Accuracy", y="Frequency") + 
  theme_minimal()

var(test1)
var(test2)
var(test3)

mean(test3)

# Leave One Out Cross Validation
glm.fit <- glm(mpg ~ horsepower, data = Auto)
coef(glm.fit)

library(boot)
glm.fit <- glm(mpg ~ horsepower, data = Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta

cv.error <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ horsepower, data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error

glm.fit <- glm(mpg ~ horsepower, data = Auto, family = gaussian)


# k-fold
set.seed(17)
cv.error.10 <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error.10


# Bootstrap
boot.fn <- function(data, index) {
  return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
}
boot.fn(Auto, 1:392)
boot.fn(Auto, sample(392, 392, replace = T))
