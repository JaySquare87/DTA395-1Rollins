# This is a code you will use for the linear regression validation analysis of this lab
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

# This is a code you will use for the logistic regression validation analysis of this lab
# create a function that takes in the training percentage and returns the accuracy
get_accuracy <- function(training_percentage) {
  accuracies <- c()
  for (i in 1:1000) {
    train <- sample(N, floor(training_percentage*N))
    glm.fit <- glm(Species ~ Sepal.Length, data = iris_sub, family = binomial, subset = train)
    if(training_percentage == 1) {
      accuracies <- c(accuracies, 
                      mean((iris_sub$Species == ifelse(predict(glm.fit, iris_sub, type = "response") > 0.5, 1, 0))))
    } else {
      accuracies <- c(accuracies, 
                      mean((iris_sub$Species == ifelse(predict(glm.fit, iris_sub, type = "response") > 0.5, 1, 0))[-train]))
    }
  }
  return(accuracies)
}