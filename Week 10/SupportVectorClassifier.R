# Load the e1071 library
library(e1071)

# Set seed to 1
set.seed(1)

# Create the x matrix
x <- matrix(rnorm(20 * 2), ncol = 2)
# Create the y matrix
y <- c(rep(-1, 10), rep(1, 10))
# Addin 1 to the x matrix where y is equal to 1 to separate the two classes
x[y == 1, ] <- x[y == 1, ] + 1

# Plot the x matrix
plot(x, col = (3 - y))

# Create a data frame from the matrx
df <- data.frame(x = x, y = as.factor(y))

# Create a support vector classifier
svm.fit <- svm(y ~ ., data = df, kernel = "linear", cost = 10, scale = FALSE)

# Plot the support vector classifier
plot(svm.fit, df)

# Get the index of the support vectors
svm.fit$index

# Get a summary of the support vector classifier
summary(svm.fit)

# Recreate the support vector classifier with a smaller cost
svm.fit <- svm(y ~ ., data = df, kernel = "linear", cost = 0.1, scale = FALSE)

# Plot the support vector classifier
plot(svm.fit, df)

# Get the index of the support vectors
svm.fit$index

# Tune the cost parameter
tune.out <- tune(svm, y ~ ., data = df, kernel = "linear", 
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

# Get a summary of the tuned support vector classifier
summary(tune.out)

# Get the best cost parameter
bestmod <- tune.out$best.model

# Get a summary of the best support vector classifier
summary(bestmod)

# Predict
xtest <- matrix(rnorm(20 * 2), ncol = 2)
ytest <- sample(c(-1, 1), 20, rep = TRUE)
xtest[ytest == 1, ] <- xtest[ytest == 1, ] + 1
df.test <- data.frame(x = xtest, y = as.factor(ytest))

# Predict the test data
ypred <- predict(bestmod, df.test)
table(predict = ypred, truth = df.test$y)

# Do the experiment again with cost 0.01
svm.fit <- svm(y ~ ., data = df, kernel = "linear", cost = 0.01, scale = FALSE)
plot(svm.fit, df)
ypred <- predict(svm.fit, df.test)
table(predict = ypred, truth = df.test$y)

# Let us makes the classes linearly separable
x[y == 1, ] <- x[y == 1, ] + 1

# Plot the x matrix
plot(x, col = (y+5) / 2, pch = 19)

# Create a data frame from the matrx
df <- data.frame(x = x, y = as.factor(y))

# Create a support vector classifier with high cost
svm.fit <- svm(y ~ ., data = df, kernel = "linear", cost = 1e5, scale = FALSE)

# Plot the support vector classifier
plot(svm.fit, df)

# Lower the cost to 1
svm.fit <- svm(y ~ ., data = df, kernel = "linear", cost = 1, scale = FALSE)

# Plot the support vector classifier
plot(svm.fit, df)
