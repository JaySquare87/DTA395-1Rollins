## Support Vector Machines

# Load the e1071 package
library(e1071)

# Set Seed
set.seed(1)
x <- matrix(rnorm(200 * 2), ncol = 2)
x[1:100, ] <- x[1:100, ] + 2
x[101:150, ] <- x[101:150, ] - 2
y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame(x = x, y = as.factor(y))

# Plot
plot(x, col=y)

# Train Test Split
train <- sample(200, 100)
svmfit <- svm(y ~ ., data=dat[train, ], kernel="radial", gamma=1, cost=1)

# Plot
plot(svmfit, dat[train, ])

# Summary
summary(svmfit)

# Redo with a larger cost
svmfit <- svm(y ~ ., data=dat[train, ], kernel="radial", gamma=1, cost=1e5)

# Plot
plot(svmfit, dat[train, ])

# Tune the model
set.seed(1)
tune.out <- tune(svm, y ~ ., data=dat[train, ], 
    kernel="radial", 
    ranges=list(cost=c(0.1, 1, 10, 100, 1000), 
                gamma=c(0.5, 1, 2, 3, 4)))

# Summary of the tuning
summary(tune.out)

# Create a table of the best parameters
table(
    true = dat[-train, "y"],
    pred = predict(
        tune.out$best.model, 
        newdata=dat[-train, ]
    )
)

# Calculate the accuracy of the model
mean(
    dat[-train, "y"] == predict(
        tune.out$best.model, 
        newdata=dat[-train, ]
    )
)

# ROC Curves

# Load the ROCR package
library(ROCR)
    
# Create a function to plot the ROC curve
rocplot <- function(pred, truth, ...){
    predob <- prediction(pred, truth)
    perf <- performance(predob, "tpr", "fpr")
    plot(perf, ...)
}
# The function rocplot takes two arguments: pred and truth. These arguments represent the predicted values and the true values, respectively.
# Inside the function, the prediction function is called with pred and truth as arguments. This function is part of the ROCR package in R and is used to create a prediction object.
# The performance function is then called with the predob object, "tpr" (true positive rate), and "fpr" (false positive rate) as arguments. This function calculates the true positive rate and false positive rate based on the prediction object.
# Finally, the plot function is called with the perf object and any additional arguments passed to the rocplot function. This function plots the performance of the prediction object, typically showing the true positive rate on the y-axis and the false positive rate on the x-axis.

# Create a SVM model with the best parameters
svmfit.opt <- svm(y ~ ., data=dat[train, ], kernel="radial", gamma=0.5, cost=1)

# Obtain the fitted values
fitted <- attributes(
    predict(svmfit.opt, dat[train, ], decision.values=TRUE)
    )$decision.values
# attributes(): This function in R is used to get or set attributes of an object. In this case, it's used to access the attributes of the object returned by the predict function.

# Plot the ROC curve
rocplot(-fitted, dat[train, "y"], main="Training Data")

# Increase the gamma parameter
svmfit.opt <- svm(y ~ ., data=dat[train, ], kernel="radial", gamma=50, cost=1)

# Obtain the fitted values
fitted <- attributes(
    predict(svmfit.opt, dat[train, ], decision.values=TRUE)
    )$decision.values

# Plot the ROC curve
rocplot(-fitted, dat[train, "y"], main="Training Data")

# Plot the svmfit.opt model
plot(svmfit.opt, dat[train, ])

# SVM with Multiple Classes

# Set Seed
set.seed(1)
x <- rbind(x, matrix(rnorm(50 * 2), ncol = 2))
y <- c(y, rep(0, 50))
x[y == 0, 2] <- x[y == 0, 2] + 2
dat <- data.frame(x = x, y = as.factor(y))

# Plot
plot(x, col=(y + 1))

# Fit the model
svmfit <- svm(y ~ ., data=dat, kernel="radial", gamma=1, cost=10)

# Plot
plot(svmfit, dat)
