# Load the MASS package
library(MASS)

# Load the tidyverse package
library(tidyverse)

# Load the palmerpenguins package
library(palmerpenguins)

# Load the penguins dataset
df <- penguins

# Remove rows with missing data
df <- df %>% drop_na()

# Linear Regression ----

# Plot the relationship between body mass and flipper length
ggplot(df, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Predict mass from flipper length
lm <- lm(body_mass_g ~ flipper_length_mm, data = df)

# Print the summary of the linear model
lm_summary <- summary(lm)

# Calculate the accuracy of the model
1 - lm_summary$sigma / mean(df$body_mass_g)

# Logistic Regression ----

# Convert the sex variable to binary number
df <- df |>
  mutate(sex = ifelse(sex == "male", 0, 1))

# Logistic Regression Model to predict sex from all other columns
logit <- glm(sex ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g, data = df, family = "binomial")

# Print the summary of the logistic model
logit_summary <- summary(logit)

# Predict the sex of the penguin with the following measurements
# bill_length_mm = 40, bill_depth_mm = 18, flipper_length_mm = 200, body_mass_g = 4000
new_data <- data.frame(bill_length_mm = 40, bill_depth_mm = 18, flipper_length_mm = 200, body_mass_g = 4000)
predict(logit, newdata = new_data, type = "response")

# Calculate the accuracy of the model
sex_pred <- predict(logit, type = "response")
y_hat <- ifelse(sex_pred > 0.5, 1, 0)
mean(y_hat == df$sex)

# LDA ----

# We will only take the rows where the Species is either Adelie or Gentoo

df2 <- df %>%
  filter(species %in% c("Adelie", "Gentoo"))

# Convert the species variable to binary number
df2 <- df2 |>
  mutate(species_binary = ifelse(species == "Adelie", 0, 1))

# P = 1

# Classify based on flipper length
# First we need to calculate the prior probabilities
adel_prior <- sum(df2$species_binary == 0) / nrow(df2)
gent_prior <- sum(df2$species_binary == 1) / nrow(df2)


# Use the LDA function to predict sex from body mass
lda <- lda(species_binary ~ flipper_length_mm, data = df2, 
           prior = c(adel_prior, gent_prior))

# Calculate the discriminant threshold
lda_threshold <- (mean(lda$means) - 
                    (log(lda$prior[1] / lda$prior[2])) / lda$scaling)

# Plot a density plot
ggplot(df2) +
  geom_density(aes(x = flipper_length_mm, fill = species), alpha=0.5) +
  geom_vline(xintercept = lda_threshold, color = "red")

# P = 2
# Create an LDA model to predict sex from body mass and flipper length
lda2 <- lda(species_binary ~ body_mass_g + flipper_length_mm, data = df2, 
            prior = c(adel_prior, gent_prior))

# Calculate slope and intercept for the discriminant threshold
lda2.slope <- -lda2$scaling[1] / lda2$scaling[2]
lda2.intercept <- -mean(lda2$means %*% lda2$scaling)
lda2.intercept <- -lda2.intercept / lda2$scaling[2]

# Plot a scatter plot with the discriminant threshold
ggplot(df2) +
  geom_point(aes(x = body_mass_g, y = flipper_length_mm, color = species)) +
  geom_abline(intercept = lda2.intercept, slope = lda2.slope)
  
# QDA ----

# Create a QDA model to predict species from body mass and flipper length

# First calculate prior probabilities for all species
adel_prior <- sum(df$species == "Adelie") / nrow(df)
chin_prior <- sum(df$species == "Chinstrap") / nrow(df)
gent_prior <- sum(df$species == "Gentoo") / nrow(df)

qda <- qda(species ~ body_mass_g + flipper_length_mm, data = df,
           prior = c(adel_prior, chin_prior, gent_prior))

# Create a grid of points
body_mass_min <- min(df$body_mass_g)
body_mass_max <- max(df$body_mass_g)
flipper_length_min <- min(df$flipper_length_mm)
flipper_length_max <- max(df$flipper_length_mm)

grid <- expand.grid(body_mass_g = seq(body_mass_min, body_mass_max, length.out = 100),
                    flipper_length_mm = seq(flipper_length_min, flipper_length_max, length.out = 100))

# Predict the species for each point in the grid
grid$species_pred <- predict(qda, newdata = grid)$class

# Plot the grid with the predicted species
ggplot(df) +
  geom_point(aes(x = body_mass_g, y = flipper_length_mm, color = species)) +
  geom_tile(data = grid, aes(x = body_mass_g, y = flipper_length_mm, color = species_pred), alpha = 0.1)


# Resampling Methods ----

# Create a training and testing set
set.seed(123)

# Create a vector of indices
indices <- sample(1:nrow(df), nrow(df) * 0.7)

# Create the training set
train <- df[indices,]

# Create the testing set
test <- df[-indices,]

# Create a linear regression model to predict body mass from flipper length
lm <- lm(body_mass_g ~ flipper_length_mm, data = train)
summary(lm)

# Predict the test set
test$pred <- predict(lm, newdata = test)

# Calculate the accuracy of the model
1 - sqrt(mean((test$body_mass_g - test$pred)^2)) / mean(test$body_mass_g)

# Cross-Validation ----

# Load the boot library
library(boot)

# Create a logistic regression model
glm.fit <- glm(sex ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g, data = df, family = "binomial")

# Create a 10-fold cross-validation
cv <- cv.glm(df, glm.fit, K = 10)

# Print the results
cv$delta
?cv.glm # Check what delta means in the help

# Calculate 
1 - cv$delta[1]

# Best Subset Selection ----

# Load the leaps library
library(leaps)

# Use the regsubset function to find the best subset of predictors
regfit.full <- regsubsets(body_mass_g ~ ., data = df, nvmax=15)
regfit.full.summary <- summary(regfit.full)

# Adjusted R2
regfit.full.summary$adjr2
which.max(regfit.full.summary$adjr2)

# BIC
regfit.full.summary$bic
which.min(regfit.full.summary$bic)

# Cp
regfit.full.summary$cp
which.min(regfit.full.summary$cp)

# Plot the best subset selection
par(mfrow=c(2,2))
plot(regfit.full.summary$adjr2, xlab="Number of Variables", ylab="Adjusted R^2", type="l")
plot(regfit.full.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
plot(regfit.full.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
