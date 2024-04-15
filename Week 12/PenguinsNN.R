# Step 1: Load Necessary Libraries
library(keras3)
library(palmerpenguins)

# Step 2: Preprocess the Data
data("penguins")
penguins <- na.omit(penguins)

# Encoding species as a numeric factor
penguins$species <- as.numeric(factor(penguins$species)) - 1

# Splitting dataset into features and labels
features <- as.matrix(penguins[, c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")])
labels <- to_categorical(penguins$species)

# Normalizing features
mean <- apply(features, 2, mean)
std <- apply(features, 2, sd)
features <- scale(features, center = mean, scale = std)

# Splitting into training and testing sets
set.seed(123)
indices <- sample(1:nrow(features), size = 0.8 * nrow(features))
x_train <- features[indices,]
y_train <- labels[indices,]
x_test <- features[-indices,]
y_test <- labels[-indices,]

# Step 3: Construct the Neural Network
model <- keras_model_sequential(input_shape = 4) |>
  layer_dense(units = 8, activation = 'relu') |>
  # This is a small dataset and network, you don't need a dropout layer.
  layer_dense(units = 3, activation = 'softmax', input_shape = ncol(x_train))

model |> compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adam(learning_rate = 1e-2), # adam is another optimizer that works well in practice
  metrics = 'accuracy'
)

# Step 4: Train the Model
model |> fit(x_train, y_train, epochs = 25, batch_size = 5, validation_split = 0.2)

# Step 5: Evaluate the Model
model |> evaluate(x_test, y_test)
