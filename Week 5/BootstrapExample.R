# Original dataset
scores <- c(78, 85, 96, 80, 69)

# Calculate the original sample mean
original_mean <- mean(scores)

# Generate bootstrap samples and compute their means
bootstrap_means <- replicate(1000, mean(sample(scores, replace = TRUE)))

# Determine the 95% confidence interval from the bootstrap distribution
confidence_interval <- quantile(bootstrap_means, c(0.025, 0.975))

list(original_mean = original_mean, confidence_interval = confidence_interval)

# Plot the bootstrap_means distribution
library(ggplot2)
ggplot(data.frame(bootstrap_means), aes(x = bootstrap_means)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  geom_vline(xintercept = original_mean, color = "red", linewidth = 1) +
  geom_vline(xintercept = confidence_interval, color = "blue", size = 1, linetype = "dashed") +
  labs(title = "Bootstrap Distribution of Sample Means",
       x = "Sample Mean",
       y = "Frequency") +
  theme_minimal()
