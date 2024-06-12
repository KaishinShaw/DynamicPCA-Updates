# Step 1: Build data matrix
total_observations <- 2e4
total_features <- 1000
total_components <- 80
training_size <- total_observations - 10
data_matrix <- matrix(runif(total_observations * total_features), nrow = total_observations, ncol = total_features)
data_matrix <- data_matrix * rep(sqrt(12 * (1:total_features)), each = total_observations)

# Step 2: Compute covariance matrix of the initial dataset
initial_cov_matrix <- cov(data_matrix[1:training_size, ])

# Step 3: Timing of covariance updates
update_cov_time <- system.time({
  update_cov_results <- vector("list", length = total_observations - training_size)
  for (i in (training_size + 1):total_observations) {
    current_col_means <- colMeans(data_matrix[1:(i - 1), ])
    update_cov_results[[i - training_size]] <- updateCovariance(initial_cov_matrix, data_matrix[i, ], i - 1, current_col_means)
  }
})

full_cov_time <- system.time({
  full_cov_results <- vector("list", length = total_observations - training_size)
  for (i in (training_size + 1):total_observations) {
    full_cov_results[[i - training_size]] <- cov(data_matrix[1:i, ])
  }
})

# Prepare data for plotting
timing_data <- data.frame(
  Method = rep(c("Full Covariance", "Incremental Update"), each = 2),
  TimeType = factor(c("User", "Elapsed", "User", "Elapsed"), levels = c("User", "Elapsed")),
  Time = c(full_cov_time["user.self"], full_cov_time["elapsed"], update_cov_time["user.self"], update_cov_time["elapsed"])
)

# Plotting the timing data
library(ggplot2)
library(colorspace)

palette_colors <- sequential_hcl(2, palette = "Burg")

ggplot(timing_data, aes(x = Method, y = Time, fill = TimeType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_fill_manual(name = "Time Type", values = palette_colors) +
  labs(title = "Incremental Update vs. Full Covariance", x = "Method", y = "Time (seconds)") +
  theme_minimal()


last_update_cov <- update_cov_results[[length(update_cov_results)]]
last_full_cov <- full_cov_results[[length(full_cov_results)]]
diff_matrix <- last_update_cov - last_full_cov

rmse <- sqrt(mean(diff_matrix^2))
rse <- sqrt(sum(diff_matrix^2) / sum(last_full_cov^2))

cat("RMSE:", rmse, "\n")
cat("RSE:", rse, "\n")