num_observations <- 2e4
num_features <- 1000
num_components <- 80
initial_observations <- (num_observations - 10)
data_matrix <- matrix(runif(num_observations * num_features), num_observations, num_features)
data_matrix <- data_matrix * rep(sqrt(12 * (1:num_features)), each = num_observations)

perturbation_pca_time <- system.time({
  feature_means <- colMeans(data_matrix[1:initial_observations, ])
  pca_result <- spectra_pca(cov(data_matrix[1:initial_observations, ]))

  for (i in (initial_observations + 1):num_observations) {
    feature_means <- updateMean(feature_means, data_matrix[i, ], i - 1)
    pca_result <- perturbation_pca(pca_result$values, pca_result$vectors, data_matrix[i, ],
      n = i - 1, data_center = feature_means
    )
  }
})

eigen_time <- system.time(
  for (i in (initial_observations + 1):num_observations) {
    pca_eigen_result <- eigen(cov(data_matrix[1:i, ]), TRUE)
  }
)

library(ggplot2)
library(colorspace)

time_data <- data.frame(
  Method = rep(c("eigen", "perturbation_pca"), each = 2),
  TimeType = factor(c("User", "Elapsed", "User", "Elapsed"), levels = c("User", "Elapsed")),
  Time = c(eigen_time["user.self"], eigen_time["elapsed"], perturbation_pca_time["user.self"], perturbation_pca_time["elapsed"])
)

palette_colors <- sequential_hcl(2, palette = "Burg")

ggplot(time_data, aes(x = Method, y = Time, fill = TimeType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_fill_manual(name = "Time Type", values = palette_colors) +
  labs(title = "perturbation_pca vs. eigen", x = "Method", y = "Time (seconds)") +
  theme_minimal()
