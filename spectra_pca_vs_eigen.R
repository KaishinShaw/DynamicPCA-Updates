num_observations <- 2e4
num_features <- 1000
num_components <- 80
data_matrix <- matrix(runif(num_observations * num_features), num_observations, num_features)
data_matrix <- data_matrix * rep(sqrt(12 * (1:num_features)), each = num_observations)
covariance_matrix <- cov(data_matrix)

eigen_decomposition_time <- system.time({
  eigen_decomposition <- eigen(covariance_matrix, TRUE)
  eigen_decomposition$values <- eigen_decomposition$values[1:num_components]
  eigen_decomposition$vectors <- eigen_decomposition$vectors[, 1:num_components]
})

spectra_pca_time <- system.time(spectra_pca_result <- spectra_pca(covariance_matrix, num_components))

library(ggplot2)
library(colorspace)

time_data <- data.frame(
    Method = rep(c("eigen", "spectra_pca"), each = 2),
    TimeType = factor(c("User", "Elapsed", "User", "Elapsed"), levels = c("User", "Elapsed")),
    Time = c(eigen_decomposition_time["user.self"], eigen_decomposition_time["elapsed"], spectra_pca_time["user.self"], spectra_pca_time["elapsed"])
)

palette_colors <- sequential_hcl(2, palette = "Burg")

ggplot(time_data, aes(x = Method, y = Time, fill = TimeType)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    scale_fill_manual(name = "Time Type", values = palette_colors) +
    labs(title = "Comparison of User and Elapsed Time", x = "Method", y = "Time (seconds)") +
    theme_minimal()
## Check equality
all.equal(eigen_decomposition$values, spectra_pca_result$values)
