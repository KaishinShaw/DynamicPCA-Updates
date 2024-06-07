perturbation_pca <- function(eigenvalues, eigenvectors, data, num_observations, forgetting_factor, data_center) {
  if (missing(forgetting_factor)) {
    forgetting_factor <- 1 / num_observations
  } else if (forgetting_factor <= 0 || forgetting_factor >= 1) {
    stop("Argument 'forgetting_factor' must be in (0,1)")
  }

  num_dimensions <- length(data)

  if (length(eigenvalues) != num_dimensions || ncol(eigenvectors) != num_dimensions) {
    stop("Dimensions of 'eigenvalues', 'eigenvectors', and 'data' are incompatible")
  }

  if (!missing(data_center)) {
    data <- data - data_center
  }

  scaled_eigenvalues <- (1 - forgetting_factor) * eigenvalues
  scaled_eigenvalues_squared <- scaled_eigenvalues^2

  scaled_transformed_data <- sqrt((1 + forgetting_factor) * forgetting_factor) * crossprod(eigenvectors, data)
  scaled_transformed_data_squared <- scaled_transformed_data^2

  numerator <- tcrossprod(scaled_transformed_data)
  denominator_part1 <- scaled_eigenvalues + scaled_transformed_data_squared
  denominator_part2 <- scaled_transformed_data_squared + scaled_eigenvalues_squared
  denominator <- matrix(denominator_part1, num_dimensions, num_dimensions, byrow = TRUE) - matrix(denominator_part2, num_dimensions, num_dimensions)

  rotation_matrix <- numerator / denominator
  diag(rotation_matrix) <- 1

  eigenvectors <- eigenvectors %*% rotation_matrix

  squared_norms <- colSums(eigenvectors^2)
  eigenvectors <- eigenvectors / rep(sqrt(squared_norms), each = num_dimensions)

  scaled_eigenvalues <- (scaled_eigenvalues + scaled_transformed_data_squared) * squared_norms

  list(values = scaled_eigenvalues, vectors = eigenvectors)
}