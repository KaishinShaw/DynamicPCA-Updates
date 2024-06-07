library(rARPACK)

spectra_pca <- function(covariance_matrix, num_components) {
  if (missing(num_components)) {
    num_components <- ncol(covariance_matrix)
  }
  if (num_components <= ncol(covariance_matrix) / 10) {
    result <- eigs_sym(covariance_matrix, num_components, lower = TRUE, ncv = ncol(covariance_matrix))
  } else {
    result <- eigen(covariance_matrix, TRUE)
    result$values <- result$values[seq_len(num_components)]
    result$vectors <- result$vectors[, seq_len(num_components)]
  }
  list(values = result$values, vectors = result$vectors)
}