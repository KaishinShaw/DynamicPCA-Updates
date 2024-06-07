secular_pca <- function(eigenvalues, eigenvectors, data, num_observations, forgetting_factor, data_center, tolerance = 1e-4, reorthogonalize = FALSE) {
  if (missing(forgetting_factor)) {
    forgetting_factor <- 1 / num_observations
  } else if (forgetting_factor <= 0 || forgetting_factor >= 1) {
    stop("Argument 'forgetting_factor' must be in (0,1)")
  }

  if (!missing(data_center)) {
    data <- data - data_center
  }

  num_dimensions <- length(eigenvalues)
  if (num_dimensions != ncol(eigenvectors)) {
    stop("Arguments 'eigenvalues' and 'eigenvectors' of incompatible dimensions")
  }

  eigenvalues <- (1 - forgetting_factor) * eigenvalues
  transformed_data <- sqrt((1 + forgetting_factor) * forgetting_factor) * eigenvectors %*% data

  sorted_indices <- order(eigenvalues)
  eigenvectors <- eigenvectors[, sorted_indices]
  eigenvalues <- eigenvalues[sorted_indices]
  transformed_data <- transformed_data[sorted_indices]

  epsilon <- max(tolerance, .Machine$double.eps)
  active_indices <- seq_len(num_dimensions)

  zero_indices <- which(abs(transformed_data) < epsilon)
  if (length(zero_indices)) {
    active_indices <- setdiff(active_indices, zero_indices)
  }

  equal_indices <- which(diff(eigenvalues[active_indices]) < epsilon)
  if (length(equal_indices)) {
    ixequal <- active_indices[sort(unique(c(equal_indices, equal_indices + 1L)))]
    dequal <- eigenvalues[ixequal]
    u <- unique(dequal)
    num_mult <- length(u)
    breaks <- c(-Inf, (u[-num_mult] + u[-1]) / 2, Inf)
    group <- split(ixequal, findInterval(dequal, breaks))

    for (i in seq_along(group)) {
      g <- group[[i]]
      mult <- length(g)
      Q1 <- eigenvectors[, g]
      z1 <- transformed_data[g]
      sigma <- sqrt(sum(z1^2))
      v <- c(sigma + z1[1], z1[-1])
      v <- v / sqrt(sum(v^2))
      H <- diag(mult) - 2 * tcrossprod(v)
      eigenvectors[, g] <- Q1 %*% H
      transformed_data[g] <- c(-sigma, rep(0, mult - 1))
      active_indices <- setdiff(active_indices, g[-1])
    }
  }

  num_active <- length(active_indices)
  active_eigenvalues <- eigenvalues[active_indices]
  active_transformed_data_squared <- transformed_data[active_indices]^2
  bounds <- c(active_eigenvalues, active_eigenvalues[num_active] + sum(active_transformed_data_squared))
  amp <- diff(bounds)
  f <- function(lambda) sum(active_transformed_data_squared / (active_eigenvalues - lambda))

  solver <- function(i, max_iter = 1000) {
    delta <- amp[i] / 100
    lb <- bounds[i]
    ub <- bounds[i + 1]
    flb <- f(lb)
    fub <- f(ub)

    iter <- 0
    while (flb * fub > 0 && ub - lb > tolerance && iter < max_iter) {
      delta <- delta / 10
      lb <- max(lb + delta, bounds[i])
      ub <- min(ub - delta, bounds[i + 1])
      flb <- f(lb)
      fub <- f(ub)
      iter <- iter + 1
    }

    # 检查是否找到有效的异号区间
    if (flb * fub < 0) {
      # 如果有异号，使用uniroot寻找精确根
      return(uniroot(f, c(lb, ub), f.lower = flb, f.upper = fub, tol = tolerance)$root)
    } else {
      # 如果没有异号，输出错误信息并停止程序
      stop(paste("Failed to find a valid root in the interval [", toString(lb), ", ", toString(ub), "]. Function values are", toString(flb), "and", toString(fub), "respectively."))
    }
  }

  roots <- sapply(seq_len(num_active), solver)
  eigenvalues[active_indices] <- roots
  if (reorthogonalize) {
    num <- roots - active_eigenvalues
    den <- active_eigenvalues - rep(active_eigenvalues, each = num_active)
    den[den == 0] <- 1
    ztilde <- sqrt(colSums(num / den))
    eigvecs <- (ztilde / (active_eigenvalues - roots)) * eigenvectors[, active_indices]
    norms <- sqrt(colSums(eigvecs^2))
    eigvecs <- eigvecs / rep(norms, each = num_observations)
  } else {
    eigvecs <- (transformed_data[active_indices] / (active_eigenvalues - roots)) * eigenvectors[, active_indices]
    norms <- sqrt(colSums(eigvecs^2))
    eigvecs <- eigvecs / rep(norms, each = num_observations)
  }

  eigenvectors[, active_indices] <- eigvecs
  return(list(values = eigenvalues[num_dimensions:1], vectors = eigenvectors[, num_dimensions:1]))
}