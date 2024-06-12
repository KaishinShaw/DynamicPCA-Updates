updateCovariance <- function(prior_covariance, new_data, num_prior_obs, prior_mean, 
                             forgetting_factor, store_byrow = TRUE) {
    stopifnot(xor(missing(num_prior_obs), missing(forgetting_factor)),
              is.matrix(prior_covariance),
              nrow(prior_covariance) == ncol(prior_covariance),
              (missing(prior_mean) || length(prior_mean) == nrow(prior_covariance)),
              (missing(forgetting_factor) || (forgetting_factor > 0 && forgetting_factor <= 1)))
    
    if (!is.matrix(new_data)) {
        new_data <- as.matrix(new_data)
        store_byrow <- FALSE
    }
    
    new_nrow <- nrow(new_data)
    new_ncol <- ncol(new_data)
    
    stopifnot(nrow(prior_covariance) == ifelse(store_byrow, new_ncol, new_nrow))
    
    new_mean <- colMeans(new_data, na.rm = TRUE)
    
    if (missing(forgetting_factor)) {
        forgetting_factor <- 1 / (num_prior_obs + new_nrow - 1)
    }
    
    factor <- forgetting_factor / (1 + forgetting_factor)
    factor_new <- new_nrow * factor
    
    if (missing(prior_mean)) {
        updated_mean <- factor_new * new_mean
        mean_diff <- updated_mean
    } else {
        updated_mean <- (1 - factor_new) * prior_mean + factor_new * new_mean
        mean_diff <- updated_mean - prior_mean
    }
    
    mean_mat <- matrix(updated_mean, new_nrow, new_ncol, byrow = store_byrow)
    data_diff <- new_data - mean_mat
    
    if (new_nrow == 1L) {
        return((1 - forgetting_factor) * prior_covariance + forgetting_factor * tcrossprod(mean_diff))
    }
    
    crossprod_term <- if (store_byrow) crossprod(data_diff) else tcrossprod(data_diff)
    
    updated_covariance <- (1 - forgetting_factor) * prior_covariance + 
        forgetting_factor * tcrossprod(mean_diff) +
        forgetting_factor * crossprod_term
    
    return(updated_covariance)
}