setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("elapsed_timer.R", echo = TRUE, print.eval = TRUE)
source("updateMean.R", echo = TRUE, print.eval = TRUE)
source("spectra_pca.R", echo = TRUE, print.eval = TRUE)
source("spectra_pca_vs_eigen.R", echo = TRUE, print.eval = TRUE)
source("perturbation_pca.R", echo = TRUE, print.eval = TRUE)
source("perturbation_pca_vs_eigen.R", echo = TRUE, print.eval = TRUE)
source("eigen_perturb_pca_rmse_rse_comparison.R", echo = TRUE, print.eval = TRUE)
