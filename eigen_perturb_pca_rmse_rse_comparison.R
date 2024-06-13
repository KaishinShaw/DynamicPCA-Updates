target <- c(1000:1)
eigen_method <- pca_eigen_result$values
perturbation_pca_method <- pca_result$values

## 计算RMSE
rmse_eigen <- sqrt(mean((eigen_method - target)^2))
rmse_perturbation <- sqrt(mean((perturbation_pca_method - target)^2))

## 计算RSE
rse_eigen <- sum((eigen_method - target)^2) / sum((target - mean(target))^2)
rse_perturbation <- sum((perturbation_pca_method - target)^2) / sum((target - mean(target))^2)

library(ggplot2)

df <- data.frame(
    Method = c("Eigen", "Perturbation"),
    RMSE = c(rmse_eigen, rmse_perturbation),
    RSE = c(rse_eigen, rse_perturbation)
)

ggplot(df, aes(x = Method)) +
    geom_col(aes(y = RMSE), fill = "#67223F", width = 0.4, position = position_nudge(x = -0.2)) +
    geom_col(aes(y = RSE * 10000), fill = "#FFC5C7", width = 0.4, position = position_nudge(x = 0.2)) +
    scale_y_continuous(
        name = "RMSE",
        sec.axis = sec_axis(~ . / 10000, name = "RSE"),
        limits = c(0, max(df$RMSE, df$RSE * 10000))  # Adjust this line
    ) +
    theme_minimal() +
    theme(
        axis.title.y.right = element_text(color = "darkgreen"),
        axis.text.y.right = element_text(color = "darkgreen"),
        axis.title.y.left = element_text(color = "darkblue"),
        axis.text.y.left = element_text(color = "darkblue")
    ) +
    labs(x = "Method", y = "RMSE", title = "RMSE and RSE Comparison")