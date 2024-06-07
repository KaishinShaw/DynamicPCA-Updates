library(survival)
library(ggplot2)
library(pca3d)
library(rgl)

head(gbsg)

num_vars <- c("age", "size", "nodes", "er", "rfstime")
data <- na.omit(gbsg[, num_vars])

data.pca <- prcomp(data, scale = TRUE)

write.table(data.pca$rotation, file = "PC.tsv", quote = F, sep = "\t", row.names = TRUE)
write.table(predict(data.pca), file = "newTab.tsv", quote = F, sep = "\t", row.names = TRUE)

pca.sum <- summary(data.pca)
write.table(pca.sum$importance, file = "importance.tsv", quote = F, sep = "\t", row.names = TRUE)

pdf(file = "pcaBarplot.pdf", width = 8, height = 6)
barplot(pca.sum$importance[2, ] * 100, 
        xlab = "Principal Component", 
        ylab = "Proportion of Variance Explained (%)",
        col = "skyblue",
        main = "Scree Plot")
dev.off()

pdf(file = "pcaPlot.pdf", width = 8, height = 6)
plot(pca.sum$importance[2, ] * 100, 
     type = "o", 
     col = "red", 
     xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained (%)",
     main = "Scree Plot")
dev.off()

# 提取 PCA 前两个主成分
pcaPredict <- predict(data.pca)[, 1:2]
PCA <- data.frame(pcaPredict, group = factor(gbsg$grade))
colnames(PCA) <- c("PC1", "PC2", "group")

PCA.mean <- aggregate(PCA[, 1:2], list(group = PCA$group), mean)

# 定义置信椭圆函数
veganCovEllipse <- function(cov, center = c(0, 0), scale = 1, npoints = 100) {
    theta <- (0:npoints) * 2 * pi / npoints
    Circle <- cbind(cos(theta), sin(theta))
    t(center + scale * t(Circle %*% chol(cov)))
}

# 计算每个组的置信椭圆
df_ell <- do.call(rbind, lapply(levels(PCA$group), function(g) {
    cbind(as.data.frame(with(
        PCA[PCA$group == g, ],
        veganCovEllipse(
            cov.wt(cbind(PC1, PC2), wt = rep(1 / length(PC1), length(PC1)))$cov,
            center = c(mean(PC1), mean(PC2))
        )
    )), group = g)
}))

pdf(file = "PCA2d.pdf", width = 8, height = 6)
ggplot(data = PCA, aes(PC1, PC2)) +
    geom_point(aes(color = group), size = 2, alpha = 0.7) +
    geom_path(data = df_ell, aes(x = PC1, y = PC2, color = group), size = 1, linetype = 2) +
    geom_text(data = PCA.mean, aes(label = group), size = 5) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs(title = "PCA Biplot", x = "PC1", y = "PC2", color = "group")
dev.off()


pcaPredict3d <- predict(data.pca)[, 1:3]
pca3d(data.pca, components = 1:3, group = factor(gbsg$grade), show.centroids = TRUE, show.group.labels = TRUE)
rgl.snapshot("pca3d.png", fmt = "png")