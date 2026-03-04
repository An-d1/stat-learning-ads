############################################################
# 04 - UNSUPERVISED LEARNING
############################################################


# LIBRARIES -----------------------------------------------

library(tidyverse)
library(factoextra)
library(cluster)


# LOAD DATA ------------------------------------------------

ads_data <- readRDS("data/cleaned/global_ads_performance_dataset_cleaned.rds")


# FEATURE SELECTION ----------------------------------------

# Select scale and efficiency metrics
data_clust <- ads_data %>%
  select(ad_spend, revenue, conversions, CTR, CPA, ROAS)


# TRANSFORMATIONS ------------------------------------------

# Log-transform skewed volume variables to reduce heavy tails
data_clust <- data_clust %>%
  mutate(
    ad_spend = log1p(ad_spend),
    revenue = log1p(revenue),
    conversions = log1p(conversions)
  )


# PCA -------------------------------------------------------

# Perform PCA with automatic centering and scaling
pca_res <- prcomp(data_clust, scale. = TRUE)

# Proportion of variance explained
pve <- pca_res$sdev^2 / sum(pca_res$sdev^2)

# Cumulative proportion of variance explained
cumulative_pve <- cumsum(pve)
cumulative_pve

# Scree plot
fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 100)) +
  ggtitle("Scree Plot")

# Variable contributions
fviz_pca_var(pca_res, col.var = "contrib", repel = TRUE) +
  ggtitle("Variables Contribution")

# PCA biplot
fviz_pca_biplot(pca_res,
                repel = TRUE,
                col.var = "#2C7BB6",
                col.ind = "grey80",
                alpha.ind = 0.3) +
  theme_minimal()


# RETAIN COMPONENTS ----------------------------------------

# Retain first two principal components
pc_scores <- pca_res$x[, 1:2]


# K-MEANS CLUSTERING ----------------------------------------

# Elbow method
fviz_nbclust(pc_scores, kmeans, method = "wss")

# Silhouette method
fviz_nbclust(pc_scores, kmeans, method = "silhouette")

# Gap statistic
fviz_nbclust(pc_scores, kmeans, method = "gap_stat")

set.seed(123)

# K-means with robust initialization
k4 <- kmeans(pc_scores, centers = 4, nstart = 50)

# Average silhouette width (numerical stability measure)
sil <- silhouette(k4$cluster, dist(pc_scores))
mean(sil[, 3])

# Cluster visualization
fviz_cluster(k4, data = pc_scores)


# CLUSTER PROFILING -----------------------------------------

# Attach cluster labels
ads_data$cluster <- k4$cluster

# Cluster sizes
table(ads_data$cluster)

# Cluster-wise means
cluster_summary <- ads_data %>%
  group_by(cluster) %>%
  summarise(
    ad_spend = mean(ad_spend),
    revenue = mean(revenue),
    conversions = mean(conversions),
    CTR = mean(CTR),
    CPA = mean(CPA),
    ROAS = mean(ROAS),
    .groups = "drop"
  )

cluster_summary

# Platform distribution across clusters
prop.table(table(ads_data$cluster, ads_data$platform), 2)

# Industry distribution across clusters
prop.table(table(ads_data$cluster, ads_data$industry), 2)


# HIERARCHICAL CLUSTERING -----------------------------------

# Standardize variables for hierarchical clustering
data_scaled <- scale(data_clust)

# Euclidean distance matrix
dist_matrix <- dist(data_scaled)

# Complete linkage
hc_complete <- hclust(dist_matrix, method = "complete")

plot(hc_complete)
rect.hclust(hc_complete, k = 4, border = 2:5)


# FINAL REMARKS ---------------------------------------------

# The segmentation highlights two main latent dimensions:
# (i) campaign scale (driven by spend, revenue and conversions)
# (ii) campaign efficiency (driven by ROAS, CPA and CTR).

# PCA confirms that most of the variability is captured by these
# orthogonal dimensions, supporting dimensionality reduction
# before clustering.

# K-means identifies four structurally distinct segments that
# differ in size and efficiency, suggesting meaningful
# heterogeneity across campaigns.

# Hierarchical clustering provides a consistent structure,
# reinforcing the robustness of the segmentation.

# Since no ground truth clustering exists, results should be
# interpreted as exploratory patterns rather than definitive classifications.