# NONLINEAR EMBEDDING: t-SNE #################################################

# t-Distributed Stochastic Neighbor Embedding
library(Rtsne)

# Compute t-SNE
set.seed(123)  # For reproducibility
tsne_out <- Rtsne(df_tbm, dims = 2, perplexity = 30, verbose = TRUE)

# Create a data frame from t-SNE results
tsne_result <- data.frame(PC1 = tsne_out$Y[, 1], PC2 = tsne_out$Y[, 2])

# Merge with original data for labeling
tsne_result <- cbind(tsne_result, data_orig[c("SoilLabel", "Ring")])

# plot
ggplot(tsne_result %>%
         # Only select labeled data points
         filter(SoilLabel != 'Unlabeled')) +
  geom_point(aes(x = PC1, y = PC2, color = SoilLabel)) +
  labs(title = "t-SNE Visualization", x = "t-SNE Component 1", y = "t-SNE Component 2") +
  theme_bw()

# NONLINEAR EMBEDDING: UMAP #################################################

# Uniform Manifold Approximation and Pojection
library(umap)

# Compute UMAP
umap_out <- umap(df_tbm, n_neighbors = 30, n_components = 2, metric = "euclidean")

# Create a data frame from UMAP results
umap_result <- data.frame(PC1 = umap_out$layout[, 1], PC2 = umap_out$layout[, 2])

# Merge with original data for labeling
umap_result <- cbind(umap_result, data_orig[c("SoilLabel", "Ring")])

# plot
ggplot(umap_result %>%
         # Only select labeled data points
         filter(SoilLabel != 'Unlabeled')) +
  geom_point(aes(x = PC1, y = PC2, color = SoilLabel)) +
  labs(title = "UMAP Visualization", x = "UMAP Component 1", y = "UMAP Component 2") +
  theme_bw()

