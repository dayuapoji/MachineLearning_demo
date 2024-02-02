# KMEANS ###########################################################################

# Input hyperparameters
num_clusters <- 6  # Number of clusters

# Perform K-means Clustering
kmeans_result <- kmeans(pc_result %>% 
                          # Select only labeled data points
                          filter(SoilLabel != 'Unlabeled') %>% 
                          # Select only PC1 and PC2
                          .[, c('PC1', 'PC2')], 
                        centers = num_clusters)

# Visualize K-means Clusters
ggplot(pc_result %>% 
         # Only select labeled data points
         filter(SoilLabel != 'Unlabeled')) +
  geom_point(aes(x = PC1, y = PC2, color = as.factor(kmeans_result$cluster))) +
  labs(title = paste("K-means Clustering: Number of Clusters =", num_clusters), 
       color = "Clusters",
       x = "Principal Component 1", y = "Principal Component 2") +
  theme_bw()


# DBSCAN ###########################################################################
library(dbscan)

# input hyperparameters, more borders with smaller reachability
eps <- 0.4 
minpts <- 3 


# Perform DBSCAN Clustering
dbscan_result <- dbscan(pc_result %>% 
                          # select only labeled data points
                          filter(SoilLabel != 'Unlabeled') %>% 
                          # select only PC1 and PC2
                          .[ , c('PC1', 'PC2')], 
                        # set hyperparameters
                        eps = eps, minPts = minpts)

# Visualize results
ggplot(pc_result %>% 
         # only select labeled data points
         filter(SoilLabel != 'Unlabeled')) +
  geom_point(aes(x = PC1, y = PC2, color = as.factor(dbscan_result$cluster))) +
  labs(title = paste("DBSCAN Clustering: Eps =", eps, "MinPts =", minpts), 
       color = "Clusters",
       x = "Principal Component 1", y = "Principal Component 2") +
  theme_bw()


