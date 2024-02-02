# SETUP ##########################################################

# set path
library(rstudioapi)
# set working directory
setwd(dirname(getActiveDocumentContext()$path))
# SETUP ##########################################################

library(tidyverse) # data toolbox
library(magrittr)
library(ggplot2) # plotting

# LOAD & PREPARE DATA ############################################

# load data
data_orig <- read_csv('../data/df_tbm.csv')

# only select the features (removing ID and label columns)
df_tbm <- data_orig %>% select(-c('Ring', 'SoilLabel'))
head(data_orig)

# PCA USING EIGENDECOMPOSITION ###################################

# Step 1: Standardize data
data_scaled <- scale(df_tbm, center = T, scale = T)

# Step 2: Compute covariance matrix of the data 
cov_matrix <- cov(data_scaled) # [C]

# Step 3: Calculate Eigenvectors and Eigenvalues
eigenvalues <- eigen(cov_matrix)$values
eigenvectors <- eigen(cov_matrix)$vectors

# Step 5: Choose Principal Components
num_components <- 2 # for example, choose the first 2 PCs
pc_selected <- data_scaled %*% eigenvectors[, 1:num_components] %>%
  as.data.frame() %>% # convert to df
  set_colnames(c("PC1", "PC2")) %>% # rename columns
  # include ID and label
  mutate(SoilLabel =  data_orig$SoilLabel,
         Ring = data_orig$Ring)

# Plot
ggplot(pc_selected %>% 
         # only select labeled data points
         filter(SoilLabel != 'Unlabeled')) +
  geom_point(aes(x = PC1, y = PC2, color = SoilLabel)) +
  labs(title = "PCA Using Eigendecomposition", x = "Principal Component 1", y = "Principal Component 2") +
  theme_bw()

# PCA USING PRCOMP FUNCTION (SVD) ###################################

# Compute PCA
pca_out <- prcomp(df_tbm, scale. = T, center = T) 

# Extract PCs
pc_result <- pca_out$x %>% as.data.frame() 
pc_result <- pc_result * -1 # inverse to have the same sign convention
pc_result <- pc_result %>%
  mutate(SoilLabel =  data_orig$SoilLabel,
         Ring = data_orig$Ring)

# Plot
ggplot(pc_result %>% 
         # only select labeled data points
         filter(SoilLabel != 'Unlabeled')) +
  geom_point(aes(x = PC1, y = PC2, color = SoilLabel)) +
  labs(title = "PCA Using SVD", x = "Principal Component 1", y = "Principal Component 2") +
  theme_bw()


# VARIANCE PLOT ################################################

variance = pca_out$sdev^2 / sum(pca_out$sdev^2)

ggplot()+
  geom_col(mapping = aes(x = 1:ncol(pca_out$x), y = variance)) +
  scale_x_continuous(labels = as.character(1:ncol(pca_out$x)), 
                     breaks = c(1:ncol(pca_out$x))) +
  xlab('Principal Components') +
  ylab('Variance') +
  theme_bw()

variance_cumulative = cumsum(pca_out$sdev^2) / sum(pca_out$sdev^2)

ggplot()+
  geom_point(mapping = aes(x = 1:ncol(pca_out$x), y = variance_cumulative)) +
  geom_line(mapping = aes(x = 1:ncol(pca_out$x), y = variance_cumulative)) +
  scale_x_continuous(labels = as.character(1:ncol(pca_out$x)), 
                     breaks = c(1:ncol(pca_out$x))) +
  ylim(0, 1) +
  xlab('Principal Components') +
  ylab('Cumulative Variance') +
  theme_bw()

# BIPLOT #######################################################

library(factoextra) # PCA visualization

fviz_pca_var(pca_out,
             col.var = "contrib", # Color by contributions to the PC
             # gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T) +
  scale_color_gradientn(name = 'Contributions', 
                        colours = c('blue', 'orange', 'red')) +
  xlab('PC1') +
  ylab('PC2') +
  guides(fill=guide_legend(title="Contributions")) +
  ggtitle('Biplot')

