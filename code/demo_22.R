# DATA PREPARATION ##############################################

# only select 2 soil labels (for linear decision boundayr)
df_tbm <- data_orig %>% filter(SoilLabel %in% c('CCS_CSG',
                                                'TLTD_CSGCSF_CCS'))

# Convert SoilLabel to a factor
df_tbm$SoilLabel <- as.factor(df_tbm$SoilLabel)

# plot thrust force, cutter torque, and the soil labels
ggplot(df_tbm) + 
  geom_point(aes(x = ThrustForce, y = CutterTorque, color = SoilLabel)) +
  theme_bw()

# DATA SPLITTING ################################################

# Split the data into training and testing sets (70:30)
set.seed(123)  # For reproducibility
n <- nrow(df_tbm)
train_index <- sample(n, n * 0.7)
df_train <- df_tbm[train_index, ]
df_test <- df_tbm[-train_index, ]

# TREE CLASSIFICATION MODEL #################################

library(rpart)

# Create a decision tree classification model
model_tree <- rpart(SoilLabel ~ ThrustForce + CutterTorque, data = df_train)

# Make predictions using the model
pred_tree <- predict(model_tree, newdata = df_test, type = "class")

# Evaluate the model
confusion_matrix <- table(pred_tree, df_test$SoilLabel)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", accuracy, "\n")

# Generate a grid of points for decision boundary visualization
x_range <- range(df_tbm$ThrustForce)
y_range <- range(df_tbm$CutterTorque)
x_grid <- seq(x_range[1], x_range[2], length.out = 100)
y_grid <- seq(y_range[1], y_range[2], length.out = 100)
grid <- expand.grid(ThrustForce = x_grid, CutterTorque = y_grid)

# Make predictions for the grid
grid$SoilLabel <- predict(model_tree, newdata = grid, type = "class")

# Create a scatterplot of the original data with decision boundary
ggplot() +
  geom_raster(data = grid, 
              aes(x = ThrustForce, y = CutterTorque, fill = SoilLabel),
              alpha = 0.25) +
  geom_point(data = df_test, 
             aes(x = ThrustForce, y = CutterTorque, color = SoilLabel),
             size = 2) +
  labs(x = "Thrust Force", y = "Cutter Torque",
       title = paste("Classification Accuracy =", round(accuracy, 4))) +
  theme_bw()

# Visualize all data
ggplot() +
  geom_raster(data = grid, 
              aes(x = ThrustForce, y = CutterTorque, fill = SoilLabel),
              alpha = 0.25) +
  geom_point(data = df_tbm, 
             aes(x = ThrustForce, y = CutterTorque, color = SoilLabel),
             size = 2) +
  labs(x = "Thrust Force", y = "Cutter Torque",
       title = paste("Classification Accuracy =", round(accuracy, 4))) +
  theme_bw()



