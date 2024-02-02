# DATA PREPARATION ##############################################

# remove unlabeled data
df_tbm <- data_orig %>% filter(SoilLabel != 'Unlabeled')

# Convert SoilLabel to a factor
df_tbm$SoilLabel <- as.factor(df_tbm$SoilLabel)

# Plot thrust force, cutter torque, and the soil labels
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

# SVM CLASSIFICATION MODEL #################################

library(e1071)

# Create an SVM classification model using the e1071 package
model_svm <- svm(SoilLabel ~ ThrustForce + CutterTorque, data = df_train)

# Make predictions using the model
pred_svm <- predict(model_svm, newdata = df_test)

# Evaluate the model
confusion_matrix <- table(pred_svm, df_test$SoilLabel)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", accuracy, "\n")

# Generate a grid of points for decision boundary visualization
x_range <- range(df_tbm$ThrustForce)
y_range <- range(df_tbm$CutterTorque)
x_grid <- seq(x_range[1], x_range[2], length.out = 100)
y_grid <- seq(y_range[1], y_range[2], length.out = 100)
grid <- expand.grid(ThrustForce = x_grid, CutterTorque = y_grid)

# Make predictions for the grid
grid$SoilLabel <- predict(model_svm, newdata = grid)

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
