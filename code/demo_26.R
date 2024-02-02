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

# RANDOM FOREST CLASSIFICATION MODEL #################################

library(ranger)
library(caret)

# Create a random forest classification model using ranger
model_rf <- ranger(SoilLabel ~ ThrustForce + 
                     CutterTorque +
                     FoamVol +
                     PolymerVol, 
                   data = df_train)

# Make predictions using the model
pred_rf <- predict(model_rf, data = df_test)$predictions

# Convert predicted probabilities to class labels
# pred_class <- ifelse(pred_rf == levels(df_tbm$SoilLabel)[2], levels(df_tbm$SoilLabel)[2], levels(df_tbm$SoilLabel)[1])

# Evaluate the model
confusion_matrix <- table(pred_rf, df_test$SoilLabel)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", accuracy, "\n")


# TUNED RANDOM FOREST CLASSIFICATION MODEL #####################

# train control
ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = FALSE,
                     search = 'grid')
# Create a grid of hyperparameters to search over
param_grid <- expand.grid(
  mtry = seq(1, 4, 1),
  splitrule = c("extratrees"),
  min.node.size = c(1, 3, 5, 10)
)

# Perform 5-fold cross-validation to tune hyperparameters
set.seed(123)

# initialize list
cv_results_list <- NULL

for (numtrees in c(1, 5, 10, 50, seq(100, 500, 100))) {
  print(system.time(
    # training
    tuned_model <- train(SoilLabel ~ ThrustForce + 
                           CutterTorque +
                           FoamVol +
                           PolymerVol, 
                         data = df_train,
                         method = 'ranger',
                         trControl = ctrl,
                         tuneGrid = param_grid,
                         num.trees = numtrees,
                         verbose = FALSE)
  ))
  
  # save results in a data frame
  cv_results <- data.frame(tuned_model$results, 
                           ntrees = rep(numtrees, 
                                        each = nrow(tuned_model$results)))
  
  # append
  cv_results_list <- rbind(cv_results_list, cv_results)
  
  # computation progress
  print(paste("numtrees = ", numtrees, "done"))
  
}


ggplot(data = cv_results_list %>% filter(min.node.size == 10),
       aes(x = ntrees, y = Accuracy, group = mtry)) +
  geom_line(aes(color = factor(mtry))) +
  geom_point(aes(color = factor(mtry))) +
  labs(x = "Ntrees", y = "OOB classification accuracy") +
  theme_bw()

ggplot(data = cv_results_list %>% filter(ntrees == 200),
       aes(x = min.node.size, y = Accuracy, group = mtry)) +
  geom_line(aes(color = factor(mtry))) +
  geom_point(aes(color = factor(mtry))) +
  labs(x = "Min Node Size", y = "OOB classification accuracy") +
  theme_bw()


