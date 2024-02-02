# SETUP ##########################################################

# set path
library(rstudioapi)
# set working directory
setwd(dirname(getActiveDocumentContext()$path))

library(tidyverse) # data toolbox
library(ggplot2) # plotting

# LOAD & EXPLORE DATA #############################################

# load data
data_orig <- read_csv('../data/df_tbm.csv')

library("PerformanceAnalytics")
chart.Correlation(data_orig[, c('ThrustForce', 'CutterTorque',
                                'AdvanceSpeed', 'FoamVol')], 
                  histogram=TRUE)

# longitudinal plot of soil label
ggplot(data_orig) + 
  geom_col(aes(x = Ring, y = 1, fill = SoilLabel)) +
  theme_bw()

# plot thrust force, cutter torque, and the soil labels
ggplot(data_orig) + 
  geom_point(aes(x = ThrustForce, y = CutterTorque, color = SoilLabel)) +
  theme_bw()

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

# LOGISTIC CLASSIFICATION MODEL #################################

# Create a logistic regression classification model
model_lr <- glm(SoilLabel ~ ThrustForce + CutterTorque, 
                data = df_train, 
                family = "binomial")

# Make predictions using the model
pred_lr <- predict(model_lr, newdata = df_test, type = "response")
pred_class <- ifelse(pred_lr > 0.5, 
                     levels(df_tbm$SoilLabel)[2], 
                     levels(df_tbm$SoilLabel)[1])

# Evaluate the model
confusion_matrix <- table(pred_class, df_test$SoilLabel)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", accuracy, "\n")

# Visualize the scatterplot with decision boundary
ggplot(df_test, aes(x = ThrustForce, y = CutterTorque, color = SoilLabel)) +
  geom_point() +
  geom_abline(intercept = -coef(model_lr)[1] / coef(model_lr)[3], 
              slope = -coef(model_lr)[2] / coef(model_lr)[3], 
              color = "blue", linetype = "dashed") +
  labs(x = "Thrust Force", y = "Cutter Torque",
       title = paste("Classification Accuracy =", round(accuracy, 4))) +
  theme_bw()

# Visualize all data
ggplot(df_tbm, aes(x = ThrustForce, y = CutterTorque, color = SoilLabel)) +
  geom_point() +
  geom_abline(intercept = -coef(model_lr)[1] / coef(model_lr)[3], 
              slope = -coef(model_lr)[2] / coef(model_lr)[3], 
              color = "blue", linetype = "dashed") +
  labs(x = "Thrust Force", y = "Cutter Torque",
       title = paste("Classification Accuracy =", round(accuracy, 4))) +
  theme_bw()


