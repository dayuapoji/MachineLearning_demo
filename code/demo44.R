# PREDICTION WITH COVARIATES ##############################


# SPLIT DATA ##############################################
# Set a random seed for reproducibility
set.seed(123)

# Generate random indices for train and test split
train_indices <- sample(seq_len(nrow(df)), size = 0.8 * nrow(df), 
                        replace = FALSE)

# Split the dataframe into train and test
df_train <- df[train_indices, c('Easting', 'Northing', 
                                'DryDensity', 'InplaceMoisture')]
df_test <- df[-train_indices, c('Easting', 'Northing', 
                                'DryDensity', 'InplaceMoisture')]


# convert data to spatial df
dfsp_train <- SpatialPointsDataFrame(coords = cbind(df_train$Easting, 
                                                    df_train$Northing),
                                     data = df_train, 
                                     proj4string = CRS(projargs = "+init=epsg:32631"))

dfsp_test <- SpatialPointsDataFrame(coords = cbind(df_test$Easting, 
                                                   df_test$Northing),
                                    data = df_test, 
                                    proj4string = CRS(projargs = "+init=epsg:32631"))


head(dfsp_train@coords)
head(dfsp_test@coords)

# remove duplicate locations
dfsp_train <- dfsp_train[-zerodist(dfsp_train)[,1],]
dfsp_test <- dfsp_test[-zerodist(dfsp_test)[,1],]

# KRIGING #################################################

# create a variogram model
variogram_model <- gstat::variogram(DryDensity ~ InplaceMoisture, 
                                    data = dfsp_train)
plot(variogram_model)

# fit variogram
fit_var <- gstat::fit.variogram(object = variogram_model,
                                model = vgm(nugget = 4, 
                                            psill = 6, 
                                            range = 20,
                                            model = 'Gau'))
plot(variogram_model, model = fit_var)

# fit kriging model
test_krig <- gstat::krige(formula = DryDensity ~ InplaceMoisture, 
                          locations = dfsp_train,
                          newdata = dfsp_test, 
                          model = fit_var)


head(test_krig@data)

# store estimation in df 
df_pred_krig <- data.frame(Easting = dfsp_test@data$Easting,
                           Northing = dfsp_test@data$Northing,
                           DryDensity = dfsp_test@data$DryDensity,
                           InplaceMoisture = dfsp_test@data$InplaceMoisture,
                           Prediction = test_krig@data$var1.pred)
df_pred_krig$Error <- abs(df_pred_krig$DryDensity - df_pred_krig$Prediction)

# plot
fig_krigcov_diag <- ggplot(df_pred_krig) +
  geom_point(aes(x = DryDensity, y = Prediction)) +
  geom_abline(intercept = 0, slope = 1, 
              color = "blue", 
              linetype = "dashed") +
  labs(title = paste('Kriging Estimation with Covariates',
                     '\nRMSE =', round(RMSE(df_pred_krig$DryDensity,
                                            df_pred_krig$Prediction), 2),
                     '\nR2 =', round(postResample(df_pred_krig$DryDensity,
                                                  df_pred_krig$Prediction)[2], 2)),
       x = "Actual Dry Density",
       y = "Estimated Dry Density") +
  coord_equal() +
  theme_bw()

ggarrange(fig_krig_diag, fig_krigcov_diag, ncol = 2)

# RANDOM FORESTS ##########################################

library(ranger)

model_rf <- ranger(DryDensity ~., data = df_train,
                   mtry = 3,
                   num.trees = 100)

# Add kriging predictions and prediction errors to the test data

df_pred_rf <- df_test %>%
  mutate(Prediction = predict(model_rf, data = df_test)$predictions) %>%
  mutate(Error =  DryDensity - Prediction)

# Plot actual vs. kriging predictions
fig_rf_diag <- ggplot(df_pred_rf) +
  geom_point(aes(x = DryDensity, y = Prediction)) +
  geom_abline(intercept = 0, slope = 1, 
              color = "blue", 
              linetype = "dashed") +
  labs(title = paste('Random Forest',
                     '\nRMSE =', round(RMSE(df_pred_rf$DryDensity,
                                            df_pred_rf$Prediction), 2),
                     '\nR2 =', round(postResample(df_pred_rf$DryDensity,
                                                  df_pred_rf$Prediction)[2], 2)),
       x = "Actual Dry Density",
       y = "Estimated Dry Density") +
  coord_equal() +
  theme_bw()

ggarrange(fig_idw_diag, fig_krig_diag, 
          fig_krigcov_diag, fig_rf_diag, ncol = 2)
