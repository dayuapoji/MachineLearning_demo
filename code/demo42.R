# INVERSE DISTANCE WEIGHTED INTERPOLATION #################

# Load required packages
library(sp)
library(sf)
library(gstat)
library(caret)

# SPLIT DATA ##############################################
# Set a random seed for reproducibility
set.seed(123)

# Generate random indices for train and test split
train_indices <- sample(seq_len(nrow(df)), size = 0.8 * nrow(df), 
                        replace = FALSE)

# Split the dataframe into train and test
df_train <- df[train_indices, c('Easting', 'Northing', 'DryDensity')]
df_test <- df[-train_indices, c('Easting', 'Northing', 'DryDensity')]

# visualize train and test data points
ggplot() +
  geom_point(data = df_train, aes(x = Easting, y = Northing), 
             color = 'blue') +
  geom_point(data = df_test, aes(x = Easting, y = Northing), 
             color = 'red') +
  theme_bw()

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

# SPATIAL ANALYSIS EVALUATION #############################

# compute IDW estimation
test_idw <- gstat::idw(formula = DryDensity~1, 
                       locations = dfsp_train,
                       newdata = dfsp_test, 
                       idp = 1)
head(test_idw@data)

# store estimation in df 
df_pred_idw <- data.frame(Easting = dfsp_test@data$Easting,
                          Northing = dfsp_test@data$Northing,
                          DryDensity = dfsp_test@data$DryDensity,
                          Prediction = test_idw@data$var1.pred)
df_pred_idw$Error <- abs(df_pred_idw$DryDensity - df_pred_idw$Prediction)

head(df_pred_idw)

# plot
fig_idw_diag <- ggplot(df_pred_idw) +
  geom_point(aes(x = DryDensity, y = Prediction)) +
  geom_abline(intercept = 0, slope = 1, 
              color = "blue", 
              linetype = "dashed") +
  labs(title = paste('IDW Estimation',
                     '\nRMSE =', round(RMSE(df_pred_idw$DryDensity,
                                          df_pred_idw$Prediction), 2),
                     '\nR2 =', round(postResample(df_pred_idw$DryDensity,
                                       df_pred_idw$Prediction)[2], 2)),
       x = "Actual Dry Density",
       y = "Estimated Dry Density") +
  coord_equal() +
  theme_bw()

fig_idw_diag

# plot error
fig_idw_error <- ggplot() +
  geom_point(data = df_pred_idw, 
             aes(x = Easting, y = Northing, color = Error),
             size = 2.5) +
  scale_color_gradient(low = "white", high = "red") +
  labs(x = "Easting", y = "Northing", title = "IDW Errors") +
  theme_bw()
  
fig_idw_error

# SPATIAL MAP #########################################

# create interpolated points
grid_points = sp::spsample(x = dfsp_train, n = 10000, type = 'regular')
plot(grid_points)

# IDW interpolation
map_idw <- gstat::idw(formula = DryDensity~1, 
                      locations = dfsp_train,
                      newdata = grid_points, 
                      idp = 1)

# Convert predictions to a data frame
grid_data <- data.frame(coordinates(grid_points), 
                        Prediction = map_idw@data$var1.pred)

head(grid_data)

# plot map
fig_idw <- ggplot() +
  geom_tile(data = grid_data, 
            aes(x = x1, y = x2, fill = Prediction)) +
  geom_point(data = df_test, aes(x = Easting, y = Northing, fill = DryDensity),
             color = 'black', size = 2.5, shape = 21) +
  scale_fill_gradient(low = "white", high = "red",
                      limits = c(min(df_train$DryDensity), 
                                 max(df_train$DryDensity))) +
  labs(x = "Easting", y = "Northing", 
       title = paste("IDW Interpolation vs Test Data",
                     '\nRMSE =', round(RMSE(df_pred_idw$DryDensity,
                                            df_pred_idw$Prediction), 2),
                     '\nR2 =', round(postResample(df_pred_idw$DryDensity,
                                                  df_pred_idw$Prediction)[2], 2))) +
  theme_bw()

fig_idw
