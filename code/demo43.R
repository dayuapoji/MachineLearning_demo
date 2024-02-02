# KRIGING #################################################

# SPLIT DATA ##############################################
# Set a random seed for reproducibility
set.seed(123)

# Generate random indices for train and test split
train_indices <- sample(seq_len(nrow(df)), size = 0.8 * nrow(df), 
                        replace = FALSE)

# Split the dataframe into train and test
df_train <- df[train_indices, c('Easting', 'Northing', 'DryDensity')]
df_test <- df[-train_indices, c('Easting', 'Northing', 'DryDensity')]

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

# create a variogram model
variogram_model <- gstat::variogram(DryDensity ~ 1, data = dfsp_train)
plot(variogram_model)

# fit variogram
fit_var <- gstat::fit.variogram(object = variogram_model,
                                model = vgm(nugget = 16, 
                                            psill = 42, 
                                            range = 150,
                                            model = 'Exp'))
plot(variogram_model, model = fit_var)

# fit kriging model
test_krig <- gstat::krige(formula = DryDensity~1, 
                          locations = dfsp_train,
                          newdata = dfsp_test, 
                          model = fit_var)


head(test_krig@data)

# store estimation in df 
df_pred_krig <- data.frame(Easting = dfsp_test@data$Easting,
                           Northing = dfsp_test@data$Northing,
                           DryDensity = dfsp_test@data$DryDensity,
                           Prediction = test_krig@data$var1.pred)
df_pred_krig$Error <- abs(df_pred_krig$DryDensity - df_pred_krig$Prediction)

head(df_pred_krig)

# plot
fig_krig_diag <- ggplot(df_pred_krig) +
  geom_point(aes(x = DryDensity, y = Prediction)) +
  geom_abline(intercept = 0, slope = 1, 
              color = "blue", 
              linetype = "dashed") +
  labs(title = paste('Kriging Estimation',
                     '\nRMSE =', round(RMSE(df_pred_krig$DryDensity,
                                          df_pred_krig$Prediction), 2),
                     '\nR2 =', round(postResample(df_pred_krig$DryDensity,
                                                  df_pred_krig$Prediction)[2], 2)),
       x = "Actual Dry Density",
       y = "Estimated Dry Density") +
  coord_equal() +
  theme_bw()

plot_grid(fig_idw_diag, fig_krig_diag)

# SPATIAL MAP #########################################

# Interpolate using kriging on a grid
grid_points <- sp::spsample(x = dfsp_train, n = 10000, type = 'regular')

# Perform kriging interpolation
map_krig <- gstat::krige(DryDensity ~ 1, 
                         locations = dfsp_train, 
                         newdata = grid_points,
                         model = fit_var)

# Convert kriging predictions to a data frame
grid_data <- data.frame(coordinates(grid_points), 
                        Prediction = map_krig@data$var1.pred,
                        Variance = map_krig@data$var1.var)

# plot map
fig_krig <- ggplot() +
  geom_tile(data = grid_data, 
            aes(x = x1, y = x2, fill = Prediction)) +
  geom_point(data = df_test, aes(x = Easting, y = Northing, fill = DryDensity),
             color = 'black', size = 2.5, shape = 21) +
  scale_fill_gradient(low = "white", high = "red",
                      limits = c(min(df_train$DryDensity), 
                                 max(df_train$DryDensity))) +
  labs(x = "Easting", y = "Northing", 
       title = paste("Kriging Interpolation vs Test Data",
                     '\nRMSE =', round(RMSE(df_pred_krig$DryDensity,
                                            df_pred_krig$Prediction), 2),
                     '\nR2 =', round(postResample(df_pred_krig$DryDensity,
                                                  df_pred_krig$Prediction)[2], 2))) +
  theme_bw()

plot_grid(fig_idw, fig_krig)

# plot variance
ggplot() +
  geom_tile(data = grid_data, 
            aes(x = x1, y = x2, fill = Variance)) +
  scale_fill_gradient(low = "white", high = "red") + 
  scale_color_gradient(low = "yellow", high = "blue") + 
  labs(x = "Easting", y = "Northing") +
  theme_bw()

ggplot() +
  geom_tile(data = grid_data, 
            aes(x = x1, y = x2, fill = Variance)) +
  geom_point(data = df_train,
             aes(x = Easting, y = Northing, color = DryDensity),
             size = 2) +
  scale_fill_gradient(low = "white", high = "red") + 
  scale_color_gradient(low = "yellow", high = "blue") + 
  labs(x = "Easting", y = "Northing") +
  theme_bw()
