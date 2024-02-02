# SETUP ##########################################################

# set path
library(rstudioapi)
# set working directory
setwd(dirname(getActiveDocumentContext()$path))

library(tidyverse) # data toolbox
library(magrittr)
library(reshape2)
library(ggplot2) # plotting

# LOAD & PREPARE DATA ############################################

df_txcu <- NULL
for (i in 1:7) {
  txcu <- read_csv(paste0('../data/TXCU/sample', i, '.csv'))
  df_txcu <- rbind(df_txcu, txcu)
  
}

# plot TXCU stress-strain curves
ggplot(df_txcu) +
  geom_point(aes(x = Strain, y = Dev, 
                # linetype = as.character(Gs), 
                group = as.character(Conf))) +
  labs(title = 'Soil Samples',
       x = "Strain (%)", y = "Deviatoric Stress (psi)") +
  theme_bw() +
  facet_wrap(~Gs)


# explore data distribution
ggplot(df_txcu %>% melt(id.vars = NULL), 
       aes(x = value)) +
  geom_histogram(fill = "dodgerblue", color = "black") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Histograms of Data Distribution")


# sample for testing
ggplot(df_txcu %>% filter(Gs == 2.745)) +
  geom_point(aes(x = Strain, y = Dev, 
                color = as.character(Conf))) +
  labs(color = "Confining\nPressure",
       x = "Strain (%)", y = "Deviatoric Stress (psi)") +
  theme_bw() 


df_test <- df_txcu %>% 
  filter(Gs %in% c(2.745)) 
  # filter(Conf %in% c(26.6, 30.6, 11.4))

# training data
df_train <- anti_join(df_txcu, df_test)

# MODELING ####################################################################

library(ranger)

# training
model_rf <- ranger(Dev ~., data = df_train,
                   min.node.size = 3,
                   mtry = 6,
                   splitrule = 'extratrees')

# predictions
pred <- predict(model_rf, data = df_test)$predictions
df_pred <- df_test %>% mutate(Pred = pred)

# plot 1:1
ggplot(df_pred) +
  geom_point(aes(x = Dev, y = Pred, color = as.character(Conf))) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  xlim(0, 100) + ylim(0, 100) +
  theme_bw() +
  coord_equal()


# Plot Strain vs. Predicted
ggplot(df_pred) +
  geom_point(aes(x = Strain, y = Pred, color = as.character(Conf))) +
  geom_line(aes(x = Strain, y = Dev, color = as.character(Conf))) +
  labs(title = "Machine Learning-based Stress-Strain Curve",
       x = "Strain (%)", y = "Deviatoric Stress (psi)",
       color = "Confining\nPressure\n(psi)") +
  theme_bw()
