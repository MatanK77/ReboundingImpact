# This code is used to create an XGBoost model for defensive rebounding impact using tracking data. It requires the dataframe 
# full_dataset_pm_clusters, which can be acquired by first running the "Rebound Impact Modeling" script.


# Some Packages

library(tidyverse)
library(xgboost)
library(caret)


# Prepare Dataset for XGBoost by selecting useful cols and one-hot encoding 

full_dataset_pm_clustersforxg <- full_dataset_pm_clusters %>%
  select(cluster, PLAYER_HEIGHT_INCHES, `Adjusted_DReb_Chance%`, DReb_Distance, Defensive_boxouts_pm, Contested_DReb_pm, 
         Uncontested_DReb_pm, RA_ORBD__Def)

full_dataset_pm_clustersforxg$cluster <- factor(full_dataset_pm_clustersforxg$cluster, levels = 1:8)

dummy <- dummyVars(" ~ .", data= full_dataset_pm_clustersforxg)
full_dataset_pm_clustersforxg <- data.frame(predict(dummy, newdata = full_dataset_pm_clustersforxg)) 


# Split the transformed data into training and testing sets

set.seed(240)  # for reproducibility
train_indices <- createDataPartition(full_dataset_pm_clustersforxg$RA_ORBD__Def, p = 0.8, list = FALSE)
train_data <- full_dataset_pm_clustersforxg[train_indices, ]
test_data <- full_dataset_pm_clustersforxg[-train_indices, ]

dtrain <- xgb.DMatrix(data = as.matrix(train_data[, -15]), label = train_data$RA_ORBD__Def)
dtest <- xgb.DMatrix(data = as.matrix(test_data[, -15]), label = test_data$RA_ORBD__Def)

params <- list(
  objective = "reg:squarederror", 
  eval_metric = "rmse",  # Evaluation metric for training (Root Mean Squared Error)
  max_depth = 5,  # Maximum depth of a tree
  gamma = 1,
  eta = 0.03,  # Learning rate
  nrounds = 60,  # Number of boosting rounds
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Perform cross-validation
cv_result <- xgb.cv(
  data = dtrain,
  params = params,
  nfold = 5,  # Number of folds for cross-validation
  nrounds = params$nrounds,
  metrics = "rmse",  
  early_stopping_rounds = 10,  # Early stopping if the performance doesn't improve for 10 rounds
  seed = 104  # Set a seed for reproducibility
)

# Train the XGBoost model
xgb_model <- xgboost(data = dtrain, params = params, nrounds = params$nrounds)

# Make predictions on the test set
predictions <- predict(xgb_model, newdata = dtest)

pred <- as_tibble(predictions)

test_data$pred_dreb <- pred$value

sd_1 <- sd(test_data$RA_ORBD__Def)

rmse1 <- sqrt(mean((test_data$RA_ORBD__Def - test_data$pred_dreb)^2))

importance_matrix <- xgb.importance(model = xgb_model)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

# Taking a look at the results on the entire dataset

full_pred <- predict(xgb_model, newdata = as.matrix(full_dataset_pm_clustersforxg[,-15]))

full_pred_df <- as_tibble(full_pred)

full_pred_total <- full_dataset_pm_clusters %>%
  mutate(pred_dreb = full_pred_df$value, .after = RA_ORBD__Def)

