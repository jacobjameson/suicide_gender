################################################################################
# AUTHOR: J. Jameson

# DESCRIPTION: This script contains a function that performs imputation using 
# XGBoost regression. The function takes a dataset and a target variable as
# input, and returns the dataset with imputed values for the target variable.
# The function also outputs a CSV file containing the model specifications and
# performance metrics. The imputation is done using XGBoost regression with
# grid search and cross-validation for hyperparameter tuning.
################################################################################

# Load required libraries
library(xgboost)
library(keras)
library(dplyr)
library(tidyr)
library(caret)
library(randomForest)

# Function to perform imputation using XGBoost regression with grid search and CV
xg_imputation_verbose <- function(data, y_variable) {
  # Prepare the dataset by converting character columns to factors
  data <- data %>%
    mutate(across(where(is.character), factor)) 
  
  # Split data into non-suppressed (with target variable) and 
  # suppressed (without target variable)
  non_suppressed_data <- filter(data, !is.na(.data[[y_variable]]))
  suppressed_data <- filter(data, is.na(.data[[y_variable]]))
  
  # Unify factor levels across the dataset for consistency
  combined_data <- bind_rows(non_suppressed_data, suppressed_data)
  categorical_vars <- names(select(combined_data, where(is.factor)))
  combined_data <- combined_data %>%
    mutate(across(all_of(categorical_vars), 
                  factor, levels = unique(unlist(combined_data[categorical_vars]))))
  
  # Re-separate combined data back to consistent factor level dataframes
  non_suppressed_data <- filter(combined_data, !is.na(.data[[y_variable]]))
  suppressed_data <- filter(combined_data, is.na(.data[[y_variable]]))
  
  # Define predictors and model formula
  predictors <- setdiff(names(non_suppressed_data), y_variable)
  model_formula <- reformulate(predictors, response = y_variable)
  
  # Setup for 5-fold CV and grid search with parameter grid and verbose output
  train_control <- trainControl(method = "cv", number = 5, 
                                search = "grid", 
                                verboseIter = TRUE, 
                                summaryFunction = defaultSummary)
  
  tune_grid <- expand.grid(
    nrounds = c(50, 100),
    max_depth = c(3, 6),
    eta = c(0.01, 0.1),
    gamma = c(0),
    colsample_bytree = c(0.5, 0.8),
    min_child_weight = c(1, 3),
    subsample = c(0.5, 0.8)
  )
  
  
  # Train the model on non-suppressed data using 'caret' for grid search and CV
  xgb_model <- train(
    model_formula, data = non_suppressed_data,
    method = "xgbTree",
    trControl = train_control,
    tuneGrid = tune_grid,
    metric = "RMSE"
  )
  
  # Extract model performance metrics
  best_model_specs <- xgb_model$bestTune
  cv_metrics <- xgb_model$results %>%
    filter(RMSE == min(RMSE)) %>%
    dplyr::select(RMSE, Rsquared, MAE) %>%
    dplyr::slice(1) %>%
    mutate(Model = "XGBoost", 
           Best_nrounds = best_model_specs$nrounds, 
           Best_max_depth = best_model_specs$max_depth, 
           Best_eta = best_model_specs$eta, 
           Best_gamma = best_model_specs$gamma,
           Best_colsample_bytree = best_model_specs$colsample_bytree, 
           Best_min_child_weight = best_model_specs$min_child_weight,
           Best_subsample = best_model_specs$subsample)
  
  # Write model specifications and performance metrics to CSV
  write.csv(cv_metrics, 
            paste0('outputs/tables/XGBoost Performance_', y_variable, '.csv'), 
            row.names = FALSE)
  
  # Predict for both non-suppressed and suppressed data using the best model
  non_suppressed_data$Predictions <- predict(xgb_model, newdata = non_suppressed_data)
  non_suppressed_data$Predictions <- round(pmax(non_suppressed_data$Predictions, 0))
  
  if (nrow(suppressed_data) > 0) {
    suppressed_data$Predictions <- predict(xgb_model, newdata = suppressed_data)
    suppressed_data$Predictions <- round(pmax(pmin(suppressed_data$Predictions, 9), 1))
  } else {
    suppressed_data$Predictions <- NA
  }
  
  # Combine and return the results
  combined_results <- bind_rows(non_suppressed_data, suppressed_data)
  
  return(combined_results)
}


# Function to perform imputation using RandomForest
rf_imputation <- function(data, y_variable) {
  # Prepare the dataset by converting character columns to factors
  data <- data %>%
    mutate(across(where(is.character), factor)) 
  
  # Re-separate combined data back to consistent factor level dataframes
  non_suppressed_data <- filter(data, !is.na(.data[[y_variable]]))
  suppressed_data <- filter(data, is.na(.data[[y_variable]]))
  
  # Define predictors and model formula
  predictors <- setdiff(names(non_suppressed_data), y_variable)
  model_formula <- reformulate(predictors, response = y_variable)
  
  
  # Setup for 5-fold CV and grid search with parameter grid and verbose output
  train_control <- trainControl(method = "cv", number = 5, 
                                search = "grid", 
                                verboseIter = TRUE, 
                                summaryFunction = defaultSummary)
  
  # Train the model on non-suppressed data using 'caret' for grid search and CV
  rf_model <- train(
    model_formula, 
    data = non_suppressed_data,
    method = "rf",
    trControl = train_control,
    metric = "RMSE"
  )
  
  # Extract model performance metrics
  best_model_specs <- rf_model$bestTune
  cv_metrics <- rf_model$results %>%
    filter(RMSE == min(RMSE)) %>%
    dplyr::select(RMSE, Rsquared, MAE) %>%
    dplyr::slice(1) %>%
    mutate(Model = "RandomForest", 
           Best_mtry = best_model_specs$mtry)
  
  # Write model specifications and performance metrics to CSV
  write.csv(cv_metrics, 
            paste0('outputs/tables/RandomForest Performance_', y_variable, '.csv'), 
            row.names = FALSE)
  
  # Predict for both non-suppressed and suppressed data using the best model
  non_suppressed_data$Predictions <- predict(rf_model, newdata = non_suppressed_data)
  non_suppressed_data$Predictions <- round(pmax(non_suppressed_data$Predictions, 0))
  
  if (nrow(suppressed_data) > 0) {
    suppressed_data$Predictions <- predict(rf_model, newdata = suppressed_data)
    suppressed_data$Predictions <- round(pmax(pmin(suppressed_data$Predictions, 9), 1))
  } else {
    suppressed_data$Predictions <- NA
  }
  
  # Combine and return the results
  combined_results <- bind_rows(non_suppressed_data, suppressed_data)
  
  return(combined_results)
}

