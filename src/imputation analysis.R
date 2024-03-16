library(xgboost)
library(caret)
library(dplyr)
library(tidyr)

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
    colsample_bytree = c(0.5),
    min_child_weight = c(1, 2),
    subsample = c(0.8)
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
    select(RMSE, Rsquared, MAE) %>%
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

# Example usage:
# imputed_data <- xg_imputation_verbose(your_data, "YourTargetVariableName")
