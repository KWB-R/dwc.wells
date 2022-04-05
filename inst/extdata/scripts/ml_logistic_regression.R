# load package, paths and variable sets from global.R --------------------------

source("inst/extdata/scripts/ml_resampling.R")
library(tidymodels)


# Specify model ----------------------------------------------------------------

logistic_model <- logistic_reg() %>%
  # Set the engine
  set_engine('glm') %>%
  # Set the mode
  set_mode('classification')


# Prepare recipe (is required) -------------------------------------------------

# prepare recipe for Qs_rel categorical
rec_log <- recipe(Qs_rel ~ ., data = df_training_cat) %>%
  # Add log transformation step for skewed numeric predictors
  #step_log(admissible_discharge, quality.TSS, quality.PO4, base = 10) %>%
  #normalize all data to a mean of 0 and sd of 1
  step_normalize(all_numeric()) %>%
  # transform categorical data into numerical dummy variables
  step_dummy(all_nominal(), -all_outcomes())

# train recipe
rec_log_prep <- rec_log %>% prep(training = df_training_cat)

# apply recipe to training and test data
df_training_log_prep <- rec_log_prep %>% bake(new_data = NULL)
df_test_log_prep <- rec_log_prep %>% bake(new_data = df_test_cat)


# Model training and assessment (classification) -------------------------------

# Train model
logistic_fit <- logistic_model %>% fit(Qs_rel ~ ., data = df_training_log_prep)

# Make predictions with class output
class_preds <- predict(logistic_fit, new_data = df_test_log_prep, type = 'class')

# Make prediction with probability output
prob_preds <- predict(logistic_fit, new_data = df_test_log_prep, type = 'prob')

# Combine test set results
test_results <- df_test_log_prep %>% cbind(class_preds, prob_preds)

# Evalulate model performance

# confusion matrix
matrix <- conf_mat(test_results, truth = Qs_rel, estimate = .pred_class)

# performance metrics
metrics <- matrix %>% summary()

# mosaic plot
matrix %>% autoplot(type = 'mosaic') + labs(x = "Observation")
ggsave("log_regression_mosaic_matrix_plot_split80.png", dpi = 600, width = 3.5, height = 3)

# roc curve
auc <- sprintf("%.2f", roc_auc(test_results, truth = Qs_rel, .pred_low)$.estimate)
roc_curve(test_results, truth = Qs_rel, .pred_low) %>% autoplot() +
  annotate(x = 0, y = 1, geom = "text", label = paste("AUC =", auc),
           hjust = 0, vjust = 1, size = 3)
ggsave("log_regression_roc_curve_split80.png", dpi = 600, width = 4, height = 2.5)

save_data(matrix, getwd(), "log_regression_matrix_split80", formats = "RData")
save_data(metrics, getwd(), "log_regression_metrics_split80")

