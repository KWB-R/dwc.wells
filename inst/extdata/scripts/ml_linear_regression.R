# load package, paths and variable sets from global.R --------------------------

source("inst/extdata/scripts/ml_resampling.R")
library(tidymodels)


# Specify model ----------------------------------------------------------------

linear_model <- linear_reg() %>%
  # Set the model engine
  set_engine('lm') %>%
  # Set the model mode
  set_mode('regression')


# Build recipe -----------------------------------------------------------------

# (log transformation, normalisation, dummy variables, only for tests,
# no  recipe needed, normalisation, dummy variables, etc. have no effect
# on model performance)

if (FALSE) {

  # prepare recipe for Qs_rel numeric
  rec <- recipe(Qs_rel ~ ., data = df_training) %>%
    # Add log transformation step for skewed numeric predictors
    step_log(admissible_discharge, quality.TSS, quality.PO4, base = 10) %>%
    #normalize all data to a mean of 0 and sd of 1
    step_normalize(all_numeric()) %>%
    # transform categorical data into numerical dummy variables
    step_dummy(all_nominal(), -all_outcomes())

  # train recipe
  rec_prep <- rec %>% prep(training = df_training)

  # apply recipe to training and test data
  df_training_prep <- rec_prep %>% bake(new_data = NULL)
  df_test_prep <- rec_prep %>% bake(new_data = df_test)

}


# Model training and assessment (regression) -----------------------------------

# Train model
lm_fit <- linear_model %>% fit(Qs_rel ~ ., data = df_training)

# Make predictions
predictions <- predict(lm_fit, df_test)

# Evaluate model performance
df_pred <- df_test %>% select(Qs_rel) %>%  bind_cols(predictions)

rmse(df_pred, truth = Qs_rel, estimate = .pred)
rsq(df_pred, truth = Qs_rel, estimate = .pred)

# scatter plot
scatterplot(df_pred, lines_80perc = FALSE, alpha = 1, pointsize = 0.9)
ggsave("linear_model_25_vars.png", width = 3.5, height = 3)


# classification performance ---------------------------------------------------

# classify Qs data
df_pred <- df_pred %>%
  mutate(Qs_rel_class = classify_Qs(Qs_rel),
         .pred_class = classify_Qs(.pred))

# confusion matrix
matrix <- conf_mat(df_pred, truth = Qs_rel_class, estimate = .pred_class)

# performance metrics
metrics <- matrix %>% summary()

save_data(matrix, getwd(), "linear_regression__numeric_to_class_matrix_split80", formats = "RData")
save_data(metrics, getwd(), "linear_regression_numeric_to_class_metrics_split80")

