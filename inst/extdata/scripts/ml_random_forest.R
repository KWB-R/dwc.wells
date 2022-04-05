# load package, paths and variable sets from global.R --------------------------

source("inst/extdata/scripts/ml_resampling.R")
library(tidymodels)


# Specify model ----------------------------------------------------------------

rf_model <- rand_forest(mtry = 6, trees = 500, min_n = 10) %>%
  # Specify the engine
  set_engine('randomForest') %>%
  # Specify the mode
  set_mode('regression')


# Model training and assessment (regression) -----------------------------------

# Train model
set.seed(26)
rf_fit <- rf_model %>% fit(Qs_rel ~ ., data = df_training)

# Make predictions
predictions <- predict(rf_fit, df_test)

# Evaluate model performance
df_pred <- df_test %>% select(Qs_rel) %>% bind_cols(predictions)
rmse(df_pred, truth = Qs_rel, estimate = .pred)
rsq(df_pred, truth = Qs_rel, estimate = .pred)

# scatter plot
scatterplot(df_pred, lines_80perc = FALSE, alpha = 1, pointsize = 0.9)
ggsave("scatterplot_rf_numeric.png", dpi = 600, width = 3.5, height = 3)


# V2: training and test using workflow and data_split object

if (FALSE) {

  # Define recipe
  rec <- recipe(Qs_rel ~ ., data = df_training)

  # Create workflow
  rf_wflow <- workflow() %>%
    # Include the model object
    add_model(rf_model) %>%
    # Include the recipe object
    add_recipe(rec)

  # Train the workflow
  set.seed(26)
  rf_fit <- rf_wflow %>%
    last_fit(split = data_split)

  # Calculate performance metrics on test data
  rf_fit %>% collect_metrics()

  # get predictions
  df_pred <- rf_fit %>% collect_predictions()

}


# classification performance ---------------------------------------------------

# classify Qs data
df_pred <- df_pred %>%
  mutate(Qs_rel_class = classify_Qs(Qs_rel),
         .pred_class = classify_Qs(.pred))

# confusion matrix
matrix <- conf_mat(df_pred, truth = Qs_rel_class, estimate = .pred_class)
matrix %>% autoplot(type = 'mosaic') + labs(x = "Observation")

# performance metrics
metrics <- matrix %>% summary()

save_data(matrix, getwd(), "rf_numeric_to_class_matrix_split80", formats = "RData")
save_data(metrics, getwd(), "rf_numeric_to_class_metrics_split80")


# Hyperparameter tuning --------------------------------------------------------

if (FALSE) {

  # specify model
rf_tune_model <- rand_forest(trees = 500, mtry = tune(), min_n = tune()) %>%
  # Specify the engine
  set_engine('randomForest') %>%
  # Specify the mode
  set_mode('regression')

# specify recipe
rec <- recipe(Qs_rel ~ ., data = df_training)

# setup  workflow
rf_tune_wflow <- workflow() %>%
  add_recipe(rec) %>%
  add_model(rf_tune_model)

# define cross validation procedure
cv_folds <- vfold_cv(df_training, v = 5)

# define hyperparameter grid
#rf_grid <- grid_random(parameters(rf_tune_model), size = 100)
#rf_grid <- grid_regular(parameters(rf_tune_model), c(5,5))
rf_grid <- grid_regular(mtry(range = c(3, 15)),
                        min_n(range = c(5, 15)),
                        levels = 13)

# parallelisation and tuning
doParallel::registerDoParallel()

set.seed(345)
rf_tuning <- tune_grid(
  rf_tune_wflow,
  resamples = cv_folds,
  #grid = 100
  grid = rf_grid
)

# visualise results
metrics <- rf_tuning %>% collect_metrics()
save_data(metrics, getwd(), "metrics_tuning_rf_regular_ramdom_resampling")

# visualise results
metrics %>%
  filter(.metric == "rmse") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  scale_x_continuous(breaks = seq.int(1, 15, 2)) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "RMSE [%]") +
  sema.berlin.utils::my_theme()

ggsave("rf_regression_hyperparameter_tuning_plot_regular_random_resampling.png", width = 6, height = 3, dpi = 600)

# raster heatmap plot
metrics %>%
  filter(.metric == "rmse") %>%
  select(mean, min_n, mtry) %>%
  ggplot(aes(x = min_n, y = mtry, fill = mean)) +
  geom_raster() +
  scale_x_continuous(breaks = seq.int(1, 15, 2)) +
  scale_y_continuous(breaks = seq.int(1, 15, 2)) +
  labs(fill = "RMSE [%]") +
  sema.berlin.utils::my_theme()

ggsave("rf_regression_hyperparameter_tuning_plot_regular_ramdom_resampling_heatmap.png", width = 5, height = 3, dpi = 600)


# determine best model
best_rmse <- select_best(rf_tuning, "rmse")
save_data(best_rmse, getwd(), "rf_regression_best_model_regular", "RData")
final_rf <- finalize_model(rf_tune_model, best_rmse)

# update workflow
rf_final_wflow <- workflow() %>%
  add_recipe(rec) %>%
  add_model(final_rf)

# train and test model / workflow
rf_final_fit <- rf_final_wflow %>% last_fit(data_split)

#get metrics
rf_final_fit %>% collect_metrics()

# get predictions
df_pred <- rf_final_fit %>% collect_predictions()

# Evaluate model performance ---
scatterplot(df_pred)
ggsave("random_forest_regression_tuned_regular.png", width = 3.5, height = 3)

}

