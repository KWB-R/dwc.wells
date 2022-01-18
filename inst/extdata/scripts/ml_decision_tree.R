# load package, paths and variable sets from global.R --------------------------

source("inst/extdata/scripts/ml_resampling.R")
library(tidymodels)


# Specify model ----------------------------------------------------------------

# Specify model (untuned, default hyperparameters)
dt_model <- decision_tree(tree_depth = 10, cost_complexity = 0.00075, min_n = 35) %>%
  # Specify the engine
  set_engine('rpart') %>%
  # Specify the mode
  set_mode('regression')


# Model training and assessment (regression) -----------------------------------

# Train model
set.seed(26)
dt_fit <- dt_model %>% fit(Qs_rel ~ ., data = df_training)

# Make predictions
predictions <- predict(dt_fit, df_test)

# Evaluate model performance
df_pred <- df_test %>% select(Qs_rel) %>% bind_cols(predictions)
rmse(df_pred, truth = Qs_rel, estimate = .pred)
rsq(df_pred, truth = Qs_rel, estimate = .pred)

# scatter plot
scatterplot(df_pred)
ggsave("scatterplot_decision_tree_resampling_by_well.png", width = 3.5, height = 3)


# V2: training and test using workflow and data_split object

if (FALSE) {

  # Specify model
  dt_model <- decision_tree() %>%
    # Specify the engine
    set_engine('rpart') %>%
    # Specify the mode
    set_mode('regression')

  # Define recipe
  rec <- recipe(Qs_rel ~ ., data = df_training)

  # Create workflow
  dt_wflow <- workflow() %>%
    # Include the model object
    add_model(dt_model) %>%
    # Include the recipe object
    add_recipe(rec)

  # Train the workflow
  dt_fit <- dt_wflow %>%
    last_fit(split = data_split)

  # Calculate performance metrics on test data
  dt_fit %>% collect_metrics()

  # Make predictions
  df_pred <- dt_fit %>% collect_predictions()

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

save_data(matrix, getwd(), "dt_numeric_to_class_matrix_split80_resampling_by_well", formats = "RData")
save_data(metrics, getwd(), "dt_numeric_to_class_metrics_split80_resampling_by_well")


# Tree visualisation
rpart.plot::rpart.plot(dt_fit$fit, roundint = FALSE)
ggsave("decision_tree_visualisation_only3levels.png", dpi = 600, width = 20, height = 15)
dev.off()


# Hyperparameter tuning --------------------------------------------------------

if (FALSE) {

  # specify model
  dt_tune_model <- decision_tree(cost_complexity = tune(),
                                 tree_depth = 10,
                                 min_n = tune()) %>%
    set_engine('rpart') %>%
    set_mode('regression')

  # define recipe
  rec <- recipe(Qs_rel ~ ., data = df_training)

  # setup workflow
  dt_tune_wflow <- workflow() %>%
    add_recipe(rec) %>%
    add_model(dt_tune_model)


# Create cross validation folds
set.seed(26)
cv_folds <- vfold_cv(df_training, v = 5, strata = Qs_rel)

# Create custom metrics function
my_metrics <- metric_set(rmse, rsq)

# set up grid
dt_grid <- grid_random(tree_depth(range = c(5, 15)),
                       min_n(range = c(2, 40)),
                       cost_complexity(range = c(0, 0.01), trans = NULL),
                       size = 1000)

dt_grid <- grid_regular(cost_complexity(range = c(0, 0.0025), trans = NULL),
                        min_n(range = c(5, 40)),
                        levels = c(11, 36))


# parallelisation
doParallel::registerDoParallel()

# tune hyperparameters
dt_tuning <- dt_tune_wflow %>%
  tune_grid(resamples = cv_folds,
            grid = dt_grid,
            metrics = my_metrics)

# show summarised metrics
metrics <- dt_tuning %>% collect_metrics
save_data(metrics, getwd(), "metrics_tuning_dt_regular_random_resampling_step2")

# visualise results
metrics %>%
  filter(.metric == "rmse") %>%
  select(mean, cost_complexity, tree_depth, min_n) %>%
  pivot_longer(c(cost_complexity, tree_depth, min_n),
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  #scale_x_continuous(breaks = seq.int(1, 15, 2)) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "RMSE [%]") +
  sema.berlin.utils::my_theme()

ggsave("dt_regression_hyperparameter_tuning_plot_random_random_resampling.png", width = 8, height = 3, dpi = 600)


# raster heatmap plot
metrics %>%
  filter(.metric == "rmse") %>%
  select(mean, min_n, cost_complexity) %>%
  ggplot(aes(x = min_n, y = cost_complexity, fill = mean)) +
  geom_raster() +
  #scale_fill_continuous(limits = c(23, 25)) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(fill = "RMSE [%]") +
  sema.berlin.utils::my_theme()

ggsave("dt_regression_hyperparameter_tuning_plot_regular_random_resampling_heatmap.png", width = 5, height = 3, dpi = 600)


# show best model
dt_tuning %>%
  show_best(metric = 'rmse', n = 5)

# select best model
best_dt_model <- dt_tuning %>%
  select_best(metric = 'rmse')

# save model
save_data(best_dt_model, getwd(), "tuned_dt_regression", "RData")

# update workflow with best model
final_dt_wflow <- dt_tune_wflow %>%
  finalize_workflow(best_dt_model)

# Train and test tuned decision tree with all training and test data
dt_final_fit <- final_dt_wflow %>%
  last_fit(split = data_split)

# View performance metrics
dt_final_fit %>% collect_metrics()

# Make predictions ---
df_pred <- dt_final_fit %>% collect_predictions()

# Evaluate model performance ---
scatterplot(df_pred)
ggsave("decision_tree_tuned.png", width = 3.5, height = 3)


# plot the tree ---

# calculate importance ranking
dt_final_fit <- final_dt_wflow %>%
  fit(df_training) %>%
  pull_workflow_fit()

p <- rpart.plot::rpart.plot(dt_final_fit$fit, roundint = FALSE)

ggsave("decision_tree_visualisation.png", p, dpi = 600, width = 20, height = 15)

}

