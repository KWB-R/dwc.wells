# load package, paths and variable sets from global.R --------------------------

source("inst/extdata/scripts/ml_resampling.R")
library(tidymodels)


# Specify model ----------------------------------------------------------------

dt_model <- decision_tree() %>%
  # Specify the engine
  set_engine('rpart') %>%
  # Specify the mode
  set_mode('classification')


# Define recipe ----------------------------------------------------------------

rec <- recipe(Qs_rel ~ ., data = df_training_cat)


# Create workflow --------------------------------------------------------------

dt_wflow <- workflow() %>%
  # Include the model object
  add_model(dt_model) %>%
  # Include the recipe object
  add_recipe(rec)


# Train the workflow / model and evaluate model performance --------------------

# train workflow
dt_fit <- dt_wflow %>%
  last_fit(split = data_split_cat)

# get predictions
df_pred <- dt_fit %>% collect_predictions()

# confusion matrix
matrix <- conf_mat(df_pred, truth = Qs_rel, estimate = .pred_class)

# performance metrics
metrics <- matrix %>% summary()

# mosaic plot
matrix %>% autoplot(type = 'mosaic') + labs(x = "Observation")
ggsave("random_forest_class_5vars_mosaic_matrix_plot_split80.png", dpi = 600, width = 3.5, height = 3)

# roc curve
auc <- sprintf("%.2f", roc_auc(df_pred, truth = Qs_rel, .pred_low)$.estimate)
roc_curve(df_pred, truth = Qs_rel, .pred_low) %>% autoplot() +
  annotate(x = 0.2, y = 0.8, geom = "text", label = paste("AUC =", auc),
           hjust = 0, vjust = 1, size = 3)
ggsave("decision_tree_roc_curve_split80.png", dpi = 600, width = 4, height = 2.5)

save_data(matrix, getwd(), "decision_tree_matrix_split80", formats = "RData")
save_data(metrics, getwd(), "decision_tree_metrics_split80")


# Hyperparameter tuning --------------------------------------------------------

if (FALSE) {

  # Specifcy model
  dt_tune_model <- decision_tree(cost_complexity = tune(),
                                 tree_depth = tune(),
                                 min_n = tune()) %>%
    set_engine('rpart') %>%
    set_mode('classification')

  # Define recipe
  rec <- recipe(Qs_rel ~ ., data = df_training_cat)

  # Create workflow
  dt_tune_wflow <- workflow() %>%
    # Include the model object
    add_model(dt_tune_model) %>%
    # Include the recipe object
    add_recipe(rec)


  # define cross validation procedure
  cv_folds <- vfold_cv(df_training_cat, v = 5)

  # parallelisation
  doParallel::registerDoParallel()

  # set up random grid
  set.seed(26)
  dt_grid <- grid_random(tree_depth(range = c(5, 15)),
                         min_n(range = c(2, 40)),
                         cost_complexity(range = c(0, 0.01), trans = NULL),
                         size = 1000)
  # dt_grid <- grid_regular(tree_depth(range = c(5, 15)),
  #                         min_n(range = c(1, 40)),
  #                         cost_complexity(range = c(0, 0.01), trans = NULL),
  #                         levels = 11)
  # tune hyperparameters
  dt_tuning <- dt_tune_wflow %>%
    tune_grid(resamples = cv_folds,
              grid = dt_grid)

  # show summarised metrics
  metrics <- dt_tuning %>% collect_metrics
  save_data(metrics, getwd(), "metrics_tuning_dt_random_random_resampling_classification")

  # show best model
  dt_tuning %>%
    show_best(metric = 'roc_auc', n = 5)

  # select best model
  best_dt_model <- dt_tuning %>%
    select_best(metric = 'roc_auc')

  save_data(best_dt_model, getwd(), "tuned_dt_class", "RData")


  # final training of best model -----

  # update workflow with best model
  final_dt_wflow <- dt_tune_wflow %>%
    finalize_workflow(best_dt_model)

  # Train and test tuned decision tree with all training and test data
  dt_final_fit <- final_dt_wflow %>%
    last_fit(split = data_split_cat)

  # View performance metrics
  dt_final_fit %>% collect_metrics()


  # Make predictions
  df_pred <- dt_final_fit %>% collect_predictions()


  # Evalulate model performance

  # confusion matrix
  matrix <- conf_mat(df_pred, truth = Qs_rel, estimate = .pred_class)

  # performance metrics
  metrics <- matrix %>% summary()

  # mosaic plot
  matrix %>% autoplot(type = 'mosaic') + labs(x = "Observation")
  ggsave("decision_tree_class_mosaic_matrix_plot_split80.png", dpi = 600, width = 3.5, height = 3)

  # roc curve
  auc <- sprintf("%.2f", roc_auc(test_results, truth = Qs_rel, .pred_low)$.estimate)
  roc_curve(test_results, truth = Qs_rel, .pred_low) %>% autoplot() +
    annotate(x = 0, y = 1, geom = "text", label = paste("AUC =", auc),
             hjust = 0, vjust = 1, size = 3)
  ggsave("decision_tree_class_roc_curve_split80.png", dpi = 600, width = 4, height = 2.5)

  save_data(matrix, getwd(), "decision_tree_class_matrix_split80", formats = "RData")
  save_data(metrics, getwd(), "decision_tree_class_metrics_split80")

}
