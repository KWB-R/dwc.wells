# load package, paths and variable sets from global.R --------------------------
library(dwc.wells)
source("inst/extdata/scripts/ml_resampling.R")


# Specify model ----------------------------------------------------------------

rf_model <- parsnip::rand_forest() %>%
  # Specify the engine
  parsnip::set_engine(engine = 'randomForest',
                      trees = 500,
                      mtry = 3,
                      n_min = 10) %>%
  # Specify the mode
  parsnip::set_mode('classification')


# Define recipe ----------------------------------------------------------------

rec <- recipes::recipe(Qs_rel ~ .,
                       data = df_training_cat)


# Create workflow --------------------------------------------------------------

rf_wflow <- workflows::workflow() %>%
  # Include the model object
  workflows::add_model(rf_model) %>%
  # Include the recipe object
  workflows::add_recipe(rec)


# Train the workflow / model and evaluate model performance --------------------

# train workflow
rf_fit <- rf_wflow %>%
  tune::last_fit(split = data_split_cat)

# get predictions
df_pred <- rf_fit %>%
  tune::collect_predictions()

# confusion matrix
matrix <- yardstick::conf_mat(df_pred, truth = Qs_rel, estimate = .pred_class)

# performance metrics
metrics <- matrix %>% summary()

# mosaic plot
matrix %>%
  ggplot2::autoplot(type = 'mosaic') +
  ggplot2::labs(x = "Observation")

ggplot2::ggsave("random_forest_class_5vars_mosaic_matrix_plot_split80.png",
                dpi = 600,
                width = 3.5,
                height = 3)

# roc curve
auc <- sprintf("%.2f",
               yardstick::roc_auc(df_pred, truth = Qs_rel, .pred_low)$.estimate)
yardstick::roc_curve(df_pred, truth = Qs_rel, .pred_low) %>%
  ggplot2::autoplot() +
  ggplot2::annotate(x = 0.2,
                    y = 0.8,
                    geom = "text",
                    label = paste("AUC =", auc),
                    hjust = 0,
                    vjust = 1,
                    size = 3)
ggplot2::ggsave("random_forest_class_5vars_roc_curve_split80.png",
                dpi = 600,
                width = 4,
                height = 2.5)

dwc.wells::save_data(matrix,
                     path = getwd(),
                     filename = "random_forest_class_5var_matrix_split80",
                     formats = "RData")


# Hyperparameter tuning --------------------------------------------------------

if (FALSE) {

rf_tune_model <- parsnip::rand_forest(trees = 500,
                                      mtry = tune::tune(),
                                      min_n = tune::tune()) %>%
  # Specify the engine
  parsnip::set_engine('randomForest') %>%
  # Specify the mode
  parsnip::set_mode('classification')

# setup new workflow
rf_tune_wflow <- workflows::workflow() %>%
  workflows::add_recipe(rec) %>%
  workflows::add_model(rf_tune_model)

# define cross validation procedure
cv_folds <- rsample::vfold_cv(df_training_cat, v = 5)

#rf_grid <- grid_random(parameters(rf_tune_model), size = 100)
#rf_grid <- grid_regular(parameters(rf_tune_model), c(5,5))

# set up random grid with 20 combinations for first screening
doParallel::registerDoParallel()

# 1 random grid
set.seed(345)
rf_tuning <- tune::tune_grid(
  rf_tune_wflow,
  resamples = cv_folds,
  grid = 100
)

# get metrics
metrics <- rf_tuning %>% tune::collect_metrics()

# visualise results
metrics %>%
  dplyr::filter(.metric == "roc_auc") %>%
  dplyr::select(mean, min_n, mtry) %>%
  tidyr::pivot_longer(min_n:mtry,
                      values_to = "value",
                      names_to = "parameter") %>%
  ggplot2::ggplot(ggplot2::aes(value, mean, color = parameter)) +
  ggplot2::geom_point(show.legend = FALSE) +
  ggplot2::facet_wrap(~parameter, scales = "free_x") +
  ggplot2::labs(x = NULL, y = "AUC") +
  sema.berlin.utils::my_theme()

ggplot2::ggsave("rf_class_hyperparameter_tuning_plot.png",
                width = 6,
                height = 3,
                dpi = 600)


# Select and retrain best model ------------------------------------------------

best_auc <- tune::select_best(rf_tuning, "roc_auc")

# regular grid results could be plotted as heatmap, analysis not yet conducted
rf_grid <- dials::grid_regular(dials::mtry(range = c(2, 20)),
                               dials::min_n(range = c(2, 20)),
                               levels = 19)

# select model with best results
final_rf <- tune::finalize_model(
  rf_tune_model,
  best_auc
)

# create final workflow
rf_final_wflow <- workflows::workflow() %>%
  workflows::add_recipe(rec) %>%
  workflows::add_model(final_rf)

# train and test final model
rf_final_fit <- rf_final_wflow %>%
  tune::last_fit(data_split_cat)

# collect metrics
rf_final_fit %>%  tune::collect_metrics()

# get predictions
df_pred <- rf_final_fit %>% tune::collect_predictions()


# Evalulate model performance ---

# confusion matrix
matrix <- yardstick::conf_mat(df_pred,
                              truth = Qs_rel,
                              estimate = .pred_class)

# performance metrics
metrics <- matrix %>% summary()

# mosaic plot
matrix %>%
  ggplot2::autoplot(type = 'mosaic') +
  ggplot2::labs(x = "Observation")

ggplot2::ggsave("random_forest_class_tuned_mosaic_matrix_plot_split80.png",
                dpi = 600,
                width = 3.5,
                height = 3)

# roc curve
auc <- sprintf("%.2f",
               yardstick::roc_auc(df_pred, truth = Qs_rel, .pred_low)$.estimate)
yardstick::roc_curve(df_pred, truth = Qs_rel, .pred_low) %>%
  ggplot2::autoplot() +
  ggplot2::annotate(x = 0.2,
                    y = 0.8,
                    geom = "text",
                    label = paste("AUC =", auc),
                    hjust = 0,
                    vjust = 1,
                    size = 3)
ggplot2::ggsave("random_forest_class_tuned_roc_curve_split80_v2.png",
                dpi = 600,
                width = 4,
                height = 2.5)

dwc.wells::save_data(matrix,
                     path = getwd(),
                     filename = "random_forest_class_tuned_matrix_split80",
                     formats = "RData")
dwc.wells::save_data(metrics,
                     path = getwd(),
                     filename = "random_forest_class_tuned_metrics_split80")

}
