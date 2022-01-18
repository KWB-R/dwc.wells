# load package, paths and variable sets from global.R --------------------------

source("inst/extdata/scripts/ml_resampling.R")
library(tidymodels)


# Specify model ----------------------------------------------------------------

rf_model <- rand_forest() %>%
  # Specify the engine
  set_engine('randomForest', trees = 500, mtry = 3, n_min = 10) %>%
  # Specify the mode
  set_mode('classification')


# Define recipe ----------------------------------------------------------------

rec <- recipe(Qs_rel ~ ., data = df_training_cat)


# Create workflow --------------------------------------------------------------

rf_wflow <- workflow() %>%
  # Include the model object
  add_model(rf_model) %>%
  # Include the recipe object
  add_recipe(rec)


# Train the workflow / model and evaluate model performance --------------------

# train workflow
rf_fit <- rf_wflow %>%
  last_fit(split = data_split_cat)

# get predictions
df_pred <- rf_fit %>% collect_predictions()

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
ggsave("random_forest_class_5vars_roc_curve_split80.png", dpi = 600, width = 4, height = 2.5)

save_data(matrix, getwd(), "random_forest_class_5var_matrix_split80", formats = "RData")
save_data(metrics, getwd(), "random_forest_class_5var_metrics_split80")


# Hyperparameter tuning --------------------------------------------------------

if (FALSE) {

rf_tune_model <- rand_forest(trees = 500, mtry = tune(), min_n = tune()) %>%
  # Specify the engine
  set_engine('randomForest') %>%
  # Specify the mode
  set_mode('classification')

# setup new workflow
rf_tune_wflow <- workflow() %>%
  add_recipe(rec) %>%
  add_model(rf_tune_model)

# define cross validation procedure
cv_folds <- vfold_cv(df_training_cat, v = 5)

#rf_grid <- grid_random(parameters(rf_tune_model), size = 100)
#rf_grid <- grid_regular(parameters(rf_tune_model), c(5,5))

# set up random grid with 20 combinations for first screening
doParallel::registerDoParallel()

# 1 random grid
set.seed(345)
rf_tuning <- tune_grid(
  rf_tune_wflow,
  resamples = cv_folds,
  grid = 100
)

# get metrics
metrics <- rf_tuning %>% collect_metrics()

# visualise results
metrics %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC") +
  sema.berlin.utils::my_theme()

ggsave("rf_class_hyperparameter_tuning_plot.png", width = 6, height = 3, dpi = 600)


# Select and retrain best model ------------------------------------------------

best_auc <- select_best(rf_tuning, "roc_auc")

# regular grid results could be plotted as heatmap, analysis not yet conducted
rf_grid <- grid_regular(mtry(range = c(2, 20)),
                        min_n(range = c(2, 20)),
                        levels = 19)

# select model with best results
final_rf <- finalize_model(
  rf_tune_model,
  best_auc
)

# create final workflow
rf_final_wflow <- workflow() %>%
  add_recipe(rec) %>%
  add_model(final_rf)

# train and test final model
rf_final_fit <- rf_final_wflow %>%
  last_fit(data_split_cat)

# collect metrics
rf_final_fit %>%  collect_metrics()

# get predictions
df_pred <- rf_final_fit %>% collect_predictions()


# Evalulate model performance ---

# confusion matrix
matrix <- conf_mat(df_pred, truth = Qs_rel, estimate = .pred_class)

# performance metrics
metrics <- matrix %>% summary()

# mosaic plot
matrix %>% autoplot(type = 'mosaic') + labs(x = "Observation")
ggsave("random_forest_class_tuned_mosaic_matrix_plot_split80.png", dpi = 600, width = 3.5, height = 3)

# roc curve
auc <- sprintf("%.2f", roc_auc(df_pred, truth = Qs_rel, .pred_low)$.estimate)
roc_curve(df_pred, truth = Qs_rel, .pred_low) %>% autoplot() +
  annotate(x = 0.2, y = 0.8, geom = "text", label = paste("AUC =", auc),
           hjust = 0, vjust = 1, size = 3)
ggsave("random_forest_class_tuned_roc_curve_split80_v2.png", dpi = 600, width = 4, height = 2.5)

save_data(matrix, getwd(), "random_forest_class_tuned_matrix_split80", formats = "RData")
save_data(metrics, getwd(), "random_forest_class_tuned_metrics_split80")

}
