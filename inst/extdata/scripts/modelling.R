# load package, paths and variable sets from global.R --------------------------

source("inst/extdata/scripts/global.R")
library(tidymodels)


# MAIN 0: load, select and split model data ------------------------------------

if (TRUE) {

  # load data
  # load(file.path(paths$data_prep_out, "model_data.RData"))
  # # must not be loaded from external source as data is part of the model

  # delete columns like date and redundant columns in unit days instead of years
  df <- model_data %>% select(Qs_rel, all_of(model_features))
  #df <- model_data %>% select(Qs_rel, all_of(top5_model_features))

  # remove correlated and unimportant variables (see "03_variable_importance.R")
  if (TRUE) {

    # remove correlated variables
    df <- df %>%
      select(-c(well_depth, quality.DR, quality.P_tot,
                volume_m3_d.sd, waterworks, surface_water))

    # remove unimportant variables
    df <- df %>%
      select(-c(n_screens, filter_length, quality.Cu, inliner))

  }

  if (FALSE) {

    # filter further 6 variables with little importance
    df <- df %>%
      select(-c(drilling_method, aquifer_coverage, quality.NO3, diameter,
                quality.DO, quality.TSS))
  }

  # retransform n_rehab into integer variable
  df <- df %>% mutate(n_rehab = as.integer(n_rehab))


  # split / resample data, can be moved to data preparation
  set.seed(1)
  data_split <- initial_split(df, prop = 0.8, strata = Qs_rel)
  df_training <- data_split %>% training()
  df_test <- data_split %>% testing()

  # prepare datasets with categorical target variable
  df_training_cat <- df_training %>% mutate(Qs_rel = classify_Qs(Qs_rel))
  df_test_cat <- df_test %>% mutate(Qs_rel = classify_Qs(Qs_rel))
  data_split_cat <- initial_split(
    df %>% mutate(Qs_rel = classify_Qs(Qs_rel)),
    prop = 0.8,
    strata = Qs_rel
    )

  # plot distribution of goal variable in training and test data
  if (FALSE) {

    # first data overview
    rbind(data.frame(type = "training", value = df_training$Qs_rel),
          data.frame(type = "test", value = df_test$Qs_rel)) %>%
      mutate(type = factor(type, levels = c("training", "test"))) %>%
      ggplot(aes(x = type, y = value)) +
      geom_boxplot(width = 0.5) +
      # geom_violin() +
      sema.berlin.utils::my_theme() +
      scale_y_continuous(labels = paste_percent) +
      labs(x = "", y = "Specific capacity")

    ggsave("data_split_Qs_rel_distribution.png", dpi = 600, width = 4, height = 3)

  }
}


# MAIN 1: Linear regression model ---------------------------------------

if (FALSE) {

  # Specify model ---
  linear_model <- linear_reg() %>%
    # Set the model engine
    set_engine('lm') %>%
    # Set the model mode
    set_mode('regression')

  # no special recipe needed, normalisation, dummy variables have no effect on model performance

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

  # Train model ---

  # 5 most important variables
  lm_fit <- linear_model %>%
    fit(Qs_rel ~ well_age_years + time_since_rehab_years + n_rehab + well_gallery + volume_m3_d.cv, data = df_training)

  # all variables
  lm_fit <- linear_model %>% fit(Qs_rel ~ ., data = df_training_prep)


  # Make predictions ---
  predictions <- predict(lm_fit, df_test_prep)


  # Evaluate model performance ---

  # data frame with prediction results
  df_pred <- df_test %>% select(Qs_rel) %>%
    bind_cols(predictions) %>%
    rename(Qs_rel_pred = .pred)


  # error metrics
  a <- df_pred %>% rmse(truth = Qs_rel, estimate = Qs_rel_pred)
  b <- df_pred %>% rsq(truth = Qs_rel, estimate = Qs_rel_pred)

  # scatter plot
  ggplot(df_pred, aes(x = Qs_rel, y = Qs_rel_pred)) +
    geom_point() +
    geom_abline(color = 'blue', linetype = 2) +
    scale_x_continuous(lim = c(0, 100), labels = paste_percent) +
    scale_y_continuous(lim = c(0, 100), labels = paste_percent) +
    labs(x = "observations", y = "predictions") +
    sema.berlin.utils::my_theme() +
    annotate("text", x = 100, y = 5, hjust = 1, col = "grey30", size = 3,
             label = paste0("r² = ", sprintf("%.2f", b$.estimate), "\n",
                            "RMSE = ", sprintf("%.1f", a$.estimate), "%"))

  ggsave("linear_model_26_vars.png", width = 3.5, height = 3)

}


# MAIN 2: Logistic regression model --------------------------------------------

if (FALSE) {

    # prepare recipe for Qs_rel categorical ---
    rec_log <- recipe(Qs_rel ~ ., data = df_training_cat) %>%
      # Add log transformation step for skewed numeric predictors
      #step_log(admissible_discharge, quality.TSS, quality.PO4, base = 10) %>%
      #normalize all data to a mean of 0 and sd of 1
      step_normalize(all_numeric()) %>%
      # transform categorical data into numerical dummy variables
      step_dummy(all_nominal(), -all_outcomes())

    # train recipe ---
    rec_log_prep <- rec_log %>% prep(training = df_training_cat)

    # apply recipe to training and test data ---
    df_training_log_prep <- rec_log_prep %>% bake(new_data = NULL)
    df_test_log_prep <- rec_log_prep %>% bake(new_data = df_test_cat)


  # Specify model ---
  logistic_model <- logistic_reg() %>%
    # Set the engine
    set_engine('glm') %>%
    # Set the mode
    set_mode('classification')


  # Train model ---

  # all variables
  logistic_fit <- logistic_model %>% fit(Qs_rel ~ ., data = df_training_log_prep)


  # Make predictions ---
  # class
  class_preds <- predict(logistic_fit, new_data = df_test_log_prep, type = 'class')

  # probabilities for each outcome value
  prob_preds <- predict(logistic_fit, new_data = df_test_log_prep, type = 'prob')

  # Combine test set results
  test_results <- df_test_log_prep %>% cbind(class_preds, prob_preds)


  # Evalulate model performance ---

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

}


# MAIN 3: Decision tree --------------------------------------------------------

if (FALSE) {

  # Define recipe ---
  rec <- recipe(Qs_rel ~ ., data = df_training)

  # Specifcy model ---
  dt_model <- decision_tree() %>%
    # Specify the engine
    set_engine('rpart') %>%
    # Specify the mode
    set_mode('regression')


  # Create workflow ---
  dt_wflow <- workflow() %>%
    # Include the model object
    add_model(dt_model) %>%
    # Include the recipe object
    add_recipe(rec)


  # Train model ---

  # Train the workflow
  dt_wflow_fit <- dt_wflow %>%
    last_fit(split = data_split)

  # Calculate performance metrics on test data
  dt_wflow_fit %>%
    collect_metrics()

  # Make predictions ---
  df_pred <- dt_wflow_fit %>% collect_predictions()

  # Evaluate model performance ---
  # error metrics
  a <- df_pred %>% rmse(truth = Qs_rel, estimate = .pred)
  b <- df_pred %>% rsq(truth = Qs_rel, estimate = .pred)

  # scatter plot
  ggplot(df_pred, aes(x = Qs_rel, y = .pred)) +
    geom_point() +
    geom_abline(color = 'blue', linetype = 2) +
    scale_x_continuous(lim = c(0, 100), labels = paste_percent) +
    scale_y_continuous(lim = c(0, 100), labels = paste_percent) +
    labs(x = "observations", y = "predictions") +
    sema.berlin.utils::my_theme() +
    annotate("text", x = 100, y = 5, hjust = 1, col = "grey30", size = 3,
             label = paste0("r² = ", sprintf("%.2f", b$.estimate), "\n",
                            "RMSE = ", sprintf("%.1f", a$.estimate), "%"))

  ggsave("decision_tree_untuned.png", width = 3.5, height = 3)

  # Hyperparameter tuning ---

  # Create cross validation folds
  set.seed(290)
  cv_folds <- vfold_cv(df_training, v = 5, strata = Qs_rel)

  # Create custom metrics function
  my_metrics <- metric_set(rmse, rsq)

  # Fit resamples
  dt_rs <- dt_wflow %>%
    fit_resamples(resamples = cv_folds, metrics = my_metrics)

  # View performance metrics
  dt_rs %>%
    collect_metrics()

  # Hyperparameter tuning
  dt_tune_model <- decision_tree(cost_complexity = tune(),
                                 tree_depth = tune(),
                                 min_n = tune()) %>%
    set_engine('rpart') %>%
    set_mode('regression')


  dt_tune_wflow <- dt_wflow %>%
    update_model(dt_tune_model)

  # set up random grid
  set.seed(214)
  dt_grid <- grid_random(parameters(dt_tune_model), size = 100)

  #grid_regular(parameters(dt_tune_model), c(5,5,5))

  # tune hyperparameters
  dt_tuning <- dt_tune_wflow %>%
    tune_grid(resamples = cv_folds,
              grid = dt_grid,
              metrics = my_metrics)

  # show summarised metrics
  dt_tuning %>% collect_metrics

  # show full metrics, filtered
  dt_tuning %>%
    collect_metrics(summarize = FALSE) %>%
    filter(.metric == 'rmse') %>%
    group_by(.config) %>%
    summarize(min_rmse = min(.estimate),
              median_rmse = median(.estimate),
              mean_rmse = mean(.estimate),
              max_rmse = max(.estimate))

  # show best model
  dt_tuning %>%
    show_best(metric = 'rmse', n = 5)

  # select best model
  best_dt_model <- dt_tuning %>%
    select_best(metric = 'rmse')

  # update workflow
  final_dt_wflow <- dt_tune_wflow %>%
    finalize_workflow(best_dt_model)

  # Train finalized decision tree workflow
  dt_final_fit <- final_dt_wflow %>%
    last_fit(split = data_split)

  # View performance metrics
  dt_final_fit %>% collect_metrics()

  # Make predictions ---
  df_pred <- dt_final_fit %>% collect_predictions()

  # Evaluate model performance ---
  # error metrics
  a <- df_pred %>% rmse(truth = Qs_rel, estimate = .pred)
  b <- df_pred %>% rsq(truth = Qs_rel, estimate = .pred)

  # scatter plot
  ggplot(df_pred, aes(x = Qs_rel, y = .pred)) +
    geom_point() +
    geom_abline(color = 'blue', linetype = 2) +
    scale_x_continuous(lim = c(0, 100), labels = paste_percent) +
    scale_y_continuous(lim = c(0, 100), labels = paste_percent) +
    labs(x = "observations", y = "predictions") +
    sema.berlin.utils::my_theme() +
    annotate("text", x = 100, y = 5, hjust = 1, col = "grey30", size = 3,
             label = paste0("r² = ", sprintf("%.2f", b$.estimate), "\n",
                            "RMSE = ", sprintf("%.1f", a$.estimate), "%"))

  ggsave("decision_tree_tuned.png", width = 3.5, height = 3)

  # plot the tree

  # calculate importance ranking
  dt_final_fit <- final_dt_wflow %>%
    fit(df_training) %>%
    pull_workflow_fit()

  p <- rpart.plot::rpart.plot(dt_final_fit$fit, roundint = FALSE)

  ggplot(dt_final_fit$fit)
  ggsave("decision_tree_visualisation.png", p, dpi = 600, width = 20, height = 15)

  }


# MAIN 4: Random Forest --------------------------------------------------------

if (FALSE) {

  # Define recipe ---
  rec <- recipe(Qs_rel ~ ., data = df_training_cat)

  # Specifcy model ---
  rf_model <- rand_forest() %>%
    # Specify the engine
    set_engine('randomForest', trees = 500, mtry = 5, n_min = 10) %>%
    # Specify the mode
    set_mode('classification')


  # Create workflow ---
  rf_wflow <- workflow() %>%
    # Include the model object
    add_model(rf_model) %>%
    # Include the recipe object
    add_recipe(rec)


  # Train model ---

  # Train the workflow
  rf_wflow_fit <- rf_wflow %>%
    last_fit(split = data_split_cat)

  # Calculate performance metrics on test data
  rf_wflow_fit %>%
    collect_metrics()


  # Hyperparameter tuning ---

  rf_tune_model <- rand_forest(trees = 500, mtry = tune(), min_n = tune()) %>%
    # Specify the engine
    set_engine('randomForest') %>%
    # Specify the mode
    set_mode('classification')


  # setup new workflow
  rf_tune_wflow <- workflow() %>%
    add_recipe(rec) %>%
    add_model(rf_tune_model)

  # alternative
  #rf_tune_wflow <- rf_wflow %>%
    #update_model(rf_tune_model)


  # define cross validation procedure
  cv_folds <- vfold_cv(df_training_cat, v = 5)

  # set up random grid with 20 combinations for first screening
   doParallel::registerDoParallel()

  set.seed(345)
  tune_res <- tune_grid(
    rf_tune_wflow,
    resamples = cv_folds,
    grid = 100
  )

  # visualise results
  tune_res %>%
    collect_metrics() %>%
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


  rf_grid <- grid_regular(
    mtry(range = c(1, 26)),
    min_n(range = c(1, 30)),
    levels = 5
  )

  rf_grid

  set.seed(456)
  regular_res <- tune_grid(
    rf_tune_wflow,
    resamples = cv_folds,
    grid = rf_grid
  )

  regular_res %>%
    collect_metrics() %>%
    filter(.metric == "roc_auc") %>%
    mutate(min_n = factor(min_n)) %>%
    ggplot(aes(mtry, mean, color = min_n)) +
    geom_line(alpha = 0.5, size = 1.5) +
    geom_point() +
    labs(y = "AUC") +
    sema.berlin.utils::my_theme()

  # determine best model
  best_auc <- select_best(regular_res, "roc_auc")


  final_rf <- finalize_model(
    rf_tune_model,
    best_auc
  )

  rf_final_wflow <- workflow() %>%
    add_recipe(rec) %>%
    add_model(final_rf)

  final_res <- rf_final_wflow %>%
    last_fit(data_split_cat)

  final_res %>%
    collect_metrics()

  # Make predictions ---
  df_pred <- final_res %>% collect_predictions()


  # Evalulate model performance ---

  # confusion matrix
  matrix <- conf_mat(df_pred, truth = Qs_rel, estimate = .pred_class)

  # performance metrics
  metrics <- matrix %>% summary()

  # mosaic plot
  matrix %>% autoplot(type = 'mosaic') + labs(x = "Observation")
  ggsave("random_forest_class_mosaic_matrix_plot_split80.png", dpi = 600, width = 3.5, height = 3)

  # roc curve
  auc <- sprintf("%.2f", roc_auc(df_pred, truth = Qs_rel, .pred_low)$.estimate)
  roc_curve(df_pred, truth = Qs_rel, .pred_low) %>% autoplot() +
    annotate(x = 0, y = 1, geom = "text", label = paste("AUC =", auc),
             hjust = 0, vjust = 1, size = 3)
  ggsave("random_forest_class_roc_curve_split80.png", dpi = 600, width = 4, height = 2.5)

  save_data(matrix, getwd(), "random_forest_matrix_split80", formats = "RData")
  save_data(metrics, getwd(), "random_forest_metrics_split80")

}


# MAIN 5: Gradient boosting ----------------------------------------------------

if (FALSE) {

  # https://www.tidymodels.org/find/parsnip/
  # Specify model
  mod_boost <- boost_tree() %>%
    set_engine("xgboost", nthreads = parallel::detectCores()) %>%
    set_mode("regression")

  # create workflow
  wflow_boost <- workflow() %>%
    add_recipe(rec) %>%
    add_model(mod_boost)

  cv_folds <- vfold_cv(df_training, v = 5, strata = Qs_rel)

  set.seed(100)
  plan(multisession)

  fit_boost <- fit_resamples(
    wflow_boost,
    cv_folds,
    metrics = metric_set(rmse, rsq),
    control = control_resamples(verbose = TRUE,
                                save_pred = TRUE)
  )

  fit_boost %>% collect_metrics()

}

