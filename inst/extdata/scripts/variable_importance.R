# load package, paths and variable sets from global.R --------------------------

source("inst/extdata/scripts/global.R")

# load(file.path(paths$data_prep_out, "model_data.RData"))
# must not be loaded from external source as data is part of the model

# delete columns like date and redundant columns in unit days instead of years
df <- model_data %>% select(Qs_rel, all_of(model_features))

# turn n_rehabs and n_screens into factor for correlation analysis
df <- df %>%
  dplyr::mutate(
    n_rehab = as.factor(n_rehab) %>% tidy_factor(level_sorting = "alphabet"),
    n_screens = as.factor(n_screens) %>% tidy_factor(level_sorting = "alphabet")
  )


# MAIN 1: Pearson correlation for numerical variables --------------------------

if (FALSE) {

  # get numerical variables
  is_numeric <- unlist(lapply(df, is.numeric))
  num_vars <- names(df)[is_numeric]
  num_input_vars <- num_vars[num_vars %in% model_features]
  df_num <- df %>% dplyr::select("Qs_rel", num_input_vars)

  pearson <- cor(df_num, use = "pairwise.complete.obs", method = "pearson")

  # simple plot: upper and lower half squares
  pdf(file.path("numerics_cor_pearson.pdf"), width = 10, height = 10)
  dwc.wells:::my.corrplot(pearson)
  dev.off()

  pdf(file.path("numerics_cor_pearson_v2.pdf"), width = 10, height = 10)
  dwc.wells:::my.corrplot.mixed(pearson, number.cex = 0.5)
  dev.off()

  save_data(pearson, path = getwd(), "pearson")

}


# MAIN 2: Spearman correlation for numerical variables --------------------------

if (FALSE) {

  # get numerical variables
  is_numeric <- unlist(lapply(df, is.numeric))
  num_vars <- names(df)[is_numeric]
  num_input_vars <- num_vars[num_vars %in% model_features]
  df_num <- df %>% dplyr::select("Qs_rel", num_input_vars)

  spearman <- cor(df_num, use = "pairwise.complete.obs", method = "spearman")

  # simple plot: upper and lower half squares
  pdf(file.path("numerics_cor_spearman.pdf"), width = 10, height = 10)
  dwc.wells:::my.corrplot(spearman)
  dev.off()

  pdf(file.path("numerics_cor_spearman_v2.pdf"), width = 10, height = 10)
  dwc.wells:::my.corrplot.mixed(spearman, number.cex = 0.5)
  dev.off()

  save_data(spearman, path = getwd(), "spearman")

  names(df)
  ggplot(df, aes(x = volume_m3_d.mean, y = volume_m3_d.cv)) +
    scale_y_continuous(limits = c(0, 1)) +
    geom_point()
}

# MAIN 3: Chi-square test for categorical variables -----------------------------

if (FALSE) {

  # create categorical variable from Qs_rel
  df$Qs_rel_cat <- classify_Qs(df$Qs_rel, 80, class_names = c("1-low", "2-high"))
  frequency_table(df$Qs_rel_cat)

  # get categorical variables
  is_factor <- unlist(lapply(df, is.factor))
  cat_vars <- names(df)[is_factor]
  cat_input_vars <- cat_vars[cat_vars %in% model_features]
  df_cat <- df %>% dplyr::select("Qs_rel_cat", cat_input_vars)


  # chi2.CramersV.test with barplot
  chi.stats <- dwc.wells:::chi2.CramersV.test(df_cat)
  chi2.values <- chi.stats[[1]] # chi2-values
  p.values <- chi.stats[[2]] # p-values (dependent of sample size)
  cramers.V <- chi.stats[[3]] # measure of effect size: "Cramér's V" (independent of sample size)
  save_data(cramers.V, path = paths$stats, "CramersV_binary_split_80")

  # correlation plot
  pdf(file.path("categorical_data_split_80_CramersV.pdf"), width = 6, height = 6)
  dwc.wells:::my.corrplot(cramers.V)
  dev.off()

}

# MAIN 4: Boruta algorithm, based on Random Forest -----------------------------

if (FALSE) {

  library(Boruta)
  #boruta <- Boruta(x = df[, model_features], y = df$Qs_rel, doTrace = 2, maxRuns = 500)
  boruta <- Boruta(x = df %>% select(-Qs_rel), y = df$Qs_rel, doTrace = 2, maxRuns = 500)
  print(boruta)
  boruta_stats <- attStats(boruta)
  save_data(boruta_stats, getwd(), "boruta_stats")
  png(file.path("boruta_variable_importance_reduced.png"), width = 15, height = 15, res = 600, units = "cm")
  par(mar = c(4, 8, 1, 1))
  plot(boruta, las = 1, cex.axis = 0.7, ylab = "", xlab = "Importance [%]",
       whichShadow = c(F, F, F), ylim = c(0, 50), horizontal = TRUE)
  dev.off()
}


# MAIN 5: Random Forest --------------------------------------------------------

if (FALSE) {

  # not used as prediction model but for estimating variable importance
  rf_model <- randomForest::randomForest(x = df_training %>% select(-Qs_rel),
                                         y = df_training$Qs_rel,
                                         mtry = 5,
                                         ntrees = 500,
                                         nodesize = 10,
                                         importance = TRUE)


  # randomForestExplainer methods
  importance <- randomForest::importance(rf_model)
  importance <- data.frame(variable = rownames(importance), importance, row.names = NULL)

  importance <- importance %>%
    mutate(variable = forcats::fct_reorder(variable, IncNodePurity))

  importance <- importance %>%
    mutate(variable = forcats::fct_reorder(variable, MeanDecreaseGini))

  importance %>%
    ggplot(aes(x = variable, y = IncNodePurity / 10^6)) +
    geom_bar(stat = "identity", fill = "chocolate2", width = 0.7) +
    sema.berlin.utils::my_theme() +
    labs(y = "Increase in node purity [10^6]", x = "") +
    scale_y_continuous(expand = c(0.02, 0.02), breaks = scales::pretty_breaks(6)) +
    scale_x_discrete(expand = c(0.03, 0.03)) +
    coord_flip() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 13),
          axis.title.x = element_text(size = 13))

  ggsave("rf_purity_increase_25vars_v2.png", dpi = 600, width = 7, height = 5)


}

# MAIN 6: Random Forest, recusive feature elimination --------------------------

if (FALSE) {

  # ensure the results are repeatable
  set.seed(2)

  # load the library
  library(caret)

  # define the control using a random forest selection function
  control <- caret::rfeControl(functions = rfFuncs, method = "cv", number = 5)


  # parallelisation
  doParallel::registerDoParallel()


  # run the RFE algorithm
  results <- caret::rfe(x = df %>% select(-Qs_rel),
                        y = df$Qs_rel,
                        sizes = c(1:36),
                        metric = "RMSE",
                        rfeControl = control)

  res <- data.frame(var_name = results$variables$var[1:36],
             results$results)

  suboptimal_variables <- setdiff(var_names, results$optVariables)
  importance <- data.frame(results$fit$importance)

  ggplot(results) + sema.berlin.utils::my_theme() + labs(x = "Number of variables", y = "RMSE [%]")
  ggsave("results_RFE.png", dpi = 600, width = 4, height = 3)
  # plot the results
  plot(results, type = c("g", "o"))

  save_data(results, getwd(), "results_rfe", formats = "RData")

  # summarize the results
  print(results)
  # list the chosen features
  predictors(results)
  results$results



  str(results)
  a <- results$variables %>% filter(Resample == "Fold1") %>%
    select(var, Variables)

  str(results)
  var_names[!var_names %in% rownames(results$fit$importance)]
  l <- list
  for (i in unique(a$Variables)) {
    l[[i]] <- var_names[!var_names %in% a$var[a$Variables == i]]
  }


  a %>% group_by(Variables) %>%
    pull(!var_names %in% var)
  unique(a$var[a$Variables == 36])
  unique(a$var[a$Variables == 35])
  str(results)
  results$fit$importance


  # plot the results
  plot(results, type = c("g", "o"))

  str(results)
  results$optVariables
  plot(results, type = c("g", "o")) + scale_x_continuous(labels = var_names)

  df %>% filter(Qs_rel >= 80) %>%
    frequency_table(key)

}


# MAIN 7: Lasso regression -----------------------------------------------------

if (FALSE) {

  library(tidymodels)

  # following the example of: https://juliasilge.com/blog/lasso-the-office/

  # for lasso regression all data has to be numeric, so dummy variables are required

  # prepare data ---

  # create recipe
  lasso_rec <- recipe(Qs_rel ~ ., data = df_training) %>%
    step_zv(all_numeric(), -all_outcomes()) %>% # discard zero variance variables
    step_normalize(all_numeric(), -all_outcomes()) %>% # normalize variables
    step_dummy(all_nominal(), -all_outcomes()) # create dummy variables

  # train recipe
  lasso_prep <- lasso_rec %>%
    prep(training = df_training, strings_as_factors = FALSE)

  # prepare training and test data according to recipe
  df_training_prep <- lasso_prep %>% bake(new_data = NULL)
  df_test_prep <- lasso_prep %>% bake(new_data = df_test)


  # train default lasso regression model ---

  # define model
  lasso_spec <- linear_reg(penalty = 0.1, mixture = 1) %>%
    set_engine("glmnet")

  # define workflow with recipe
  wflow <- workflow() %>%
    add_recipe(lasso_rec)

  # add model to workflow and fit training data
  lasso_fit <- wflow %>%
    add_model(lasso_spec) %>%
    fit(data = df_training)

  # get results of fit model with default parameters
  lasso_fit %>%
    pull_workflow_fit() %>%
    tidy()

  # tune model hyperparameters ---

  # create boostrap sampling
  set.seed(1234)
  df_training_boot <- bootstraps(df_training, strata = Qs_rel)

  # define tuning variables
  tune_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet", type.multinomial = "grouped")

  # define grid of 50 different values
  lambda_grid <- grid_regular(penalty(), levels = 50)

  # parallelisation
  doParallel::registerDoParallel()

  # tune a workflow that consists of a a recipe and amodel
  # defining the bootstrap samples and the parameter grid
  set.seed(2020)
  lasso_grid <- tune_grid(
    wflow %>% add_model(tune_spec),
    resamples = df_training_boot,
    grid = lambda_grid
  )

  # collect metrics for all 50 models
  lasso_grid %>%
    collect_metrics()


  # plot metrics
  p <- lasso_grid %>%
    collect_metrics() %>%
    ggplot(aes(penalty, mean, color = .metric)) +
    geom_errorbar(aes(
      ymin = mean - std_err,
      ymax = mean + std_err
    ),
    width = 0.1,
    alpha = 0.5
    ) +
    geom_point(size = 2) +
    facet_wrap(~.metric, scales = "free", nrow = 2) +
    scale_x_log10() +
    sema.berlin.utils::my_theme(legend.position = "none")

  plotly::ggplotly(p)

  ggsave("lasso_regression_penalty_tuning.png", dpi = 600, width = 7, height = 6)

  # train and test final model with best penalty value lambda = 0.0954 ---

  # get parametrisation of best model
  lowest_rmse <- lasso_grid %>%
    select_best("rmse")

  # finalise workflow with tuned model
  final_lasso <- finalize_workflow(
    wflow %>% add_model(tune_spec),
    lowest_rmse
  )

  # calculate importance ranking
  importance_ranking_lasso <- final_lasso %>%
    fit(df_training) %>%
    pull_workflow_fit() %>%
    tidy() %>%
    data.frame %>%
    arrange(-abs(estimate))

  # calculate mean importance for dummy variables of one feature
  factor_vars <- names(df_training %>% select_if(is.factor))
  res <- list()
  for (factor_var in factor_vars) {
    res[[factor_var]] <- importance_ranking_lasso %>%
      filter(grepl(factor_var, term)) %>%
      summarise(mean(abs(estimate))) %>%
      pull()
  }
  results_cat <- data.frame(term = names(res), estimate = unlist(res), row.names = NULL)


  # get importance for numeric variables
  numeric_vars <- names(df_training %>% select_if(is.numeric) %>% select(-Qs_rel))
  results_num <- importance_ranking_lasso %>%
    select(term, estimate) %>%
    dplyr::filter(term %in% numeric_vars) %>%
    mutate(estimate = abs(estimate))

  # combine both to cleaned ranking
  importance_ranking_lasso_cleaned <- rbind(results_cat, results_num) %>%
    arrange(-abs(estimate)) %>%
    mutate(term = factor(term, levels = rev(term)))


  # plot ranking
  ggplot(importance_ranking_lasso_cleaned, aes(x = estimate, y = term)) +
    geom_col(fill = "deepskyblue3", width = 0.5) +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    labs(y = NULL, x = "importance") +
    sema.berlin.utils::my_theme(legend.position = "top")

  ggsave("lasso_variable_importance_dummies_cleaned_26vars.png", dpi = 600, width = 8, height = 7)
  save_data(importance_ranking_lasso_cleaned, getwd(), "importance_ranking_lasso_dummies_cleaned_26_vars")


  # importance scores from vip package
  library(vip)

  final_lasso %>%
    fit(df_training) %>%
    pull_workflow_fit() %>%
    vi(lambda = lowest_rmse$penalty) %>%
    mutate(
      Importance = abs(Importance),
      Variable = forcats::fct_reorder(Variable, Importance)
    ) %>%
    ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
    geom_col() +
    scale_x_continuous(expand = c(0, 0)) +
    labs(y = NULL) +
    sema.berlin.utils::my_theme(legend.position = "top")

  ggsave("lasso_variable_importance.png", dpi = 600, width = 10, height = 20)

}
