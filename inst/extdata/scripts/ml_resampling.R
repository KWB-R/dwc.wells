# load package, paths and variable sets from global.R --------------------------

source("inst/extdata/scripts/global.R")
library(tidymodels)
resampling <- "random"
#resampling <- "by_well"
set.seed(1)


# load data --------------------------------------------------------------------

# load(file.path(paths$data_prep_out, "model_data.RData"))
df <- model_data %>% select(Qs_rel, well_id, all_of(model_features))


# refine variable selection ----------------------------------------------------


# remove correlated and unimportant variables (see "03_variable_importance.R")

if (TRUE) {

  # remove correlated variables
  df <- df %>%
    select(-c(well_depth, quality.DR, quality.P_tot,
              volume_m3_d.sd, waterworks, surface_water))

  # remove unimportant variables
  df <- df %>%
    select(-c(n_screens, filter_length, quality.Cu, inliner))

  # remove well gallery (local variable which makes models not applicable
  # to new sites or well galleries)
  df <- df %>% select(-well_gallery)

}


# select only top 5 variables

if (FALSE) {
  df <- df %>% select(Qs_rel, all_of(top5_model_features))
}


# filter further 6 variables with little importance
if (FALSE) {
  df <- df %>%
    select(-c(drilling_method, aquifer_coverage, quality.NO3, diameter,
              quality.DO, quality.TSS))
}


# split / resample data --------------------------------------------------------


# version 1: random splitting of data points

if (resampling == "random") {

  # for regression
  data_split <- initial_split(df %>% select(-well_id),
                              prop = 0.8,
                              strata = Qs_rel)
  df_training <- data_split %>% training()
  df_test <- data_split %>% testing()

  # for classification
  df_training_cat <- df_training %>% mutate(Qs_rel = classify_Qs(Qs_rel))
  df_test_cat <- df_test %>% mutate(Qs_rel = classify_Qs(Qs_rel))
  data_split_cat <- initial_split(
    df %>% mutate(Qs_rel = classify_Qs(Qs_rel)) %>% select(-well_id),
    prop = 0.8,
    strata = Qs_rel
  )
}


# version 2: splitting per well ids

if (resampling == "by_well") {

  well_ids <- unique(df$well_id)
  train_ids <- sample(well_ids, 0.8 * length(well_ids))
  test_ids <- setdiff(well_ids, train_ids)
  df_training <- df %>% filter(well_id %in% train_ids) %>% select(-well_id)
  df_test <- df %>% filter(well_id %in% test_ids) %>% select(-well_id)

  df_training_cat <- df_training %>% mutate(Qs_rel = classify_Qs(Qs_rel))
  df_test_cat <- df_test %>% mutate(Qs_rel = classify_Qs(Qs_rel))

}

# save training and test data to model
usethis::use_data(df_training, compress = "xz", overwrite = TRUE)
usethis::use_data(df_test, compress = "xz", overwrite = TRUE)



# plot distribution of goal variable in training and test data ---------------

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

  ggsave("data_split_Qs_rel_distribution_resampling_by_well.png", dpi = 600, width = 4, height = 3)

}

