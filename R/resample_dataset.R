#' Resample Dataset
#'
#' @param model_data (reduced) model data set as retrieved by \code{\link{model_data_reduced}}
#' @param method resampling method. Either "random" or "by_well" (default: "random")
#' @param prop The proportion of data to be retained for modeling/analysis (default:
#' 0.8), i.e. 80% of the dataset
#'
#' @return list with elements "training" and "testing" datasets
#' @export
#' @importFrom rsample initial_split testing training
#' @importFrom tibble as_tibble
resample_dataset <- function(model_data,
                             method = "random",
                             prop = 0.8) {

stopifnot(tolower(method) %in% c("random", "by_well"))
stopifnot(all(c("well_id", "Qs_rel") %in% names(model_data)))

if (method == "random") {
  # for regression
  data_split <- rsample::initial_split(model_data %>%
                                         dplyr::select(- .data$well_id),
                                       prop = prop,
                                       strata = .data$Qs_rel)
  df_training <- data_split %>% rsample::training()
  df_test <- data_split %>% rsample::testing()
}
# version 2: splitting per well ids
if (method == "by_well") {
  well_ids <- unique(model_data$well_id)
  train_ids <- sample(well_ids, prop * length(well_ids))
  test_ids <- setdiff(well_ids, train_ids)
  df_training <- model_data %>%
    dplyr::filter(well_id %in% train_ids) %>%
    dplyr::select(-well_id)
  df_test <- model_data %>%
    dplyr::filter(well_id %in% test_ids) %>%
    dplyr::select(-well_id)

}

list(training = tibble::as_tibble(df_training),
     testing = tibble::as_tibble(df_test)
     )
}
