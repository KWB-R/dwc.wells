#' Get Predictions
#'
#' @param model trained ML model
#' @param sim_data as retrieved by \code{\link{prepare_simulation_donothing_df}}
#' @param predicted_parameter name of predicted parameter (default: "Qs_rel")
#' @return data frame with predictions
#' @export
#'
#' @importFrom stats predict
#' @importFrom dplyr bind_cols mutate
get_predictions <- function(model,
                            sim_data,
                            predicted_parameter = "Qs_rel") {

  sim_data_pred <- stats::predict(model, sim_data) %>%
    dplyr::mutate(.pred = ifelse(.data[[".pred"]] < 0,
                                 0,
                                 .data[[".pred"]]))

  names(sim_data_pred) <- predicted_parameter

  dplyr::bind_cols(sim_data, sim_data_pred) %>%
    dplyr::relocate(.data[[predicted_parameter]],
                    .after = .data$type)
}
