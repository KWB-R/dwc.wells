#' Helper Function: select first elements per well_id
#'
#' @param model_data (reduced) model data set as retrieved by \code{\link{model_data_reduced}}
#'
#' @return groups data by column "well_id" and selects for all remaining columns
#' first element
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom dplyr across first group_by summarise
#' @importFrom tidyselect everything
select_first_elements_per_well_id <- function(model_data) {
  model_data %>%
    dplyr::group_by(.data$well_id) %>%
    dplyr::summarise(dplyr::across(.cols = tidyselect::everything(),
                                   dplyr::first)
                     )
}


#' Prepare 'Do Nothing' Simulation Data Frame
#'
#' @description Prepares a simulation data frame with the regenerations that
#' were performed in the past but without assumung new ones in the future
#' @param model_data (reduced) model data set as retrieved by \code{\link{model_data_reduced}},
#' will be merged with but filtering out duplicated columns (i.e. 'well_age_years',
#' 'n_rehab' and 'time_since_rehab_years') and name of "predicted_parameter"
#' @param rehabs rehabiliations dataset as retrieved by \code{\link{rehabs}}
#' @param operational_start operational start dataset as retrieved by
#' \code{\link{operational_start}}
#' @param sim_interval simulation interval in years at which predictions should be
#' performed (default: 1L)
#' @param sim_period simulation period in years since operational start of each
#' production well (default: 60L)
#' @param sim_reference_date (default: as.Date("2021-05-01")), used to generate
#' fill up column 'type' with 'past' (< sim_reference_date) and 'future'
#' (>= sim_reference_date)
#' @param predicted_parameter name of predicted parameter (default: "Qs_rel")
#' @return simulation data frame
#' @export
#' @importFrom dplyr arrange group_by left_join mutate rename select ungroup
#' @importFrom tidyr expand fill
#' @importFrom lubridate %m+% time_length years
prepare_simulation_donothing_df <- function(
    model_data = dwc.wells::model_data_reduced,
    rehabs = dwc.wells::rehabs,
    operational_start = dwc.wells::operational_start,
    sim_interval = 1L,
    sim_period = 60L,
    sim_reference_date = as.Date("2021-05-01"),
    predicted_parameter = "Qs_rel") {

  # get well id and age combinations
  well_ages <- operational_start %>% # get well id and operational start date
    dplyr::group_by(.data$well_id) %>%
    tidyr::expand(well_age_years = seq.int(0, sim_period, sim_interval))

  # combine both and calculate sim dates
  well_ages_dates <- operational_start %>%
    dplyr::left_join(well_ages, by = "well_id") %>%
    dplyr::mutate(sim_date = .data$operational_start.date %m+% lubridate::years(.data$well_age_years)) %>%
    dplyr::select(-.data$operational_start.date)

  # tmp data 1
  sim_data_tmp1 <- rehabs %>%
    dplyr::rename(sim_date = .data$rehab_date) %>%
    dplyr::mutate(well_age_years = NA) %>%
    dplyr::select(.data$well_id, .data$sim_date, .data$well_age_years, .data$n_rehab)

  # tmp data 2
  sim_data_tmp2 <- well_ages_dates %>%
    dplyr::mutate(n_rehab = NA) %>%
    dplyr::select(.data$well_id, .data$sim_date, .data$well_age_years, .data$n_rehab)

  # create sim data
  sim_data_base <- rbind(sim_data_tmp1, sim_data_tmp2) %>%
    dplyr::arrange(.data$well_id, .data$sim_date) %>%
    dplyr::left_join(rehabs,
                     by = c("well_id", "n_rehab")) %>%
    dplyr::group_by(.data$well_id) %>%
    tidyr::fill(c(.data$n_rehab, .data$rehab_date), .direction = "down") %>%
    dplyr::mutate(time_since_rehab_years = lubridate::time_length(.data$sim_date - .data$rehab_date,
                                                                  "years")) %>%
    dplyr::mutate(n_rehab = ifelse(is.na(.data$n_rehab), 0, .data$n_rehab),
                  time_since_rehab_years = ifelse(is.na(.data$time_since_rehab_years),
                                                  .data$well_age_years,
                                                  .data$time_since_rehab_years)) %>%
    dplyr::mutate(type = ifelse(.data$sim_date < sim_reference_date, "past", "future")) %>%
    dplyr::filter(!is.na(.data$well_age_years)) %>%
    dplyr::select(-.data$rehab_date) %>%
    dplyr::ungroup() %>%
    dplyr::relocate(.data$type,
                    .after = .data$sim_date)

  ## Select only different columns in 'model_data' (besides `well_id` required for
  ## merging)
  non_duplicated_cols <- names(model_data)[!names(model_data) %in% names(sim_data_base)]
  non_duplicated_feature_cols <- non_duplicated_cols[!non_duplicated_cols %in% predicted_parameter]

  missing_feature_cols <- c("well_id", non_duplicated_feature_cols)

  model_feature_data_first_values <- model_data[,missing_feature_cols] %>%
    select_first_elements_per_well_id()

  # create sim data
  sim_data_base %>%
    dplyr::left_join(model_feature_data_first_values, by = "well_id")
}
