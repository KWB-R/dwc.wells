# combine_pump_test_and_Q_monitoring_data --------------------------------------

#' Combined Pumptest and Q Monitoring Dataset
#'
#' @param df_pump_tests_tidy df_pump_tests_tidy
#' @param df_Q_monitoring df_Q_monitoring
#' @param pump_test_vars default: \code{\link{get_pump_test_vars}}
#' @return combined pumptest and Q monitoring dataset
#' @export
#'
combine_pump_test_and_Q_monitoring_data <- function(df_pump_tests_tidy,
                                                    df_Q_monitoring,
                                                    pump_test_vars = get_pump_test_vars()
                                                    ) {

  df_Qs_all <- df_pump_tests_tidy %>%
    dplyr::bind_rows(df_Q_monitoring) %>%
    dplyr::arrange(.data$well_id, .data$date) %>%
    dplyr::select(c(dplyr::all_of(pump_test_vars), "operational_start.date", "W_static.origin")) %>%
    dplyr::filter(!is.na(.data$date)) %>%
    dplyr::ungroup()


  # complete data
  df_Qs_all <- df_Qs_all %>%
    tidyr::fill(.data$n_rehab) %>%
    tidyr::fill(.data$last_rehab.date) %>%
    dplyr::mutate(key = dplyr::if_else(!is.na(.data$W_static.origin),
                                       "quantity measurements",
                                       .data$key),
                  key2 = forcats::fct_collapse(
                    .data$key,
                    'pump tests' = c("operational_start", "pump_test_1", "pump_test_2")
                  ),
                  W_static.origin = tidyr::replace_na(.data$W_static.origin, "measured"),
                  days_since_operational_start = as.integer(
                    difftime(.data$date, .data$operational_start.date, units = "days")
                  )) %>%
    dplyr::group_by(.data$well_id, .data$n_rehab) %>%
    dplyr::mutate(time_since_rehab_days =  dplyr::if_else(
      is.na(.data$time_since_rehab_days),
      as.integer(.data$date - min(.data$last_rehab.date)),
      .data$time_since_rehab_days
    )) %>%
    dplyr::ungroup()

  df_Qs_all

  }
