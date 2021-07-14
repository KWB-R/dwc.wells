# combine_pump_test_and_Q_monitoring_data --------------------------------------

combine_pump_test_and_Q_monitoring_data <- function(df_pump_tests_tidy, df_Q_monitoring) {

  df_Qs_all <- df_pump_tests_tidy %>%
    dplyr::bind_rows(df_Q_monitoring) %>%
    dplyr::arrange(well_id, date) %>%
    dplyr::select(c(dplyr::all_of(pump_test_vars), "operational_start.date", "W_static.origin")) %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::ungroup()


  # complete data
  df_Qs_all <- df_Qs_all %>%
    tidyr::fill(n_rehab) %>%
    tidyr::fill(last_rehab.date) %>%
    dplyr::mutate(key = dplyr::if_else(!is.na(W_static.origin),
                                       "quantity measurements",
                                       key),
                  key2 = forcats::fct_collapse(
                    key,
                    'pump tests' = c("operational_start", "pump_test_1", "pump_test_2")
                  ),
                  W_static.origin = tidyr::replace_na(W_static.origin, "measured"),
                  days_since_operational_start = as.integer(
                    difftime(date, operational_start.date, units = "days")
                  )) %>%
    dplyr::group_by(well_id, n_rehab) %>%
    dplyr::mutate(time_since_rehab_days =  dplyr::if_else(
      is.na(time_since_rehab_days),
      as.integer(date - min(last_rehab.date)),
      time_since_rehab_days
    )) %>%
    dplyr::ungroup()

  df_Qs_all

  }
