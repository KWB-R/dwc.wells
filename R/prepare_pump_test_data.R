# prepare_pump_test_data_1 -----------------------------------------------------

#' Prepare pump test data in wide format
#' Steps: i) read, rename and clean data, ii) correct wrong pump test dates,
#' iii) fill up missing pump test dates, iv) get information for replaced wells,
#' v) calculate Qs and Qs_rel, vi) determine action type, vii) select columns
#'
#' @param path path to pump test data
#' @param renamings list with renamings
#' @param df_wells prepared data frame with well characteristics
#'
#' @export
#'
prepare_pump_test_data_1 <- function(path, renamings, df_wells) {

  # read, rename and clean data ---

  df_pump_tests <- read_csv(path, skip = 2) %>%
    select_rename_cols(renamings$main, "old_name", "new_name_en") %>%
    # filter data for site id 11 (not unique, used for rehabilitated wells)
    dplyr::filter(site_id != 11) %>%
    # delete rows with same site_id, pump test_1.date and pump_test_2.date
    dplyr::distinct_at(c("site_id", "pump_test_1.date", "pump_test_2.date"),
                       .keep_all = TRUE) %>%
    # assign date format to dates
    dplyr::mutate(pump_test_1.date = as.Date(pump_test_1.date, format = "%Y-%m-%d"),
                  pump_test_2.date = as.Date(pump_test_2.date, format = "%Y-%m-%d")) %>%
    # delete row if both values are NA
    dplyr::filter(!(is.na(pump_test_1.date) & is.na(pump_test_2.date))) %>%
    # delete data with pump test date in the future
    dplyr::filter(pump_test_1.date < Sys.Date()| pump_test_2.date < Sys.Date())


  # swap pump test dates 1 and 2 if pump_test_2.date < pump_test_1.date --------

  # check, how many rows have dates in wrong order
  cond <- swapped_dates(df_pump_tests)
  check_swapped_dates(cond, df_pump_tests)

  # swap dates
  df_pump_tests <- df_pump_tests %>%
    dplyr::mutate(
      pump_test_1.date_tmp = dplyr::if_else(cond, pump_test_2.date, pump_test_1.date),
      pump_test_2.date_tmp = dplyr::if_else(cond, pump_test_1.date, pump_test_2.date)
    ) %>%
    dplyr::mutate(pump_test_1.date = pump_test_1.date_tmp,
                  pump_test_2.date = pump_test_2.date_tmp) %>%
    dplyr::select(-c(pump_test_1.date_tmp, pump_test_2.date_tmp))

  # check again, how many dates are in wrong order
  cond <- swapped_dates(df_pump_tests)
  check_swapped_dates(cond, df_pump_tests)


  # fill up pump test dates and calculate action date --------------------------

  df_pump_tests <- df_pump_tests %>%
    # add date column not containing NAs (required for creating an "action_id")
    dplyr::mutate(
      interval_days = dplyr::if_else(
        !is.na(pump_test_1.date) & !is.na(pump_test_2.date),
        real_interval(pump_test_2.date, pump_test_1.date),
        default_interval(pump_test_2.date, pump_test_1.date, func = mean)
      ),
      interval_type = dplyr::if_else(
        !is.na(pump_test_1.date) & !is.na(pump_test_2.date), "real", "default"
      ),
      pump_test_1.date = dplyr::if_else(
        is.na(pump_test_1.date) & !is.na(pump_test_2.date),
        pump_test_2.date - interval_days,
        pump_test_1.date
      ),
      pump_test_2.date = dplyr::if_else(
        is.na(pump_test_2.date) & !is.na(pump_test_1.date),
        pump_test_1.date + interval_days,
        pump_test_2.date
      ),
      action_date = pump_test_1.date + ceiling(interval_days / 2)
    )


  # correct ids for replaced wells ---------------------------------------------

  # join relevant ids and construction date from well data
  cols <-  c("site_id", "well_id", "well_id_replaced", "construction_date")
  df_pump_tests <- df_pump_tests %>% dplyr::left_join(df_wells[, cols], by = "site_id")


  # use well id of replaced well if pump test date < construction date
  if (FALSE) {
    ptd1 <- df_pump_tests$pump_test_1.date
    ptd2 <- df_pump_tests$pump_test_2.date
    cd <- df_pump_tests$construction_date
    cond <- (ptd1 < cd & ptd2 < cd | ptd1 < cd & is.na(ptd2) | is.na(ptd1) & ptd2 < cd) & !is.na(cd)
  }

  cond <- which(df_pump_tests$pump_test_1.date < df_pump_tests$construction_date |
                  df_pump_tests$pump_test_2.date < df_pump_tests$construction_date)

  df_pump_tests[cond, "well_id"] <- df_pump_tests[cond, "well_id_replaced"]

  # delete unrequired columns, use 'well_id' as join column from now on
  df_pump_tests <- df_pump_tests %>%
    dplyr::select(-c("site_id", "well_id_replaced", "construction_date"))


  # calculate Qs and Qs rel ----------------------------------------------------

  df_wells_operational_start <- df_wells %>%
    dplyr::select("well_id", tidyselect::starts_with("operational_start."))

  df_pump_tests <- df_pump_tests %>%
    # get well characteristics to calculate Qs_rel
    dplyr::inner_join(df_wells_operational_start, by = "well_id") %>%
    # discard data without Qs as there will be no reference for pump test data
    dplyr::filter(!is.na(operational_start.Qs)) %>%
    # calculate Qs and Qs_rel for pump tests 1 and 2
    dplyr::mutate(pump_test_1.Qs = pump_test_1.Q /
                    (pump_test_1.W_dynamic - pump_test_1.W_static),
                  pump_test_1.Qs_rel =  pump_test_1.Qs / operational_start.Qs * 100,
                  pump_test_2.Qs = pump_test_2.Q /
                    (pump_test_2.W_dynamic - pump_test_2.W_static),
                  pump_test_2.Qs_rel =  pump_test_2.Qs / operational_start.Qs * 100
    )


  # derive action type ---------------------------------------------------------

  df_pump_tests <- df_pump_tests %>%
    # give action id
    dplyr::arrange(well_id, action_date) %>%
    dplyr::group_by(well_id) %>%
    dplyr::mutate(action_id = dplyr::row_number()) %>%
    # check if pump test is associated with regeneration (three types)
    dplyr::mutate(pump_test_2.well_rehab = (well_rehab.general + well_rehab.shock +
                                              well_rehab.hydropulse) != 0,
                  pump_test_2.substitute_pump = substitute_pump != 0,
                  pump_test_2.pressure_sleeve =  pressure_sleeve != 0) %>%
    dplyr::mutate(pump_test_2.comment_liner = ifelse(
      grepl("Liner|liner|Inliner|inliner|Lining|lining", well_rehab.comment), TRUE, FALSE
    ))


  # select relevant columns ----------------------------------------------------

  df_pump_tests <- df_pump_tests %>%
    # select important variables
    dplyr::select("well_id",
                  "action_id",
                  "action_date",
                  tidyselect::starts_with("interval_"),
                  tidyselect::starts_with("operational_start"),
                  tidyselect::starts_with("pump_test")
                  #-tidyselect::ends_with(c("Q", "W_static", "W_dynamic"))
    )

  df_pump_tests

}


# prepare_pump_test_data_2 -----------------------------------------------------

#' reformats untidy pump test data from wide into long format
#'
#' @param df_pump_tests_untidy pump test data in wide format
#' @param df_wells prepared data frame with well characteristics
#'
#' @export
#'
prepare_pump_test_data_2 <- function(df_pump_tests_untidy, df_wells) {


  # tidy data (to long format) -------------------------------------------------

  cols_to_longer <- df_pump_tests_untidy %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyr::starts_with(c("operational_start", "pump_test"))) %>%
    names()

  df_pump_tests_tidy <- df_pump_tests_untidy %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), as.character)) %>%
    tidyr::pivot_longer(cols = tidyselect::all_of(cols_to_longer),
                        names_to = c("key", "parameter"),
                        names_sep = "\\.",
                        values_to = "value") %>%
    dplyr::filter(!is.na(value)) %>%
    tidyr::pivot_wider(names_from = "parameter",
                       values_from = "value") %>%
    #dplyr::select(- year) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(c("well_id", "action_id")), as.integer)) %>%
    dplyr::mutate(dplyr::across(tidyselect::matches("date"), as.Date)) %>%
    dplyr::mutate(dplyr::across(tidyselect::starts_with("Q"), as.double)) %>%
    dplyr::mutate(dplyr::across(tidyselect::starts_with("W_"), as.double)) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(c("well_rehab",
                                                     "substitute_pump",
                                                     "pressure_sleeve",
                                                     "comment_liner")), as.logical)) %>%
    dplyr::filter(!(key == "operational_start" & action_id != 1)) %>%
    dplyr::mutate(action_id = dplyr::if_else(key == "operational_start",
                                             0L,
                                             action_id),
                  action_date = dplyr::if_else(key == "operational_start",
                                               date,
                                               action_date),
                  Qs_rel = dplyr::if_else(key == "operational_start",
                                          100,
                                          Qs_rel)
    ) %>%
    dplyr::arrange(well_id, action_id) %>%

    # join  dates of operational start to calculate time differences
    dplyr::left_join(df_wells %>%
                       dplyr::select(well_id, operational_start.date),
                     by = "well_id") %>%
    dplyr::mutate(days_since_operational_start =
                    as.integer(
                      difftime(date, operational_start.date, units = "days")
                    )) %>%
    dplyr::filter(days_since_operational_start >= 0) %>%
    dplyr::group_by(well_id) %>%
    dplyr::mutate(n_rehab = as.integer(cumsum_no_na(well_rehab)),
                  n.substitute_pump = cumsum_no_na(substitute_pump),
                  n.pressure_sleeve = cumsum_no_na(pressure_sleeve),
                  n.comment_liner = cumsum_no_na(comment_liner)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(well_id, n_rehab, date) %>%
    dplyr::group_by(well_id, n_rehab) %>%
    dplyr::mutate(
      last_rehab.date = min(action_date),
      time_since_rehab_days =  dplyr::if_else(
        n_rehab > 0,
        as.integer(date - last_rehab.date),
        days_since_operational_start
      ))


  # recalculate days since operational start and days since last rehab into
  # new variables well_age_years and time_since_rehab_years

  df_pump_tests_tidy <- df_pump_tests_tidy %>%
    dplyr::mutate(well_age_years = days_since_operational_start / 365.25,
                  time_since_rehab_years = time_since_rehab_days / 365.25)

  df_pump_tests_tidy %>%
    dplyr::select(pump_test_vars) %>%
    dplyr::filter(!is.na(Qs_rel)) %>%
    dplyr::ungroup()
}


# prepare_pump_test_data -------------------------------------------------------

#' prepare pump test data with one row per Qs-measurement + rehab history
#'
#' @param path path to pump test data
#' @param renamings list with renamings
#' @param df_wells prepared data frame with well characteristics
#'
#' @export
#'
#'
prepare_pump_test_data <- function(path, renamings, df_wells) {

  prepare_pump_test_data_1(path, renamings, df_wells) %>%
    prepare_pump_test_data_2(df_wells)

}
