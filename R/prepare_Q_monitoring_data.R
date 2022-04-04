#' Get W_static measurement data from Neubaupumpversuche, Kurzpumpversuche and other sources
#'
#' @param path path to static water level data (csv-file)
#' @param renamings list with renamings
#' @param df_wells data frame with prepared well data
#'
#' @export
#' @importFrom readr read_csv
#' @importFrom dplyr select mutate filter
#' @importFrom tidyr pivot_longer
get_W_static_data <- function(path, renamings, df_wells) {

  # get static water level data from df_wells (Neubaupumpversuche)
  df_W_static_1 <- df_wells %>%
    dplyr::select(.data$site_id,
                  .data$well_id,
                  .data$operational_start.date,
                  .data$operational_start.W_static,
                  .data$monitoring.date,
                  .data$monitoring.W_static) %>%
    tidyr::pivot_longer(-c(.data$site_id, .data$well_id),
                        names_sep = "\\.",
                        names_to = c("origin", ".value")) %>%
    dplyr::mutate(W_static = dplyr::na_if(.data$W_static, 0)) %>%
    dplyr::filter(!is.na(.data$W_static))

  # import other static water level data provided by Sebastian Schimmelpfennig
  # origin: H2O2-Messungen, Kurzpumpversuche, Ergiebigkeitsmessungen
  df_W_static_2 <- readr::read_csv(path, skip = 30) %>%
    select_rename_cols(renamings$main, "old_name", "new_name_en") %>%
    dplyr::mutate(date = as.Date(.data$date, format = "%Y-%m-%d")) %>%
    dplyr::select(.data$site_id,
                  .data$well_id,
                  .data$origin,
                  .data$date,
                  .data$W_static)

  # combine both data sources
  df_W_static <- dplyr::bind_rows(list(df_W_static_1, df_W_static_2)) %>%
    tidyr::drop_na(.data$site_id,
                   .data$date,
                   .data$W_static) %>% # one date missing
    dplyr::arrange(.data$site_id,
                   .data$date) %>%
    dplyr::rename(W_static.origin = .data$origin)

  df_W_static
}


# prepare_Q_monitoring_data ----------------------------------------------------

prepare_Q_monitoring_data <- function(df_wells, path_quantity,
                                      path_W_static, renamings) {

  # read quantity measurement data
  df_Q <- readr::read_csv(path_quantity, skip = 26) %>%
    select_rename_cols(renamings$main, "old_name", "new_name_en") %>%
    dplyr::mutate(date = as.Date(.data$date)) %>%
    dplyr::mutate(date = dplyr::na_if(.data$date, "1899-12-30 00:00:00")) %>%
    tidyr::drop_na(-.data$W_static) %>%
    dplyr::distinct(.keep_all = TRUE)

  # get data for static water level measurements
  df_W_static <- get_W_static_data(path_W_static, renamings, df_wells)

  summary <- df_W_static %>%
    dplyr::group_by(.data$well_id) %>%
    dplyr::summarise(n_valid = sum(!is.na(.data$W_static))) %>%
    dplyr::arrange(dplyr::desc(.data$n_valid))

  # combine quantity data and extra static water level data
  df_Q_W <- df_Q %>%
    dplyr::mutate(W_static.origin = ifelse(
      !is.na(.data$W_static), "quantity_measurements", NA
    )) %>%
    dplyr::bind_rows(dplyr::select(df_W_static, -.data$well_id)) %>%
    # remove duplicates
    dplyr::distinct(.data$site_id, .data$date, .keep_all = TRUE) %>%
    dplyr::arrange(.data$site_id, .data$date)

  frequency_table(duplicated((df_Q_W[, c("site_id", "date")])))
  a <- df_Q_W[duplicated(df_Q_W[, c("site_id", "date")]), ]

  print(frequency_table(df_Q_W[!is.na(df_Q_W$W_static), "W_static.origin"]))

  # remove rows for wells with no static water level data at all
  df_Q_W <- df_Q_W %>%
    dplyr::group_by(.data$site_id) %>%
    dplyr::filter(!all(is.na(.data$W_static)))


  # check number of non-NA values per site
  a <- df_Q_W %>% dplyr::group_by(.data$site_id) %>%
    dplyr::summarise(n_valid = sum(!is.na(.data$W_static))) %>%
    data.frame()

  b <- a %>%
    dplyr::count(.data$n_valid >= 1) %>%
    dplyr::pull(.data$n)

  cat("Number of wells with at least one W_static measurement:", b, "\n")

  # interpolate and fill up static water level
  df_Q_W_new <- interpolate_and_fill(
    df_Q_W, x_col = "date", y_col = "W_static",
    group_by_col = "site_id", origin_col = "W_static.origin"
  )

  # show origin of data
  frequency_table(df_Q_W_new[!is.na(df_Q_W_new$W_static), "W_static.origin"])

  # group types for W_static
  df_Q_W_new <- df_Q_W_new %>%
    dplyr::mutate(W_static.origin =
                    forcats::fct_collapse(
                      .data$W_static.origin,
                      measured = setdiff(unique(.data$W_static.origin),
                                         c("interpolated", "filled up"))) %>%
                    forcats::fct_relevel("measured", "interpolated", "filled up"),
                  full_data_set = !is.na(.data$Q) & !is.na(.data$W_dynamic)
    )


  # clean outliers and remove NA
  df_Q_W_new <- df_Q_W_new %>%
    dplyr::mutate(Q = ifelse(.data$Q > 1000 | .data$Q == 0,
                             NA,
                             .data$Q),
                  W_dynamic = ifelse(.data$W_dynamic > 50 | .data$W_dynamic == 0,
                                     NA,
                                     .data$W_dynamic)
    ) %>%
    tidyr::drop_na(.data$Q, .data$W_dynamic) %>%
    dplyr::select(-.data$W_static.incomplete)


  # calculate Qs and remove negative values
  df_Q_W_new <- df_Q_W_new %>%
    dplyr::mutate(Qs = .data$Q / (.data$W_dynamic - .data$W_static))


  # join with Qs from operational start and calculate Qs_rel
  df_Q_W_new <- df_Q_W_new %>%
    dplyr::left_join(df_wells[, c("site_id", "well_id",
                                  "operational_start.date",
                                  "operational_start.Qs",
                                  "admissible_discharge")],
                     by = "site_id") %>%
    # calculate Qs_rel
    dplyr::mutate(Qs_rel =  .data$Qs / .data$operational_start.Qs,
                  ratio_Q_admissible_discharge = .data$Q / .data$admissible_discharge) %>%
    dplyr::select(-.data$operational_start.Qs)

  df_Q_W_new

}
