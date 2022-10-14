# prepare well data ------------------------------------------------------------

#' Prepare Well Data
#'
#' @param path path
#' @param renamings renamings
#'
#' @return prepared well data
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom dplyr across all_of filter mutate na_if
#' @importFrom tidyr starts_with
prepare_well_data  <- function(path, renamings) {

  # read data from tsv and filter Vertikalfilterbrunnen
  df_wells <- read_csv(path, skip = 9) %>%
    remove_column_with_duplicated_name() %>%
    select_rename_cols(renamings$main, "old_name", "new_name_en") %>%
    dplyr::filter(grepl("V$", .data$well_name))


  # check for duplicates
  if (FALSE) {
    sum(duplicated(df_wells$site_id)) # duplicates: rehabilitated wells get site id 11
    sum(duplicated(df_wells$well_id)) # no duplicates
    sum(duplicated(df_wells$drilling_id)) # no duplicates
  }


  # assign data type "date"
  date_cols <- c("construction_date", "operational_start.date",
                 "operational_state.date", "inliner.date")
  df_wells <- df_wells %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(date_cols), .fns = as.Date, format = "%Y-%m-%d"),
                  monitoring.date = as.Date(.data$monitoring.date, format = "%d.%m.%Y"))


  # remove outliers in dates and numerical data
  df_wells <- df_wells %>%
    dplyr::mutate(
      # false dates
      dplyr::across(dplyr::all_of(date_cols), .fns = dplyr::na_if, "1899-12-30"),
      # specific capacity at operational start
      operational_start.Qs = dplyr::na_if(.data$operational_start.Qs, 469),
      admissible_discharge = dplyr::na_if(.data$admissible_discharge, 0),
      n_screens = dplyr::na_if(.data$n_screens, 0)
    )


  # replace missing construction dates with operational_start.date
  cd_missing <- is.na(df_wells$construction_date)
  df_wells[cd_missing, "construction_date"] <- df_wells[cd_missing, "operational_start.date"]


  # calculate filter length and absolute well depth
  df_wells$filter_length <- df_wells$screen_bottom_level - df_wells$screen_top_level
  df_wells$well_depth <- df_wells$well_top_level - df_wells$well_depth_level


  # recalcuate Qs, as there are 97 wells with no Qs but with Q, W_dynamic, W_static
  df_wells <- df_wells %>%
    dplyr::mutate(
      operational_start.Qs = .data$operational_start.Q /
        (.data$operational_start.W_dynamic - .data$operational_start.W_static))


  # calculate years from date
  df_wells <- df_wells %>%
    dplyr::mutate(
      construction_year = lubridate::year(.data$construction_date),
      operational_start.year = lubridate::year(.data$operational_start.date)
    )


  # clean categorical variables
  df_wells <- df_wells %>%
    dplyr::mutate(
      dplyr::across(tidyr::starts_with("aquifer"), ~dplyr::na_if(., "nicht bekannt"))
      )


  # group categorical variables according to lookup table
  vars <- c("screen_material", "casing_material", "well_function", "waterworks")
  for (var in vars) {
    df_wells[[var]] <- rename_values(df_wells[[var]], renamings[[var]])
  }

  # create new categorical variables
  df_wells <- df_wells %>%
    dplyr::mutate(
      inliner = factor(ifelse(!is.na(.data$inliner.date), "Yes", "No"),
                       levels = c("Yes", "No")),
      well_gallery = substr(.data$well_name, 1, 7)
    )


  # assign data type "factor"
  factor_cols <- c("well_function", "operational_state", "aquifer_confinement",
                   "aquifer_coverage", "casing_material", "screen_material",
                   "waterworks", "well_gallery")
  df_wells <- df_wells %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(factor_cols), .fns = tidy_factor))

  df_wells

}
