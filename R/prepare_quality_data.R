#' Prepare Quality Data
#'
#' @param path path
#' @param renamings renamings
#'
#' @return prepared quality day
#' @export
#'
#' @importFrom stats sd
prepare_quality_data <- function(path, renamings) {

  # read data
  df_quality <- read_ms_access_mri(path, "DB2LABOR_Daten") %>%
    select_rename_cols(renamings$main, "old_name", "new_name_en")


  # Select and rename quality parameter given in renamings file 'quality.csv'
  df_quality <- df_quality %>%
    dplyr::filter(quality.parameter %in% renamings$quality$old_name) %>%
    dplyr::mutate(quality.parameter = rename_values(quality.parameter,
                                                    renamings$quality,
                                                    old_name_col = "old_name",
                                                    new_name_col = "new_name_en"))

  # adapt date format
  df_quality <- df_quality %>%
    dplyr::mutate(quality.date = as.Date(quality.date, format = "%Y-%m-%d"))

  # transform concentration units from "mµg/l" to "mg/l"
  indices <- df_quality$quality.unit == "\u00B5g/l"
  df_quality[indices, "quality.value"] <- df_quality[indices, "quality.value"] * 10^-3
  df_quality[indices, "quality.unit"] <- "mg/l"

  # delete measurements in unit "mg/kg"
  df_quality <- df_quality %>%
    dplyr::filter(quality.unit != "mg/kg") %>%
    dplyr::filter(! (quality.parameter == "DR" & quality.unit == "%"))

  # overview quality data: parameters and units
  cat("Overview of quality data (data count per parameter and unit):\n\n")
  print(check_n_parameters_and_units(df_quality))


  # aggregate / clean data
  df_quality_agg <- df_quality %>%
    dplyr::mutate(quality.date = as.Date(quality.date, format = "%Y-%m-%d")) %>%
    dplyr::group_by(well_id, quality.parameter) %>%
    dplyr::summarise(quality.median = median(quality.value, na.rm = TRUE),
                     quality.std_dev = stats::sd(quality.value, na.rm = TRUE),
                     quality.number = dplyr::n()) %>%
    dplyr::filter(quality.parameter != "LOI") %>%  # discard Gluehverlust data (only 11 wells with observations)
    dplyr::select(well_id, quality.parameter, quality.median) %>%
    tidyr::pivot_wider(names_from = quality.parameter,
                       values_from = quality.median,
                       id_cols = "well_id") %>%
    data.frame()


  # append quality to colnames
  colnames(df_quality_agg)[-1] <- paste0("quality.", colnames(df_quality_agg)[-1])

  df_quality_agg

}

# check_n_parameters_and_units -------------------------------------------------

check_n_parameters_and_units <- function(df_quality) {

  # check if only one unit per parameter is used
  if (FALSE) {
    df_quality %>% dplyr::group_by(quality.parameter) %>%
      dplyr::summarise(length(unique(quality.unit)))
  }

a <- list()
pars <- unique(df_quality$quality.parameter)

for (par in pars) {
  a[[par]] <- df_quality %>%
    dplyr::filter(quality.parameter == par) %>%
    dplyr::pull(quality.unit) %>%
    frequency_table() %>%
    dplyr::rename(unit = value)
}

data.frame(par = pars, dplyr::bind_rows(a))

}
