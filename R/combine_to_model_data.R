# combine to model data --------------------------------------------------------

combine_to_model_data <- function(df_pump_tests_tidy,
                                  well_feature_data) {

  # join with feature table
  df <- df_pump_tests_tidy %>%
    dplyr::left_join(well_feature_data, by = "well_id")

  # handle inliner
  df <- handle_inliner(df)

  # remove invalid Qs data
  df <- df %>% dplyr::filter(.data$Qs_rel >= 0 & !is.na(.data$Qs_rel))

  # remove construction_date and operational_start.date as years are used
  df <- df %>% dplyr::select(- c(.data$construction_date, .data$operational_start.date))

  # fill up NA values
  df <- fill_up_na_with_median_from_lookup(df, well_feature_data)

  df %>% droplevels()
}
