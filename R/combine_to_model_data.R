# combine to model data --------------------------------------------------------

combine_to_model_data <- function(df_Qs = df_pump_tests_tidy,
                               well_feature_data = well_feature_data) {

  # join with feature table
  df <- df_Qs %>%
    dplyr::left_join(df_well_features, by = "well_id")

  # handle inliner
  df <- handle_inliner(df)

  # remove invalid Qs data
  df <- df %>% dplyr::filter(Qs_rel >= 0 & !is.na(Qs_rel))

  # remove construction_date and operational_start.date as years are used
  df <- df %>% dplyr::select(- c(construction_date, operational_start.date))

  # fill up NA values
  df <- fill_up_na_with_median_from_lookup(df, df_well_features)

  df %>% droplevels()
}
