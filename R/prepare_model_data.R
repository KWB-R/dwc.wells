# prepare model data -----------------------------------------------------------

prepare_model_data <- function(df_Qs = df_pump_tests_tidy,
                               df_well_features = df_well_features) {

  # join with feature table
  df <- df_Qs %>%
    dplyr::left_join(df_well_features, by = "well_id")

  # handle inliner
  df <- dwc.wells:::handle_inliner(df)

  # remove invalid Qs data
  df <- df %>% dplyr::filter(Qs_rel >= 0 & !is.na(Qs_rel))

  # remove construction_date and operational_start.date as years are used
  df <- df %>% dplyr::select(- construction_date, operational_start.date)

  # fill up NA values
  df <- fill_up_na_with_median(df, df_well_features)

  df %>% droplevels()
}
