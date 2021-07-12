
# Paths and packages -----------------------------------------------------------

library(dwc.wells)
source("inst/extdata/config/path_list.R")
source("inst/extdata/config/variable_list.R")
paths <- kwb.utils::resolve(path_list)
renamings <- dwc.wells:::get_renamings(paths)


# get and prepare individual data sets -----------------------------------------

if (FALSE) {

  # 1. well characteristics
  df_wells <- dwc.wells:::prepare_well_data()
  save_data(df_wells, paths$data_out, "well_data")

  # 2. surface water information
  df_drilling <- dwc.wells:::prepare_drilling_data()
  save_data(df_drilling, paths$data_out, "drilling_data")

  # 3. drilling method information
  df_drilling_tech <- dwc.wells:::prepare_drilling_tech_data()
  save_data(df_drilling_tech, paths$data_out, "drilling_tech_data")

  # 4. water quality data
  df_quality_agg <- dwc.wells:::prepare_quality_data()
  save_data(df_quality_agg, paths$data_out, "quality_data")

  # 5. capacity measurements (virtual pump tests)
  df_Q_monitoring <- dwc.wells:::prepare_Q_monitoring_data(df_wells)
  save_data(df_Q_monitoring, paths$data_out, "Q_monitoring_data")

  # 6. pump test and rehab data
  df_wells_operational_start <- df_wells %>%
    dplyr::select("well_id", tidyselect::starts_with("operational_start."))
  df_pump_tests_tidy <- dwc.wells:::prepare_pump_test_data(df_wells_operational_start)
  save_data(df_pump_tests_tidy, paths$data_out, "pump_test_data")

}

if (FALSE) {

  frequency_table(ifelse(is.na(df_wells$well_id_replaced), 1, 2)) # count well generation

  df_quality_agg_long <- df_quality_agg %>%
    tidyr::pivot_longer(cols = -1, names_to = "quality.parameter", values_to = "quality.value") %>%
    dplyr::left_join(lookup_par_unit)
}


# combine data sets ------------------------------------------------------------

if (FALSE) {

  # 1. well data + surface water + drilling + quality (-> feature table)
  df_well_features <- df_wells %>%
    dplyr::left_join(df_drilling, by = "drilling_id") %>%
    dplyr::left_join(df_drilling_tech, by = "drilling_id") %>%
    dplyr::left_join(df_quality_agg, by = "well_id") %>%
    dplyr::select(dplyr::all_of(well_features)) %>%
    dplyr::mutate(drilling_method = tidyr::replace_na(drilling_method, "Unbekannt")) %>%
    droplevels()

  save_data(df_well_features, paths$data_out, "well_feature_data")


  # 2. combine pump test data and capacity measurements (virtual pump_tests)
  df_pump_test_Q_monitoring <-
    dwc.wells:::combine_pump_test_and_Q_monitoring_data(
      df_pump_tests_tidy, df_Q_monitoring
    )



  # 3. combine Qs and feature tables
  df <- dwc.wells:::prepare_model_data(df_pump_tests_tidy, df_well_features)
  save_data(df, paths$data_out, "model_data")

}
