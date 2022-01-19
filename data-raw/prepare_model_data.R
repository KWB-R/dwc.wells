
# load package, paths and variable sets from global.R --------------------------

source("inst/extdata/scripts/global.R")


# MAIN 1: get and prepare individual data sets ---------------------------------

# 1. well characteristics
df_wells <- dwc.wells:::prepare_well_data(paths$data_wells, renamings)
save_data(df_wells, paths$data_prep_out, "well_data")

# 2. surface water information
df_drilling <- dwc.wells:::prepare_drilling_data(paths$data_drilling, renamings)
save_data(df_drilling, paths$data_prep_out, "drilling_data")

# 3. drilling method information
df_drilling_tech <- dwc.wells:::prepare_drilling_tech_data(
  paths$data_drilling_tech, renamings
  )
save_data(df_drilling_tech, paths$data_prep_out, "drilling_tech_data")

# 4. water quality data
df_quality_agg <- dwc.wells:::prepare_quality_data(paths$db, renamings)
save_data(df_quality_agg, paths$data_prep_out, "quality_data")

# 5. abstraction volumes
df_volumes_agg <- dwc.wells:::prepare_volume_data(paths$db, renamings, df_wells)
save_data(df_volumes_agg, paths$data_prep_out, "volume_data")

# 6. capacity measurements (virtual pump tests)
df_Q_monitoring <- dwc.wells:::prepare_Q_monitoring_data(
  df_wells, paths$data_quantity, paths$data_W_static, renamings
  )
save_data(df_Q_monitoring, paths$data_prep_out, "Q_monitoring_data")

# 7. pump test and rehab data
df_pump_tests_tidy <- prepare_pump_test_data(
  paths$data_pump_tests, renamings, df_wells
  )
save_data(df_pump_tests_tidy, paths$data_prep_out, "pump_test_data")

# 8. get standard deviation in static water level measurements
df_W_static_sd <- get_W_static_data(paths$data_W_static, renamings, df_wells) %>%
  group_by(well_id) %>%
  summarise(W_static.sd = sd(W_static, na.rm = TRUE))


# MAIN 2: combine individual data sets -----------------------------------------

# 1. well data + surface water + drilling + quality (-> feature table)
well_feature_data <- df_wells %>%
  dplyr::left_join(df_drilling, by = "drilling_id") %>%
  dplyr::left_join(df_drilling_tech, by = "drilling_id") %>%
  dplyr::left_join(df_quality_agg, by = "well_id") %>%
  dplyr::left_join(df_volumes_agg, by = "well_id") %>%
  dplyr::left_join(df_W_static_sd, by = "well_id") %>%
  dplyr::select(dplyr::all_of(well_features)) %>%
  dplyr::mutate(drilling_method = tidyr::replace_na(drilling_method, "Unbekannt"),
                W_static.sd = replace_na_with_median(W_static.sd),
                volume_m3_d.mean = replace_na_with_median(volume_m3_d.mean),
                volume_m3_d.sd = replace_na_with_median(volume_m3_d.sd),
                volume_m3_d.cv = replace_na_with_median(volume_m3_d.cv)) %>%
  droplevels()


# 2. combine pump test data and capacity measurements (virtual pump_tests)
df_pump_test_Q_monitoring <-
  dwc.wells:::combine_pump_test_and_Q_monitoring_data(
    df_pump_tests_tidy,
    df_Q_monitoring
  )


# 3. combine Qs and well_features
model_data <- dwc.wells:::combine_to_model_data(df_pump_tests_tidy, well_feature_data)
save_data(model_data, paths$data_prep_out, "model_data")


### Check different compression formats (as recommended by Rcmdcheck):
### https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Data-in-packages

#usethis::use_data(model_data, compress = "gzip", overwrite = TRUE) # 247 kB
#usethis::use_data(model_data, compress = "bzip2", overwrite = TRUE) # 261 kB
usethis::use_data(model_data, compress = "xz", overwrite = TRUE) # 180 kB
