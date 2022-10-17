if (FALSE) {

################################################################################
### 2. Make initial model dataset (with 48 columns)
################################################################################

# load package, paths and variable sets from global.R --------------------------
source("inst/extdata/scripts/global.R")

anomynize_model_dataset <- TRUE

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
df_pump_tests_tidy <- dwc.wells::prepare_pump_test_data(
  paths$data_pump_tests, renamings, df_wells
  )
save_data(df_pump_tests_tidy, paths$data_prep_out, "pump_test_data")

# 8. get standard deviation in static water level measurements
df_W_static_sd <- get_W_static_data(paths$data_W_static, renamings, df_wells) %>%
  dplyr:::group_by(well_id) %>%
  dplyr::summarise(W_static.sd = sd(W_static, na.rm = TRUE))

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

# 3. combine Qs and well_features (in total: 48 columns)
model_data <- dwc.wells:::combine_to_model_data(df_pump_tests_tidy,
                                                well_feature_data) %>%
  anonymize_dataset(anomynize = anomynize_model_dataset)

save_data(model_data, paths$data_prep_out, "model_data")

### Check different compression formats (as recommended by Rcmdcheck):
### https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Data-in-packages

#usethis::use_data(model_data, compress = "gzip", overwrite = TRUE) # 247 kB
#usethis::use_data(model_data, compress = "bzip2", overwrite = TRUE) # 261 kB
usethis::use_data(model_data, compress = "xz", overwrite = TRUE) # 180 kB

################################################################################
### 2. Make reduced model dataset (based on "data analysis" findings),
###    (with 27 columns)
################################################################################
model_data_reduced <- reduce_model_dataset(model_data)

usethis::use_data(model_data_reduced, compress = "xz", overwrite = TRUE) # 180 kB
}


reduce_model_dataset <- function(model_data) {

  model_data %>%
    dplyr::select(.data$Qs_rel,
                  .data$well_id,
                  tidyselect::all_of(model_features)) %>%
  # remove correlated and unimportant variables (see "inst/extdata/scripts/variable_importance.R")
  # remove correlated variables
    dplyr::select(-c(well_depth, quality.DR, quality.P_tot,
                     volume_m3_d.sd, waterworks, surface_water)) %>%
  # remove unimportant variables
    dplyr::select(-c(n_screens, filter_length, quality.Cu, inliner)) %>%
  # remove well gallery (local variable which makes models not applicable
  # to new sites or well galleries)
    dplyr::select(-well_gallery)

}


anonymize_hash <- function(x, algo="crc32"){

  unq_hashes <- vapply(unique(x),
                       function(object) digest::digest(object, algo=algo),
                       FUN.VALUE="",
                       USE.NAMES=TRUE)

  unname(unq_hashes[x])
}

anonymize_int <- function(x, n_values = 1000000){

  values <- x
  unique_values <- unique(values)
  n_unique_values <- as.integer(length(unique_values))
  random_values <- sample.int(n = n_values,
                              size = n_unique_values,
                              replace = FALSE)

  ### https://stackoverflow.com/a/50898859
  as.integer(as.character(factor(values, unique_values, random_values)))
}


anonymize_dataset <- function(df, anomynize = TRUE, seed_number = 1) {

  if(anomynize) {
    stopifnot(all(c("aquifer_coverage",
                    "drilling_method",
                    "screen_material",
                    "surface_water.distance",
                    "well_id") %in% names(df)))
    set.seed(seed = seed_number)
    df$aquifer_coverage <- as.factor(anonymize_hash(df$aquifer_coverage))
    df$drilling_method <- as.factor(anonymize_hash(df$drilling_method))
    df$screen_material <- as.factor(anonymize_hash(df$screen_material))
    df$surface_water.distance <- as.factor(anonymize_hash(df$surface_water.distance))
    df$well_id <- anonymize_int(df$well_id)
  }

  df

}


### currently not used
translate_aquifer_coverage <- function(stings, translate = TRUE) {

  if(translate) {
    strings <- kwb.utils::multiSubstitute(
      strings = strings,
      replacements = list(`bedeckt` = "confined",
                          `unbedeckt` = "unconfined",
                          `Unbekannt` = "unknown",
                          `bedeckt` = "confined",
                          `randlich` = "edges",
                          `teilweise` = "partly")
    ) %>%
      as.factor()
  }

  strings
}
