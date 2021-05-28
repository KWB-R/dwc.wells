
# Paths and packages -----------------------------------------------------------

library(dwc.wells)

path_list <- list(
  db = file.path(kwb.utils::desktop(), "tmp/DWC/wells/BWB_WV_Brunnenexport_2017.mdb"),
  csv_data = file.path(kwb.utils::desktop(), "tmp/DWC/wells/Data/Daten_BWB_Schimmelpfennig"),
  data_wells = "<csv_data>/GWBR.csv",
  data_drilling = "<csv_data>/Bohrungen.csv",
  data_drilling_tech = "<csv_data>/Bohrtechnik.csv",
  data_pump_tests = "<csv_data>/Arbeitsberichte.csv",
  data_W_static = "<csv_data>/RWS.csv",
  data_quantity = "<csv_data>/ERG.csv",
  data_quality = "<csv_data>/___.csv",
  data_quality_para1 = "<csv_data>/LIMS_Para.csv",
  data_quality_para2 = "<csv_data>/LIMS_PM.csv",
  data_kf = "<csv_data>/KF-Werte.csv",
  renamings = dwc.wells::extdata_file("renamings"),
  renamings_main = "<renamings>/main.csv",
  renamings_material_screen = "<renamings>/material_screen.csv",
  renamings_material_casing = "<renamings>/material_casing.csv",
  renamings_surface_water = "<renamings>/surface_water.csv",
  renamings_quality = "<renamings>/quality.csv",
  lookup_actions = "<renamings>/actions.csv"
)

paths <- kwb.utils::resolve(path_list)


# MAIN 0: Read renamings -------------------------------------------------------

if (TRUE) {

  # load renamings

  elements <- c("main", "quality", "material_screen",
                "material_casing", "surface_water")

  renamings <- lapply(paste("renamings", elements, sep = "_"), function(x) {
    load_renamings_csv(paths[[x]])
  })

  names(renamings) <- elements

  lookup <- list(actions = load_renamings_csv(paths$lookup_actions))

}


# MAIN 1: general well characteristics and water body information --------------

if (FALSE)
  {

  # read data from db
  #df_wells <- dwc.wells:::read_ms_access_mri(paths$db, "WV_GMS_TBL_GWBR")

  # read data from csv
  df_wells <- read_csv(paths$data_wells, skip = 7)

  # select and rename columns
  df_wells <- select_rename_cols(df_wells,
                                renamings$main,
                                old_name_col = "old_name",
                                new_name_col = "new_name_en")


  # filter Vertikalfilterbrunnen
  df_wells <- df_wells %>% dplyr::filter(grepl("V$", well_name))


  # filter duplicates or NA in site_id
  site_ids_with_duplicates <- unique(df_wells$site_id[duplicated(df_wells$site_id)])
  df_wells <- df_wells %>% dplyr::filter(!site_id %in% site_ids_with_duplicates)

  # read drilling data from csv
  df_drilling <- read_csv(paths$data_drilling)

  # select rename
  df_drilling <- select_rename_cols(df_drilling,
                                    renamings$main,
                                    old_name_col = "old_name",
                                    new_name_col = "new_name_en")


  # read drilling tech data from csv
  df_drilling_tech <- read_csv(paths$data_drilling_tech) %>%
    select_rename_cols(renamings$main,"old_name", "new_name_en")


  # append water body data
  df_wells <- dplyr::left_join(df_wells, df_drilling)


  # set false date imports to NA
  df_wells <- df_wells %>%
    dplyr::mutate(operational_start.date = dplyr::na_if(
      operational_start.date, "1899-12-30 00:00:00")
      )


  # set "nicht bekannt" to NA
  df_wells <- df_wells %>%
    dplyr::mutate(dplyr::across(tidyr::starts_with("aquifer"),
                                ~dplyr::na_if(., "nicht bekannt")))


  # group categorical variables
  df_wells <- df_wells %>%
    dplyr::mutate(screen_material = rename_values(screen_material,
                                                  renamings$material_screen),
                  casing_material = rename_values(casing_material,
                                                  renamings$material_casing),
                  surface_water = rename_values(surface_water, renamings$surface_water))


  # modify data types
  factor_cols <- c("well_function", "operational_state", "aquifer_confinement",
                   "aquifer_coverage", "casing_material", "screen_material",
                   "waterworks", "surface_water")

  date_cols <- c("construction_date", "operational_start.date",
                 "operational_state.date", "inliner.date")

  df_wells <- df_wells %>%
    dplyr::mutate(dplyr::across(date_cols, .fns = as.Date, format = "%Y-%m-%d"),
                  monitoring.date = as.Date(monitoring.date, format = "%d.%m.%Y"),
                  dplyr::across(dplyr::all_of(factor_cols), .fns = tidy_factor))


  # calculate years from date, not required
  if (FALSE) {
    df_wells <- df_wells %>%
      dplyr::mutate(construction_year = lubridate::year(construction_date),
                    operational_start.year = lubridate::year(operational_start.date))
  }

  # determine name of well gallery
  df_wells$well_gallery <- substr(df_wells$well_name, 1, 7)

  # short name of waterworks
  if (FALSE) {
    df_wells$waterworks_short <- substr(df_wells$well_name, 1, 3)
      }

  # select columns with model variables
  relevant_vars <- c("site_id",
                     "waterworks",
                     "well_gallery",
                     "aquifer_coverage",
                     "aquifer_confinement",
                     "screen_material",
                     "diameter",
                     "n_screens",
                     "surface_water",
                     "inliner.date")

  df_wells_vars <- df_wells %>% dplyr::select(dplyr::all_of(relevant_vars))
  #write.table(df_wells_var, file = "Stammdaten_aufbereitet_Auswahl.csv", dec = ".", sep = ";", row.names = FALSE)

  }


# MAIN 2: pump test data -------------------------------------------------------

if (FALSE) {

  # 0. select columns from well data to be joined with pump test data
  # for further calculations
  df_wells_operational_start <- df_wells %>%
    dplyr::select("well_id", "site_id", tidyselect::starts_with("operational_start."))


  # 0. read and rename data
  df_pump_tests <- read_csv(paths$data_pump_tests, skip = 2) %>%
      select_rename_cols(renamings$main, "old_name", "new_name_en")


  #colnames(df_pump_tests) %in% renamings$main$old_name


  # 1. assign date format to dates
  df_pump_tests <- df_pump_tests %>%
    # assign date format to dates
    dplyr::mutate(pump_test_1.date = as.Date(pump_test_1.date, format = "%Y-%m-%d"),
                  pump_test_2.date = as.Date(pump_test_2.date, format = "%Y-%m-%d"))

  if (FALSE) {
    # fix wrong date entry for well_id = 6405, not required for new csv data
    df_pump_tests <- df_pump_tests %>%
      dplyr::mutate(pump_test_1.date = dplyr::if_else(
        pump_test_1.date == as.Date("0205-04-28"),
        as.Date("2005-04-28"),
        pump_test_1.date
        ))
  }


  # 2. fill up pump test dates and calculate action date
  df_pump_tests <- df_pump_tests %>%
    # delete row if both values are NA
    dplyr::filter(!(is.na(pump_test_1.date) & is.na(pump_test_2.date))) %>%
    # add date column not containing NAs (required for creating an "action_id")
    dplyr::mutate(interval_days = dplyr::if_else(!is.na(pump_test_1.date) & !is.na(pump_test_2.date),
                                                  real_interval(pump_test_2.date,
                                                                 pump_test_1.date),
                                                  default_interval(pump_test_2.date,
                                                                    pump_test_1.date,
                                                                    func = mean)),
                  interval_type = dplyr::if_else(!is.na(pump_test_1.date) & !is.na(pump_test_2.date),
                                                  "real",
                                                  "default"),
                  pump_test_1.date = dplyr::if_else(is.na(pump_test_1.date) & !is.na(pump_test_2.date),
                                                   pump_test_2.date - interval_days,
                                                   pump_test_1.date),
                  pump_test_2.date = dplyr::if_else(is.na(pump_test_2.date) & !is.na(pump_test_1.date),
                                                   pump_test_1.date + interval_days,
                                                   pump_test_2.date),
                  action_date = pump_test_1.date + ceiling(interval_days/2))


  # 3. calculate Qs and Qs rel
  df_pump_tests <- df_pump_tests %>%
    # get well characteristics to calculate Qs_rel
    dplyr::right_join(df_wells_operational_start, by = "site_id") %>%
    # calculate Qs and Qs_rel for pump tests 1 and 2
    dplyr::mutate(pump_test_1.Qs = pump_test_1.Q /
                    (pump_test_1.W_dynamic - pump_test_1.W_static),
                  pump_test_1.Qs_rel =  pump_test_1.Qs / operational_start.Qs,
                  pump_test_2.Qs = pump_test_2.Q /
                    (pump_test_2.W_dynamic - pump_test_2.W_static),
                  pump_test_2.Qs_rel =  pump_test_2.Qs / operational_start.Qs
    )


  # 4. derive action type
  df_pump_tests <- df_pump_tests %>%
    # give action id
    dplyr::arrange(site_id, action_date) %>%
    dplyr::group_by(site_id) %>%
    dplyr::mutate(action_id = dplyr::row_number()) %>%
    # check if pump test is associated with regeneration (three types)
    dplyr::mutate(pump_test_2.well_rehab = (well_rehab.general + well_rehab.shock +
                                              well_rehab.hydropulse) != 0,
                  pump_test_2.substitute_pump = substitute_pump != 0,
                  pump_test_2.pressure_sleeve =  pressure_sleeve != 0) %>%
    dplyr::mutate(pump_test_2.comment_liner = ifelse(
      grepl("Liner|liner|Inliner|inliner|Lining|lining", well_rehab.comment), TRUE, FALSE
    ))


  # 5. select relevant columns
  df_pump_tests <- df_pump_tests %>%
    # select important variables
    dplyr::select("site_id",
                  "well_id",
                  "action_id",
                  "action_date",
                  tidyselect::starts_with("interval_"),
                  tidyselect::starts_with("operational_start"),
                  tidyselect::starts_with("pump_test"),
                  -tidyselect::ends_with(c("Q", "W_static", "W_dynamic"))
                  )


  # get column names needed for pivoting data
  cols_to_longer <- df_pump_tests %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyr::starts_with(c("operational_start", "pump_test"))) %>%
    names()


  # 6. tidy data
  df_pump_tests_tidy <- df_pump_tests %>%
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
    dplyr::mutate(dplyr::across(tidyselect::all_of(c("site_id", "well_id", "action_id")), as.integer)) %>%
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
                                          1,
                                          Qs_rel)
                  ) %>%
    dplyr::arrange(site_id, action_id) %>%
    dplyr::group_by(site_id) %>%
    # join  dates of operational start to calculate time differences
    dplyr::left_join(df_wells_operational_start %>%
                     dplyr::select(site_id, operational_start.date),
                     by = "site_id") %>%

    dplyr::mutate(days_since_operational_start =
                    as.integer(
                      difftime(date, operational_start.date, units = "days")
                      ),
                  n.well_rehab = cumsum_no_na(well_rehab),
                  n.substitute_pump = cumsum_no_na(substitute_pump),
                  n.pressure_sleeve = cumsum_no_na(pressure_sleeve),
                  n.comment_liner = cumsum_no_na(comment_liner)
                  ) %>%
   dplyr::ungroup() %>%
   dplyr::arrange(site_id, n.well_rehab, date) %>%
   dplyr::group_by(site_id, n.well_rehab) %>%
   dplyr::mutate(days_since_last_rehab =  dplyr::if_else(
     n.well_rehab > 0,
     as.integer(date - min(date) + (min(date) - min(action_date))),
     days_since_operational_start
     ))


relevant_cols <- c(
    "site_id",
    "well_id",
    "date",
    "key",
    "Qs_rel",
    "days_since_operational_start",
    "n.well_rehab",
    "days_since_last_rehab")



 # preliminary model input
 df_model_input_1 <- df_pump_tests_tidy %>%
   dplyr::select(relevant_cols) %>%
   dplyr::left_join(df_wells_vars, by = "site_id")


 if (FALSE) {
    write.table(df_model_input_1, file = "model_input_1.csv", dec = ".", sep = ";", row.names = FALSE)
   write.table(df_pump_tests_tidy, file = "Kurzpumpversuche_tidy.csv", dec = ".", sep = ";", row.names = FALSE)
  }

}


# MAIN 3: Q measurement data ---------------------------------------------------

if (FALSE) {

  # read data
  if (FALSE) {
    df_Q <- read_select_rename(paths$db,
                               "WV_BRU_TBL_ERG",
                               renamings$main,
                               old_name_col = "old_name",
                               new_name_col = "new_name_en")
  }

  df_Q <- read_csv(paths$data_quantity) %>%
    select_rename_cols(renamings$main, "old_name", "new_name_en") %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::mutate(date = dplyr::na_if(date, "1899-12-30 00:00:00"))


  # check ratio Qmom / Qzul
  df_Q <- df_Q %>%
    dplyr::left_join(df_main[, c("well_id", "admissible_discharge")]) %>%
    dplyr::mutate(Ratio_Qmom_Qzul = Q / admissible_discharge)



  if (FALSE) {
    write_csv(df_Q, "Qmom_Qzul_ratio.csv")
  }

  # Kommentar: Es gibt Messungen zu 929 Brunnen, im Durchschnitt 40 Messwerte
  # pro Brunnen, im Median 80 m?/h momentane F?rderleistung


}


# MAIN 4: water quality data ---------------------------------------------------

if (FALSE) {

  # read data
  df_quality <- read_ms_access_mri(paths$db, "DB2LABOR_Daten") %>%
    select_rename_cols(renamings$main, "old_name", "new_name_en")

  str(df_quality)


  # Select and rename quality parameter given in renamings file 'quality.csv'
  df_quality <- df_quality %>%
    dplyr::filter(quality.parameter %in% renamings$quality$old_name) %>%
    dplyr::mutate(quality.parameter = rename_values(quality.parameter,
                                            renamings$quality,
                                            old_name_col = "old_name",
                                            new_name_col = "new_name_en"))


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


  df_quality %>% dplyr::group_by(quality.parameter) %>%
    dplyr::summarise(length(unique(quality.unit)))

  # check units in quality data
  for (par in unique(df_quality$quality.parameter)) {

    a <- df_quality %>%
      dplyr::filter(quality.parameter == par) %>%
      dplyr::pull(quality.unit) %>%
      frequency_table()

    cat(paste0("\n", par, ":\n"))
    print(a)
  }

  # aggregate / clean data
  df_quality_agg <- df_quality %>%
    dplyr::mutate(quality.date = as.Date(quality.date, format = "%Y-%m-%d")) %>%
    dplyr::group_by(well_id, quality.parameter) %>%
    dplyr::summarise(quality.median = median(quality.value, na.rm = TRUE),
                     quality.std_dev = sd(quality.value, na.rm = TRUE),
                     quality.number = dplyr::n()) %>%
    dplyr::filter(quality.parameter != "LOI") %>%  # discard Gluehverlust data (only 11 wells with observations)
    dplyr::select(well_id, quality.parameter, quality.median) %>%
    tidyr::pivot_wider(names_from = quality.parameter,
                       values_from = quality.median,
                       id_cols = "well_id") %>%
    data.frame()


  if (FALSE) {
    write.table(df_quality_agg, file = "quality_measurements_agg.csv", dec = ".", sep = ";", row.names = FALSE)
  }

  lookup_par_unit <- aggregate(quality.unit~quality.parameter, df_quality, FUN = unique)

  df_quality_agg_long <- df_quality_agg %>%
    tidyr::pivot_longer(cols = -1, names_to = "quality.parameter", values_to = "quality.value") %>%
    dplyr::left_join(lookup_par_unit)

}

