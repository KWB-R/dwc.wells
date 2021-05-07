
# Paths and packages -----------------------------------------------------------

library(dwc.wells)

path_list <- list(
  db = file.path(kwb.utils::desktop(), "tmp/DWC/wells/BWB_WV_Brunnenexport_2017.mdb"),
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

if (FALSE) {

  # read main data
  df_main <- read_select_rename(paths$db,
                                "WV_GMS_TBL_GWBR",
                                renamings$main,
                                old_name_col = "old_name",
                                new_name_col = "new_name_en")


  # read data with water body information
  df_drilling <- read_select_rename(paths$db,
                                    "WV_GMS_TBL_BOHRUNGEN",
                                    renamings$main,
                                    old_name_col = "old_name",
                                    new_name_col = "new_name_en")


  # append water body data
  df_main <- dplyr::left_join(df_main, df_drilling)


  # filter Vertikalfilterbrunnen
  df_main <- df_main %>% dplyr::filter(grepl("V$", well_name))


  # group categorical variables
  df_main <- df_main %>%
    dplyr::mutate(screen_material = rename_values(screen_material,
                                                 renamings$material_screen),
                  casing_material = rename_values(casing_material,
                                                   renamings$material_casing),
                  surface_water = rename_values(surface_water, renamings$surface_water))


  # correct false date imports in case of NA
  df_main <- df_main %>%
    dplyr::mutate(operational_start.date = dplyr::na_if(
      operational_start.date, "1899-12-30 00:00:00")
      )


  # modify data types
  factor_cols <- c("well_function", "operational_state", "aquifer_confinement",
                   "casing_material", "screen_material", "site", "surface_water")

  df_main <- df_main %>%
    dplyr::mutate(construction_year = as.Date(construction_year, format = "%Y-%m-%d"),
                  operational_start.date = as.Date(operational_start.date, format = "%Y-%m-%d"),
                  operational_start.year = lubridate::year(operational_start.date),
                  monitoring.date = as.Date(monitoring.date, format = "%d.%m.%Y")) %>%
    dplyr::mutate(dplyr::across(.cols = all_of(factor_cols), .fns = tidy_factor))


  # Gebiet short name
  df_main$site_short <- substr(df_main$well_name, 1, 3)
  table(df_main$site, df_main$site_short)

}



# MAIN 2: pump test data -------------------------------------------------------

if (FALSE) {

  # read main data
  df_main_tmp <- df_main %>%
    dplyr::select("well_id", tidyselect::starts_with("operational_start."))


  # read data
  df_pump_tests <- read_select_rename(paths$db,
                                      "WV_BRU_TBL_PUMPENTECHNDATEN",
                                      renamings$main,
                                      old_name_col = "old_name",
                                      new_name_col = "new_name_en") %>%
    # assign date format to dates
    dplyr::mutate(pump_test_1.date = as.Date(pump_test_1.date, format = "%Y-%m-%d"),
                  pump_test_2.date = as.Date(pump_test_2.date, format = "%Y-%m-%d")) %>%
    ### fix wrong date entry for well_id = 6405
    dplyr::mutate(pump_test_1.date = dplyr::if_else(pump_test_1.date == as.Date("0205-04-28"),
                                            as.Date("2005-04-28"),
                                            pump_test_1.date)) %>%
    # delete row if both values are NA
    dplyr::filter(!(is.na(pump_test_1.date) & is.na(pump_test_2.date))) %>%
    # add date column not containing NAs (required for creating an "action_id")
    dplyr::mutate(pump_test.date = dplyr::if_else(!is.na(pump_test_1.date),
                                          as.Date(pump_test_1.date),
                                          as.Date(pump_test_2.date))) %>%
    # get well characteristics to calculate Qs_rel
    dplyr::left_join(df_main_tmp, by = "well_id") %>%
    # filter data with pump tests before operational start (data refers to rehabilitated well)
    # dplyr::filter(Datum_KPVvor > Datum_Inbetriebnahme) %>%should be done later
    # calculate Qs and Qs_rel before and after pump tests
    dplyr::mutate(pump_test_1.Qs = pump_test_1.Q /
                    (pump_test_1.W_dynamic - pump_test_1.W_static),
                  pump_test_1.Qs_rel =  pump_test_1.Qs / operational_start.Qs,
                  pump_test_2.Qs = pump_test_2.Q /
                    (pump_test_2.W_dynamic - pump_test_2.W_static),
                  pump_test_2.Qs_rel =  pump_test_2.Qs / operational_start.Qs
                 ) %>%
    #dplyr::mutate(dplyr::across(tidyselect::everything(), as.character)) %>%
    dplyr::arrange(well_id, pump_test.date) %>%
    dplyr::group_by(well_id) %>%
    dplyr::mutate(action_id = dplyr::row_number()) %>%
    # check if pump test is associated with "Regenerierung"
    dplyr::mutate(pump_test_2.well_rehab = (well_rehab.general + well_rehab.shock +
                                     well_rehab.hydropulse) != 0,
                  pump_test_2.substitute_pump = substitute_pump != 0,
                  pump_test_2.pressure_sleeve =  pressure_sleeve != 0) %>%
    dplyr::mutate(pump_test_2.comment_liner = ifelse(
      grepl("Liner|liner|Inliner|inliner|Lining|lining", well_rehab.comment), TRUE, FALSE
    )) %>%
    dplyr::select("well_id",
                  "action_id",
                  tidyselect::starts_with("operational_start"),
                  tidyselect::starts_with("pump_test"),
                  "pump_test_2.comment_liner",
                  "pump_test_2.well_rehab",
                  "pump_test_2.substitute_pump",
                  "pump_test_2.pressure_sleeve") %>%
    dplyr::select(- "pump_test.date")


  to_longer_columns <- df_pump_tests %>%
    dplyr::ungroup() %>%
    dplyr::select(
    tidyselect::starts_with("operational_start"),
    tidyselect::starts_with("pump_test")
    ) %>%
    names()

  # Helper function
  cumsum_no_na <- function (x) {
    cumsum(ifelse(is.na(x), 0, x))
  }

  df_pump_tests_tidy <- df_pump_tests %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), as.character)) %>%
    tidyr::pivot_longer(cols = tidyselect::all_of(to_longer_columns),
                        names_to = c("key", "parameter"),
                        names_sep = "\\.",
                        values_to = "value") %>%
    dplyr::filter(!is.na(value)) %>%
    tidyr::pivot_wider(names_from = "parameter",
                       values_from = "value") %>%
    dplyr::select(- year) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(c("well_id", "action_id")), as.integer)) %>%
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
                  Qs_rel = dplyr::if_else(key == "operational_start",
                                          100,
                                          Qs_rel)
                  ) %>%
    dplyr::arrange(well_id, action_id) %>%
    dplyr::group_by(well_id) %>%
    dplyr::left_join(df_main_tmp %>%
                     dplyr::select(well_id, operational_start.date),
                     by = "well_id") %>%
    dplyr::mutate(days_since_operational_start = difftime(date,
                                                          operational_start.date,
                                                          units = "days") %>% as.integer(),
                  days_since_last_action = days_since_operational_start - dplyr::lag(days_since_operational_star,
                                                                                     default = 0),
                  n.well_rehab = cumsum_no_na(well_rehab),
                  n.substitute_pump = cumsum_no_na(substitute_pump),
                  n.pressure_sleeve = cumsum_no_na(pressure_sleeve),
                  n.comment_liner = cumsum_no_na(comment_liner)
                  )



  df_pump_tests_tidy %>% View()


  a <- df_pump_tests %>% dplyr::filter(is.na(df_pump_tests$pump_test_1.date) & is.na(df_pump_tests$pump_test_2.date))

  frequency_table(a$well_rehab)
  frequency_table(a$substitute_pump)
  frequency_table(a$pressure_sleeve)


  if (FALSE) {
    write.table(df_pump_tests, file = "Kurzpumpversuche.csv", dec = ".", sep = ";", row.names = FALSE)
  }

}


# MAIN 3: Q measurement data ---------------------------------------------------

if (FALSE) {

  # read data
  df_Q <- read_select_rename(paths$db,
                             "WV_BRU_TBL_ERG",
                             renamings$main,
                             old_name_col = "old_name",
                             new_name_col = "new_name_en")


  str(df_Q)

  # turn date to date format
  df_Q <- df_Q %>%
    dplyr::mutate(monitoring.date = as.Date(monitoring.date, format = "%Y-%m-%d"))


  # check ratio Qmom / Qzul
  df_Q <- df_Q %>%
    dplyr::left_join(df_main[, c("well_id", "admissible_discharge")]) %>%
    dplyr::mutate(Ratio_Qmom_Qzul = monitoring.Q / admissible_discharge)


  if (FALSE) {
    write_csv(df_Q, "Qmom_Qzul_ratio.csv")
  }

  # Kommentar: Es gibt Messungen zu 929 Brunnen, im Durchschnitt 40 Messwerte
  # pro Brunnen, im Median 80 m?/h momentane F?rderleistung


}


# MAIN 4: water quality data ---------------------------------------------------

if (FALSE) {

  # read data
  df_quality <- read_select_rename(paths$db,
                                   "DB2LABOR_Daten",
                                   renamings$main,
                                   old_name_col = "old_name",
                                   new_name_col = "new_name_en")



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


  lookup_par_unit <- aggregate(quality.unit~quality.parameter, df_quality, FUN = unique)

  df_quality_agg_long <- df_quality_agg %>%
    tidyr::pivot_longer(cols = -1, names_to = "quality.parameter", values_to = "quality.value") %>%
    dplyr::left_join(lookup_par_unit)

}


# MAIN 5: join data ------------------------------------------------------------

if (FALSE) {

  # verknuepfen

  df <- df_pump_tests %>%
    dplyr::left_join(df_main, by = "id_Brunnen") %>%
    dplyr::left_join(df_Q_agg, by = "id_Brunnen") %>%
    dplyr::right_join(df, df2, by = "id_Brunnen")

  data.frame(table(df$fil_Brunnenfunktion))

}


# MAIN 6: Data analysis -----------------------------------------------------------

if (FALSE) {

  # export frequency lists
  frequency_table(df_main$Gewaesser) %>% write_csv("Gewaesser.csv")
  frequency_table(df_main$Filtermaterial) %>% write_csv("Materialien_Filter.csv")

}
