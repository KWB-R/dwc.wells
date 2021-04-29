
# Paths and packages -----------------------------------------------------------

library(dwc.wells)

path_list <- list(
  db = file.path(kwb.utils::desktop(), "tmp/DWC/wells/BWB_WV_Brunnenexport_2017.mdb"),
  renamings = extdata_file("renamings"),
  renamings_main = "<renamings>/main.csv",
  renamings_material_filter = "<renamings>/material_filter.csv",
  renamings_material_vollrohr = "<renamings>/material_vollrohr.csv",
  renamings_surface_water = "<renamings>/surface_water.csv",
  renamings_quality = "<renamings>/quality.csv",
  lookup_actions = "<renamings>/actions.csv"
)

paths <- kwb.utils::resolve(path_list)


# MAIN 0: Read renamings -------------------------------------------------------

if (TRUE) {

  # load renamings

  elements <- c("main", "quality", "material_filter",
                "material_vollrohr", "surface_water")

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
                                new_name_col = "new_name_de")

  # read data with water body information
  df_drilling <- read_select_rename(paths$db,
                                    "WV_GMS_TBL_BOHRUNGEN",
                                    renamings$main,
                                    old_name_col = "old_name",
                                    new_name_col = "new_name_de")

  # append water body data
  df_main <- dplyr::left_join(df_main, df_drilling)


  # filter Vertikalfilterbrunnen
  df_main <- df_main %>% dplyr::filter(grepl("V$", Name_Brunnen))


  # group categorical variables (material, surface water)
  df_main <- df_main %>%
    dplyr::mutate(Filtermaterial = rename_values(Filtermaterial,
                                                 renamings$material_filter),
                  Vollrohrmaterial = rename_values(Vollrohrmaterial,
                                                   renamings$material_vollrohr),
                  Gewaesser = rename_values(Gewaesser, renamings$surface_water)
                  )

  # correct false date imports in case of NA
  df_main <- df_main %>%
    dplyr::mutate(Datum_Inbetriebnahme = dplyr::na_if(Datum_Inbetriebnahme,
                                                    "1899-12-30 00:00:00"))

  # modify data types
  df_main <- df_main %>%
    dplyr::mutate(Baujahr = as.Date(Baujahr, format = "%Y-%m-%d"),
                  Datum_Inbetriebnahme = as.Date(Datum_Inbetriebnahme,
                                               format = "%Y-%m-%d"),
                  Jahr_Inbetriebnahme = lubridate::year(Datum_Inbetriebnahme),
                  Datum_RWS = as.Date(Datum_RWS, format = "%d.%m.%Y"),
                  Funktion_Brunnen = as.factor(Funktion_Brunnen),
                  Zustand_Brunnen = as.factor(Zustand_Brunnen),
                  Aquifer = as.factor(Aquifer),
                  Spannung = as.factor(Spannung),
                  Vollrohrmaterial = as.factor(Vollrohrmaterial),
                  Filtermaterial = as.factor(Filtermaterial),
                  Gebiet = as.factor(Gebiet),
                  Gewaesser = as.factor(Gewaesser))


  # turn any NAs in factor variables to "Unbekannt"
  df_main <- replace_NAs_in_factor_vars(df_main)


    # Gebiet short name
  df_main$Gebiet_short <- substr(df_main$Name_Brunnen, 1, 3)
  table(df_main$Gebiet, df_main$Gebiet_short)

}



# MAIN 2: pump test data -------------------------------------------------------

if (FALSE) {

  # read data
  df_pump_tests <- read_select_rename(paths$db,
                                      "WV_BRU_TBL_PUMPENTECHNDATEN",
                                      renamings$main,
                                      old_name_col = "old_name",
                                      new_name_col = "new_name_de")


  # clean data
  df_pump_tests <- df_pump_tests %>%
    # assign date format to dates
    dplyr::mutate(Datum_KPVvor = as.Date(Datum_KPVvor, format = "%Y-%m-%d"),
                  Datum_KPVnach = as.Date(Datum_KPVnach, format = "%Y-%m-%d")) %>%
    #filter(!(is.na(df_pump_tests$Datum_KPVvor) & is.na(df_pump_tests$Datum_KPVnach))) %>% # delete row if both values are NA
    # tidyr::drop_na(Datum_KPVvor, Datum_KPVnach) %>% # delete row if one of the two values is NA
    # get well characteristics to calculate Qs_rel
    dplyr::left_join(df_main[, c("id_Brunnen", "Datum_Inbetriebnahme", "Qs_neu")], by = "id_Brunnen") %>%
    # filter data with pump tests before operational start (data refers to rehabilitated well)
    # dplyr::filter(Datum_KPVvor > Datum_Inbetriebnahme) %>%should be done later
    # calculate Qs and Qs_rel before and after pump tests
    dplyr::mutate(Qs_KPVvor = Q_KPVvor / (BWS_KPVvor - RWS_KPVvor),
                  Qs_KPVvor_rel =  Qs_KPVvor / Qs_neu,
                  Qs_KPVnach = Q_KPVnach / (BWS_KPVnach - RWS_KPVnach),
                  Qs_KPVnach_rel =  Qs_KPVnach / Qs_neu) %>%
    dplyr::arrange(id_Brunnen, ifelse(!is.na(Datum_KPVvor), Datum_KPVvor, Datum_KPVnach)) %>%
    # check if pump test is associated with "Regenerierung"
    dplyr::mutate(Regenerierung = REG_Mechanisch + REG_Sprengschock +
                    REG_Hydropuls != 0,
                  Pumpenwechsel = Pumpenwechsel != 0,
                  Manschette = Manschette != 0) %>%
    dplyr::mutate(Bemerkung_Liner = ifelse(
      grepl("Liner|liner|Inliner|inliner|Lining|lining", Bemerkung), "Ja", "Nein"
      )) %>%
    dplyr::select(c("id_Brunnen", "Datum_Inbetriebnahme", "Qs_neu",
                    "Datum_KPVvor", "Qs_KPVvor", "Qs_KPVvor_rel",
                    "Datum_KPVnach", "Qs_KPVnach", "Qs_KPVnach_rel",
                    "Regenerierung", "Pumpenwechsel", "Manschette"))


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
                             new_name_col = "new_name_de")

  # check ratio Qmom / Qzul
  df_Q <- df_Q %>% dplyr::left_join(df_main[, c("id_Brunnen", "Qzul")]) %>%
    dplyr::mutate(Ratio_Qmom_Qzul = Qmom / Qzul) %>%
    dplyr::arrange(-Ratio_Qmom_Qzul)

  if (FALSE) {
    write_csv(df_Q, "Qmom_Qzul_ratio.csv")
  }

  # Kommentar: Es gibt Messungen zu 929 Brunnen, im Durchschnitt 40 Messwerte
  # pro Brunnen, im Median 80 m³/h momentane Förderleistung


  }


# MAIN 4: water quality data ---------------------------------------------------

if (FALSE) {

  # read data
  df_quality <- read_select_rename(paths$db,
                                   "DB2LABOR_Daten",
                                   renamings$main,
                                   old_name_col = "old_name",
                                   new_name_col = "new_name_de")


  # Select and rename quality parameter given in renamings file 'quality.csv'
  df_quality <- df_quality %>%
    dplyr::filter(Parameter %in% renamings$quality$old_name) %>%
    dplyr::mutate(Parameter = rename_values(Parameter,
                                     renamings$quality,
                                     old_name_col = "old_name",
                                     new_name_col = "new_name_de"))


  # transform concentration units from "µg/l" to "mg/l"
  indices <- df_quality$Einheit == "µg/l"
  df_quality[indices, "Wert"] <- df_quality[indices, "Wert"] * 10^-3
  df_quality[indices, "Einheit"] <- "mg/l"

  # delete measurements in unit "mg/kg"
  df_quality <- df_quality %>%
    dplyr::filter(Einheit != "mg/kg") %>%
    dplyr::filter(! (Parameter == "TS" & Einheit == "%"))

  df_quality %>% dplyr::group_by(Parameter) %>%
    dplyr::summarise(length(unique(Einheit)))

  # check units in quality data
  for (par in unique(df_quality$Parameter)) {

    a <- df_quality %>%
      dplyr::filter(Parameter == par) %>%
      dplyr::pull(Einheit) %>%
      frequency_table()

    cat(paste0("\n", par, ":\n"))
    print(a)
  }

  # aggregate / clean data
  df_quality_agg <- df_quality %>%
    dplyr::mutate(Datum_Wasserchemie = as.Date(Datum, format = "%Y-%m-%d")) %>%
    dplyr::group_by(id_Brunnen, Parameter) %>%
    dplyr::summarise(median = median(Wert, na.rm = TRUE),
                     std_dev = sd(Wert, na.rm = TRUE),
                     number = dplyr::n()) %>%
    dplyr::filter(Parameter != "GV") %>%  # discard Gluehverlust data (only 11 wells with observations)
    dplyr::select(id_Brunnen, Parameter, median) %>%
    tidyr::pivot_wider(names_from = Parameter, values_from = median,
                       id_cols = "id_Brunnen") %>%
    data.frame()

  str(df_quality_agg)

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
