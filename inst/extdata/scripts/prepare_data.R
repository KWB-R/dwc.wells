# source("C:/Users/mriech/Desktop/R_Development/RScripts/SEMA/UseCase_Berlin/1_DataPreparation/SEMA_PrepareData_Berlin.R")

# links:
# https://www.brodrigues.co/blog/2020-03-08-tidymodels/
# https://www.r-bloggers.com/2021/02/machine-learning-with-r-a-complete-guide-to-gradient-boosting-and-xgboost/
# https://www.r-bloggers.com/2021/02/deep-learning-with-r-and-keras-build-a-handwritten-digit-classifier-in-10-minutes/
# https://www.r-bloggers.com/2021/02/getting-started-with-k-means-and-tidytuesday-employment-status/
# https://www.r-bloggers.com/2021/03/k-means-101-an-introductory-guide-to-k-means-clustering-in-r/

library(dwc.wells)

# MAIN 0: load paths and variables ---------------------------------------------

path_db <- file.path(kwb.utils::desktop(), "tmp/DWC/wells/BWB_WV_Brunnenexport_2017.mdb")
path_renamings <- extdata_file("renamings")
#path_renamings <- file.path(kwb.utils::desktop(), "tmp/DWC/wells/renamings")


# MAIN 0: Read renamings -------------------------------------------------------

if (FALSE) {


  # load renamings
  renamings <- load_renamings_excel(
    file.path(path_renamings, "Parameterliste.xlsx")
    )

  renamings_quality <- load_renamings_csv(
    file.path(path_renamings, "quality.csv")
    )

  renamings_material_filter <- load_renamings_csv(
    file.path(path_renamings, "material_filter.csv")
    )

  renamings_material_vollrohr <- load_renamings_csv(
    file.path(path_renamings, "material_vollrohr.csv")
  )

}

# MAIN 1: general well characteristics -----------------------------------------

if (FALSE) {

  # read data
  df_main <- read_select_rename(path_db, "WV_GMS_TBL_GWBR", renamings$main)

  # filter Vertikalfilterbrunnen
  df_main <- df_main %>% dplyr::filter(grepl("V$", name_Brunnen))

  # group filter material
  df_main$mw_Filtermaterial <- rename_values(df_main$mw_Filtermaterial,
                                             renamings_material_filter)

  # group vollrohr material
  df_main$mw_Vollrohrmaterial <- rename_values(df_main$mw_Vollrohrmaterial,
                                             renamings_material_vollrohr)


  # correct false date imports in case of NA
  df_main <- df_main %>%
    dplyr::mutate(dat_Inbetriebnahme = dplyr::na_if(dat_Inbetriebnahme,
                                                    "1899-12-30 00:00:00"))

  # modify data types
  df_main <- df_main %>%
    dplyr::mutate(Baujahr = as.Date(Baujahr, format = "%Y-%m-%d"),
                  dat_Inbetriebnahme = as.Date(dat_Inbetriebnahme,
                                               format = "%Y-%m-%d"),
                  Jahr_Inbetriebnahme = lubridate::year(dat_Inbetriebnahme),
                  dat_RWS = as.Date(dat_RWS, format = "%d.%m.%Y"),
                  fil_Brunnenfunktion = as.factor(fil_Brunnenfunktion),
                  fil_Brunnenzustand = as.factor(fil_Brunnenzustand),
                  mw_Aquifer = as.factor(mw_Aquifer),
                  mw_Spannung = as.factor(mw_Spannung),
                  mw_Vollrohrmaterial = as.factor(mw_Vollrohrmaterial),
                  mw_Filtermaterial = as.factor(mw_Filtermaterial),
                  Gebiet = as.factor(Gebiet))


  # turn any NAs in factor variables to "Unbekannt"
  factor_vars <- names(df_main)[sapply(df_main, is.factor)]

  for (factor_var in factor_vars) {

    # turn NA into "Unbekannt"
    df_main[, factor_var] <- forcats::fct_explicit_na(
      df_main[, factor_var], na_level = "Unbekannt"
    )

    # turn empty fields into "Unbekannt"
    df_main[df_main[, factor_var] == "", factor_var] <- "Unbekannt"

    # remove empty factor levels and arrange factor levels
    df_main[, factor_var] <- df_main[, factor_var] %>%
      droplevels() %>%
      forcats::fct_infreq()

  }

  # Gebiet short name
  df_main$Gebiet_short <- substr(df_main$name_Brunnen, 1, 3)
  table(df_main$Gebiet, df_main$Gebiet_short)

  # first plot
  variables_cat <- c("fil_Brunnenzustand",
                     "mw_Filtermaterial",
                     "Gebiet",
                     "mw_Spannung")

  plots_cat <- lapply(variables_cat, function(x) { plot_frequencies(df_main, x, x) })
  cowplot::plot_grid(plotlist = plots_cat, ncol = 2, align = "hv")

  variables_num <- c("Jahr_Inbetriebnahme",
                     "Qs_neu",
                     "mw_Durchmesser",
                     "mw_Filteranzahl")

  variables_num_1 <- c("Jahr_Inbetriebnahme",
                     "Qs_neu")


  variables_num_2 <- c("mw_Durchmesser",
                     "mw_Filteranzahl")


  plot_distribution(Data = df_main,
                    variable = "mw_Filteranzahl",
                    binwidth = 1,
                    title = "Filteranzahl",
                    vertical_x_axis_labels = FALSE,
                    col = "white")

  plots_num <- lapply(variables_num_1, function(x) {
    plot_distribution(Data = df_main, variable = x, title = x)
    })

  plot_distribution(Data = df_main,
                    variable = "mw_Filteranzahl",
                    binwidth = 1,
                    title = "Filteranzahl",
                    vertical_x_axis_labels = FALSE)

  plot_distribution(Data = df_main,
                    variable = "mw_Durchmesser",
                    binwidth = 50,
                    title = "Durchmesser",
                    vertical_x_axis_labels = FALSE)

  plots_num <- lapply(variables_num_2, function(x) {
    plot_distribution(Data = df_main, variable = x, title = x)
  })


  cowplot::plot_grid(plotlist = plots_num, ncol = 2, align = "hv")
  data.frame(table(df_main$mw_Durchmesser, useNA = "ifany"))
  ggplot2::ggsave("frequency_plot_v2.png", width = 10, height = 7, dpi = 600)

  str(df_main)
}



# MAIN 2: pump test data -------------------------------------------------------

if (FALSE) {

  # read data
  df_pump_tests <- read_select_rename(path_db, "WV_BRU_TBL_PUMPENTECHNDATEN",
                                      renamings$main)

  # clean data
  df_pump_tests <- df_pump_tests %>%
    dplyr::mutate(dat_KPVvor = as.Date(dat_KPVvor, format = "%Y-%m-%d"),
                  dat_KPVnach = as.Date(dat_KPVnach, format = "%Y-%m-%d")) %>%
    filter(!(is.na(df_pump_tests$dat_KPVvor) & is.na(df_pump_tests$dat_KPVnach))) %>% # delete row if both values are NA
    # tidyr::drop_na(dat_KPVvor, dat_KPVnach) %>% # delete row if one of the two values is NA
    dplyr::left_join(df_main[, c("id_Brunnen", "dat_Inbetriebnahme", "Qs_neu")], by = "id_Brunnen") %>%
    dplyr::mutate(Qs_KPVvor = mw_Q_KPVvor / (mw_BWS_KPVvor - mw_RWS_KPVvor),
                  Qs_KPVvor_rel =  Qs_KPVvor / Qs_neu,
                  Qs_KPVnach = mw_Q_KPVnach / (mw_BWS_KPVnach - mw_RWS_KPVnach),
                  Qs_KPVnach_rel =  Qs_KPVnach / Qs_neu) %>%
    dplyr::arrange(id_Brunnen, ifelse(!is.na(dat_KPVvor), dat_KPVvor, dat_KPVnach)) %>%
    dplyr::select(c("id_Brunnen", "dat_Inbetriebnahme", "Qs_neu",
                    "dat_KPVvor", "Qs_KPVvor", "Qs_KPVvor_rel",
                    "dat_KPVnach", "Qs_KPVnach", "Qs_KPVnach_rel"))

 # write.table(df_pump_tests, file = "Q_s_Berechnungen_v2.csv", dec = ".", sep = ";", row.names = FALSE)

}


# MAIN 3: Q measurement data ---------------------------------------------------

  if (FALSE) {

  # read data
  df_Q <- read_select_rename(path_db, "WV_BRU_TBL_ERG", renamings$main)

  # aggregate data
  df_Q_agg <- dplyr::filter(df_Q, Qmom < 1000) %>%
    dplyr::group_by(id_Brunnen) %>%
    dplyr::summarise(Qmom_median = median(Qmom, na.rm = TRUE),
                     std_dev = sd(Qmom, na.rm = TRUE),
                     number = n()) %>%
    tidyr::drop_na()

  # Kommentar: Es gibt Messungen zu 929 Brunnen, im Durchschnitt 40 Messwerte
  # pro Brunnen, im Median 80 m³/h momentane Förderleistung


  }


# MAIN 4: drilling and surface water body information --------------------------

if (FALSE) {

  # read data
  df_drilling <- read_select_rename(path_db, "WV_GMS_TBL_BOHRUNGEN", renamings$main)

}

# MAIN 5: water quality data ---------------------------------------------------

if (FALSE) {

  # read data
  df_quality <- read_select_rename(path_db, "DB2LABOR_Daten", renamings$quality)

  # aggregate / clean data
  df_quality_agg <- df_quality %>%
    dplyr::filter(Parameter %in% renamings_quality$old_name) %>%
    dplyr::mutate(Parameter = renamings_quality$new_name[match(Parameter, renamings_quality$old_name)]) %>%
    dplyr::mutate(dat_Wasserchemie = as.Date(dat_Wasserchemie, format = "%Y-%m-%d")) %>%
    dplyr::group_by(id_Brunnen, Parameter) %>%
    dplyr::summarise(median = median(Wert, na.rm = TRUE),
                     std_dev = sd(Wert, na.rm = TRUE),
                     number = n()) %>%
    dplyr::filter(Parameter != "mw_GV") %>%  # discard Gluehverlust data (only 11 wells with observations)
    dplyr::select(id_Brunnen, Parameter, median) %>%
    tidyr::pivot_wider(names_from = Parameter, values_from = median,
                       id_cols = "id_Brunnen") %>%
    data.frame()

}




# MAIN 6: join data ------------------------------------------------------------

if (FALSE) {

  # verknuepfen

  df <- df_pump_tests %>%
    dplyr::left_join(df_main, by = "id_Brunnen") %>%
    dplyr::left_join(df_Q_agg, by = "id_Brunnen") %>%
    dplyr::right_join(df, df2, by = "id_Brunnen")

  str(df)
  data.frame(table(df$fil_Brunnenfunktion))
}


# MAIN 7: very first data analysis ---------------------------------------------

if (FALSE) {

  # export material lists

  a <- data.frame(table(df_main$mw_Vollrohrmaterial, useNA = "ifany")) %>%
    dplyr::arrange(-Freq)
  write.table(a, file = "Materialien_Vollrohr.csv", dec = ".", sep = ";", row.names = FALSE)
  b <- data.frame(table(df_main$mw_Inliner, useNA = "ifany")) %>%
    dplyr::arrange(-Freq)
  c <- data.frame(table(df_main$mw_Filtermaterial, useNA = "ifany")) %>%
    dplyr::arrange(-Freq)
  write.table(c, file = "Materialien_Filter.csv", dec = ".", sep = ";", row.names = FALSE)


  # plot Q measurements
    ggplot2::ggplot(df_Q_agg, ggplot2::aes(x = Qmom_median)) +
      ggplot2::geom_histogram(fill = "lightblue", binwidth = 5) +
      sema.berlin.utils::my_theme() +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
      ggplot2::labs(x = "Q_obs_median [m³/h]", y = "Frequency [-]")
    summary(df_Q_agg)

    ggplot2::ggsave("histogram_Ergiebigkeit_Q_obs.png", width = 4, height = 2.5, dpi = 600)



  # first data analysis
  data.frame(table(df$well_function, useNA = "ifany"))
  a <- data.frame(table(df[df$well_function == "Betriebsbrunnen betriebsbereit", "surface_water_name"], useNA = "ifany")) %>% arrange(-Freq)
  data.frame(table(df$surface_water_distance, useNA = "ifany"))


  df <- read_ms_access(path_db, "WV_GMS_TBL_GWBR")

  str(df)

  # make all column names upper case
  colnames(df) <- toupper(colnames(df))

  # select defined columns in df and renamings
  df <- df[, colnames(df) %in% renamings$old_name]
  renamings <- renamings[renamings$old_name %in% colnames(df),]

  # rename columns
  df <- df %>% dplyr::rename(setNames(renamings$old_name, renamings$new_name))


  str(df)
  sort(colnames(df))

}
