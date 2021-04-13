# source("C:/Users/mriech/Desktop/R_Development/RScripts/SEMA/UseCase_Berlin/1_DataPreparation/SEMA_PrepareData_Berlin.R")

# links:
# https://www.brodrigues.co/blog/2020-03-08-tidymodels/
# https://www.r-bloggers.com/2021/02/machine-learning-with-r-a-complete-guide-to-gradient-boosting-and-xgboost/
# https://www.r-bloggers.com/2021/02/deep-learning-with-r-and-keras-build-a-handwritten-digit-classifier-in-10-minutes/
# https://www.r-bloggers.com/2021/02/getting-started-with-k-means-and-tidytuesday-employment-status/
# https://www.r-bloggers.com/2021/03/k-means-101-an-introductory-guide-to-k-means-clustering-in-r/



# Paths and packages -----------------------------------------------------------

library(dwc.wells)

path_list <- list(
  db = file.path(kwb.utils::desktop(), "tmp/DWC/wells/BWB_WV_Brunnenexport_2017.mdb"),
  renamings = extdata_file("renamings"),
  #renamings_main = "<renamings>/Parameterliste.xlsx",
  renamings_main = "<renamings>/main.csv",
  renamings_material_filter = "<renamings>/material_filter.csv",
  renamings_material_vollrohr = "<renamings>/material_vollrohr.csv",
  lookup_actions = "<renamings>/actions.csv",
  renamings_quality = "<renamings>/quality.csv"
)

paths <- kwb.utils::resolve(path_list)


# MAIN 0: Read renamings -------------------------------------------------------

if (TRUE) {

  # load renamings
  renamings <- list(
    #main = load_renamings_excel(paths$renamings_main),
    main = load_renamings_csv(paths$renamings_main),
    quality = load_renamings_csv(paths$renamings_quality),
    material_filter = load_renamings_csv(paths$renamings_material_filter),
    material_vollrohr = load_renamings_csv(paths$renamings_material_vollrohr)
  )

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

  # group filter material
  df_main$Filtermaterial <- rename_values(df_main$Filtermaterial,
                                             renamings$material_filter)

  # group vollrohr material
  df_main$Vollrohrmaterial <- rename_values(df_main$Vollrohrmaterial,
                                             renamings$material_vollrohr)


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


  # summarise marginal factor levels
  df_main$Gewaesser <- df_main$Gewaesser %>%
    summarise_marginal_factor_levels(perc_threshold = 3,
                                     marginal_name = "Andere")


  # Gebiet short name
  df_main$Gebiet_short <- substr(df_main$Name_Brunnen, 1, 3)
  table(df_main$Gebiet, df_main$Gebiet_short)


  # first plot

  variables_cat <- c("Zustand_Brunnen",
                     "Filtermaterial",
                     "Gebiet",
                     "Spannung",
                     "Gewaesser")


  plots_cat <- lapply(variables_cat, function(x) {
    plot_frequencies(df_main, x, gsub("_", " ", x), 0.05)
    })




  plot_frequencies(df_main, "Gewaesser")

  ggplot2::ggsave("frequency_plot_Gewaesser_v2.png", width = 5,
                  height = 6, dpi = 600)


  plot_num_1 <- plot_distribution(Data = df_main,
                                   variable = "Jahr_Inbetriebnahme",
                                   title = "Jahr Inbetriebnahme",
                                   vertical_x_axis_labels = FALSE) +
    ggplot2::scale_x_continuous(limits = c(1920, 2020),
                                breaks = scales::pretty_breaks())

  plot_num_2 <- plot_distribution(Data = df_main,
                                   variable = "Qs_neu",
                                   title = "Qs (neu)",
                                   vertical_x_axis_labels = FALSE)

  plot_num_3 <- plot_frequencies(Data = df_main,
                                variable = "Filteranzahl",
                                title = "Filteranzahl",
                                vertical_x_axis_labels = FALSE)


  plot_num_4 <- plot_distribution(Data = df_main,
                    variable = "Durchmesser",
                    binwidth = 50,
                    title = "Durchmesser",
                    vertical_x_axis_labels = FALSE)

  plots <- cowplot::plot_grid(plotlist = c(plots_cat,
                                  list(plot_num_1, plot_num_2,
                                       plot_num_3, plot_num_4)),
                     nrow = 3, align = "hv", scale = 0.9)

  ggplot2::ggsave("frequency_plots_v3.png", plot = plots, width = 20,
                  height = 20, dpi = 600)


  table(df_main$Zustand_Brunnen, df_main$Spannung)
  table(df_main$Zustand_Brunnen, df_main$Gewaesser)


  htmlwidgets::saveWidget(widget = plotly::ggplotly(plot_num_1),
                          file = "frequency_plot_Inbetriebnahme.html",
                          selfcontained = TRUE)

  htmlwidgets::saveWidget(widget = plotly::ggplotly(plot_num_2),
                          file = "frequency_plot_Qs_neu.html",
                          selfcontained = TRUE)

  htmlwidgets::saveWidget(widget = plotly::ggplotly(plot_num_4),
                          file = "frequency_plot_Durchmesser.html",
                          selfcontained = TRUE)
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
      grepl("Liner|liner|Inliner|inliner|Lining|lining", Bemerkung),
      "Ja",
      "Nein")
      ) %>%
    dplyr::select(c("id_Brunnen", "Datum_Inbetriebnahme", "Qs_neu",
                    "Datum_KPVvor", "Qs_KPVvor", "Qs_KPVvor_rel",
                    "Datum_KPVnach", "Qs_KPVnach", "Qs_KPVnach_rel",
                    "Regenerierung", "Pumpenwechsel", "Manschette"))

  # write.table(df_pump_tests, file = "Q_s_Berechnungen_v2.csv", dec = ".", sep = ";", row.names = FALSE)

}


# MAIN 3: Q measurement data ---------------------------------------------------

  if (FALSE) {

  # read data
  df_Q <- read_select_rename(paths$db,
                             "WV_BRU_TBL_ERG",
                             renamings$main,
                             old_name_col = "old_name",
                             new_name_col = "new_name_de")

  df_Q <- df_Q %>% dplyr::left_join(df_main[, c("id_Brunnen", "Qzul")]) %>%
    dplyr::mutate(Ratio_Qmom_Qzul = Qmom / Qzul) %>%
    dplyr::arrange(-Ratio_Qmom_Qzul)

  write.table(df_Q, file = "Qmom_Qzul_ratio.csv", dec = ".", sep = ";", row.names = FALSE)

  prop.table(table(df_Q$Ratio > 1.3))

  p1 <- ggplot2::ggplot(df_Q, ggplot2::aes(x = Qmom/Qzul, y = stat(count) / sum(stat(count)))) +
    ggplot2::geom_histogram(binwidth = 0.1, fill = "grey", col = "white", boundary = 1) +
    ggplot2::scale_x_continuous(limits = c(0, 2)) +
    ggplot2::scale_y_continuous(name = "Percentage",
                                breaks = scales::pretty_breaks(),
                                labels = scales::percent_format(accuracy = 1)) +
    sema.berlin.utils::my_theme()
  plotly::ggplotly(p1)


  l <- lapply(seq(0, 2, 0.1), function(x) table(df_Q$Ratio > x))
  names(l) <- sprintf("%3.1f", seq(0, 2, 0.1))
  df <- data.frame(do.call("rbind", l))
  colnames(df) <- c("valid", "invalid")
  df$threshold <- rownames(df)
  df <- tidyr::pivot_longer(data = df, cols = c("valid", "invalid"))
  #df$name <- factor(df$name, levels = c("valid", "invalid"))
  df$name <- factor(df$name, levels = c("invalid", "valid"))

  p2 <- ggplot2::ggplot(df, ggplot2::aes(x = threshold, y = value, fill = name)) +
    ggplot2::geom_bar(stat = "identity", position = "fill") +
    sema.berlin.utils::my_theme(legend.position = "top",
                                axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    ggplot2::scale_fill_manual(values = c("coral", "darkseagreen3")) +
    ggplot2::scale_x_discrete(expand = c(0.05, 0.05)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(),
                                breaks = scales::pretty_breaks()) +
    ggplot2::labs(x = 'Threshold "Qmom/Qzul"', y = "Percentage", fill = "")

  p2

  ggplot2::ggsave("Qmom_Qzul_threshold.png", p2, dpi = 600, height = 4, width = 6)
 p2

  cowplot::plot_grid(p1, p2)

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


# MAIN 4: water quality data ---------------------------------------------------

if (FALSE) {

  # read data
  df_quality <- read_select_rename(path_db, "DB2LABOR_Daten", renamings$quality)

  # aggregate / clean data
  df_quality_agg <- df_quality %>%
    dplyr::filter(Parameter %in% renamings_quality$old_name) %>%
    dplyr::mutate(Parameter = renamings_quality$new_name[match(Parameter, renamings_quality$old_name)]) %>%
    dplyr::mutate(Datum_Wasserchemie = as.Date(Datum_Wasserchemie, format = "%Y-%m-%d")) %>%
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
