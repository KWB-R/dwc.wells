
# Paths and packages -----------------------------------------------------------

library(dwc.wells)

path_list <- list(

  # mas access database ---
  db = file.path(kwb.utils::desktop(), "tmp/DWC/wells/Data/01_ms_access",
                 "BWB_WV_Brunnenexport_2017.mdb"),

  # csv data ---
  csv_data = file.path(kwb.utils::desktop(), "tmp/DWC/wells/Data/02_csv"),
  data_wells = "<csv_data>/GWBR_02.csv",
  data_drilling = "<csv_data>/Bohrungen_02.csv",
  data_drilling_tech = "<csv_data>/Bohrtechnik.csv",
  data_pump_tests = "<csv_data>/Arbeitsberichte.csv",
  data_W_static = "<csv_data>/RWS.csv",
  data_quantity = "<csv_data>/ERG_02.csv",
  data_operational_hours = "<csv_data>/Betriebsstunden_zw_Regenerierungen.csv",
  #data_quality = "<csv_data>/___.csv",
  #data_quality_para1 = "<csv_data>/LIMS_Para.csv",
  #data_quality_para2 = "<csv_data>/LIMS_PM.csv",
  data_kf_ = "<csv_data>/KF-Werte.csv",
  data_kf = "<csv_data>/kf_mean_filled_HSc.csv",

  # renamings ---
  renamings = dwc.wells::extdata_file("renamings"),
  renamings_main = "<renamings>/main.csv",
  renamings_screen_material = "<renamings>/screen_material.csv",
  renamings_casing_material = "<renamings>/casing_material.csv",
  renamings_well_function = "<renamings>/well_function.csv",
  renamings_waterworks = "<renamings>/waterworks.csv",
  renamings_surface_water = "<renamings>/surface_water.csv",
  renamings_quality = "<renamings>/quality.csv",
  lookup_actions = "<renamings>/actions.csv"
)

paths <- kwb.utils::resolve(path_list)


# MAIN 0: Read renamings -------------------------------------------------------

if (TRUE) {

  # load renamings

  elements <- grep("renamings_", x = names(paths), value = TRUE)
  renamings <- lapply(elements, function(x) { load_renamings_csv(paths[[x]]) })
  names(renamings) <- gsub("renamings_", "", elements)

  #lookup <- list(actions = load_renamings_csv(paths$lookup_actions))

}


# MAIN 1: general well characteristics -----------------------------------------

if (FALSE)
  {

  # read data from csv and filter Vertikalfilterbrunnen
  df_wells <- read_csv(paths$data_wells, skip = 9) %>%
    select_rename_cols(renamings$main, "old_name", "new_name_en") %>%
    dplyr::filter(grepl("V$", well_name))


  # check for duplicates
  if (FALSE) {
    sum(duplicated(df_wells$site_id)) # duplicates: rehabilitated wells get site id 11
    sum(duplicated(df_wells$well_id)) # no duplicates
    sum(duplicated(df_wells$drilling_id)) # no duplicates
  }


  # assign data type "date"
  date_cols <- c("construction_date", "operational_start.date",
                 "operational_state.date", "inliner.date")
  df_wells <- df_wells %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(date_cols), .fns = as.Date, format = "%Y-%m-%d"),
                  monitoring.date = as.Date(monitoring.date, format = "%d.%m.%Y"))


  # remove outliers in dates and numerical data
  df_wells <- df_wells %>%
    dplyr::mutate(
      # false dates
      dplyr::across(dplyr::all_of(date_cols), .fns = dplyr::na_if, "1899-12-30"),
      # specific capacity at operational start
      operational_start.Qs = dplyr::na_if(operational_start.Qs, 469)
    )


  # replace missing construction dates with operational_start.date
  cd_missing <- is.na(df_wells$construction_date)
  df_wells[cd_missing, "construction_date"] <- df_wells[cd_missing, "operational_start.date"]


  # recalcuate Qs, as there are 97 wells with no Qs but with Q, W_dynamic, W_static
  df_wells <- df_wells %>%
    dplyr::mutate(
      operational_start.Qs = operational_start.Q /
        (operational_start.W_dynamic - operational_start.W_static))


  # calculate years from date
  df_wells <- df_wells %>%
    dplyr::mutate(
      construction_year = lubridate::year(construction_date),
      operational_start.year = lubridate::year(operational_start.date)
    )


  # clean categorical variables
  df_wells <- df_wells %>%
    dplyr::mutate(
      dplyr::across(tidyr::starts_with("aquifer"), ~dplyr::na_if(., "nicht bekannt")),
      n_screens = dplyr::na_if(n_screens, 0) %>% tidy_factor(level_sorting = "alphabet")
    )


  # group categorical variables according to lookup table
  vars <- c("screen_material", "casing_material", "well_function", "waterworks")
  for (var in vars) {
    df_wells[[var]] <- rename_values(df_wells[[var]], renamings[[var]])
  }


  # create new categorical variables
  df_wells <- df_wells %>%
    dplyr::mutate(
      inliner = factor(ifelse(!is.na(inliner.date), "Yes", "No"),
                       levels = c("Yes", "No")),
      well_gallery = substr(well_name, 1, 7)
    )


  # assign data type "factor"
  factor_cols <- c("well_function", "operational_state", "aquifer_confinement",
                   "aquifer_coverage", "casing_material", "screen_material",
                   "waterworks")
  df_wells <- df_wells %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(factor_cols), .fns = tidy_factor))


  # check if well is replaced / 1st or 2nd generation
  sum(!is.na(df_wells$well_id.after_replacement))
  # -> all ids in well.id_after replacement empty
  sum(!is.na(df_wells$well_id.after_replacement[!is.na(df_wells$well_id_replaced)]))
  # -> no 3rd generation
  df_wells <- df_wells %>% dplyr::select(-well_id.after_replacement)

  # create variable 'well_generation'
  df_wells$well_generation <- ifelse(is.na(df_wells$well_id_replaced),1, 2)
  frequency_table(df_wells$well_generation)
  table(df_wells$well_generation, df_wells$operational_state)
  }


# MAIN 2: surface water body information (drilling) ----------------------------

if (FALSE)
{

# read drilling data from csv
# does not work with standard encoding
df_drilling <- read.csv(file = paths$data_drilling, header = TRUE,
                        skip = 2, dec = ".", sep = "\t", na.strings = "(null)") %>%
  select_rename_cols(renamings$main, "old_name", "new_name_en")

# adapt encoding for surface water name (otherwise umlaute will not be read)
Encoding(df_drilling$surface_water) <- "UTF-8"


# regroup and tidy surface waters
df_drilling <- df_drilling %>%
  dplyr::mutate(surface_water =
                  rename_values(surface_water, renamings$surface_water) %>%
                  tidy_factor)


# tidy distance to surface water
df_drilling <- df_drilling %>%
  dplyr::mutate(surface_water.distance =
                  tidy_factor(surface_water.distance) %>%
                  factor(levels = c("0-25", "25-50", "50-100", "100-200",
                                    "200-500", "500-1000", ">1000", "Unbekannt"))
  )


# read drilling tech data from csv
df_drilling_tech <- read_csv(paths$data_drilling_tech) %>%
  select_rename_cols(renamings$main,"old_name", "new_name_en")

# ...information on drilling technique still missing


}


# MAIN 3: water quality data ---------------------------------------------------

if (FALSE)
  {

  # read data
  df_quality <- dwc.wells:::read_ms_access_mri(paths$db, "DB2LABOR_Daten") %>%
    select_rename_cols(renamings$main, "old_name", "new_name_en")


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


  # append quality to colnames
  colnames(df_quality_agg)[-1] <- paste0("quality.", colnames(df_quality_agg)[-1])


  if (FALSE) {
    write.table(df_quality_agg, file = "quality_measurements_agg.csv", dec = ".", sep = ";", row.names = FALSE)
  }

  lookup_par_unit <- aggregate(quality.unit~quality.parameter, df_quality, FUN = unique)

  df_quality_agg_long <- df_quality_agg %>%
    tidyr::pivot_longer(cols = -1, names_to = "quality.parameter", values_to = "quality.value") %>%
    dplyr::left_join(lookup_par_unit)

}


# MAIN tmp: append kf values ---------------------------------------------------

if (FALSE) {

  df_kf <- read_csv(file = paths$data_kf, sep = ";", dec = ",", skip = 0)

  df_kf_ <- read_csv(file = paths$data_kf_, skip = 2)
  sum(unique(df_wells$drilling_id) %in% unique(df_kf_$BOHRUNGSNUMMER))
  sum(unique(df_kf_$BOHRUNGSNUMMER) %in% unique(df_wells$drilling_id))
  kf_drilling_ids <- unique(df_kf_$BOHRUNGSNUMMER)
  kf_drilling_ids_wells <- kf_drilling_ids[kf_drilling_ids %in% unique(df_wells$drilling_id)]
  table(df_wells$operational_state, data_available = df_wells$drilling_id %in% kf_drilling_ids)
  table(df_wells$operational_state, useNA = "ifany")


  names(df_kf) <- c("well_name", "kf")
  length(unique(df_kf_$BOHRUNGSNUMMER))
  length(unique(df_kf_$BOHRUNGSNUMMER))
  table(df_wells$operational_state, kf_available = !is.na(df_wells$kf))

  sum(df_kf$well_name %in% df_wells$well_name)
  df_wells <- df_wells %>% dplyr::left_join(df_kf, by = "well_name")
  str(df_wells$kf)
  summary(df_wells$kf)
}

# MAIN 4: combine well characteristics, surface water information and water quality data

  if (FALSE) {

    # append water body data
    df_wells <- df_wells %>%
      dplyr::left_join(df_drilling, by = "drilling_id") %>%
      dplyr::left_join(df_quality_agg, by = "well_id")


    #save(df_wells, file = "df_wells.RData")




    # variables for plotting well characteristics
    well_vars <- c("operational_start.year",
                   "well_function",
                   "operational_state",
                   "waterworks",
                   "admissible_discharge",
                   "operational_start.Qs",
                   "aquifer_confinement",
                   "aquifer_coverage",
                   "screen_material",
                   "inliner",
                   #"inliner.date",
                   "diameter",
                   "n_screens",
                   "surface_water",
                   "surface_water.distance",
                   "quality.Cu",
                   "quality.DR",
                   "quality.Fe_tot",
                   "quality.Mn",
                   "quality.NO3",
                   "quality.P_tot",
                   "quality.pH",
                   "quality.PO4",
                   "quality.Redox",
                   "quality.SO4",
                   "quality.Temp",
                   "quality.TSS")


    df_wells_1 <- df_wells %>% dplyr::select(dplyr::all_of(well_vars))

  # select columns with model variables
  model_vars <- c("site_id",
                  "well_name",
                     "waterworks",
                     "well_gallery",
                     "aquifer_coverage",
                     "aquifer_confinement",
                     "screen_material",
                     "diameter",
                     "n_screens",
                     "surface_water",
                     "surface_water.distance",
                     "inliner.date")


  df_wells_vars <- df_wells %>% dplyr::select(dplyr::all_of(relevant_vars))
  #write.table(df_wells_var, file = "Stammdaten_aufbereitet_Auswahl.csv", dec = ".", sep = ";", row.names = FALSE)

  }

# MAIN 5: capacity measurements (virtual pump tests) ---------------------------

if (FALSE)
  {

   # read quantity measurement data
  df_Q <- read_csv(paths$data_quantity, skip = 26) %>%
    select_rename_cols(renamings$main, "old_name", "new_name_en") %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::mutate(date = dplyr::na_if(date, "1899-12-30 00:00:00")) %>%
    tidyr::drop_na(-W_static) %>%
    dplyr::distinct(.keep_all = TRUE)


  # get static water level data from df_wells
  df_W_static_1 <- df_wells %>%
    dplyr::select(site_id, operational_start.date, operational_start.W_static, monitoring.date, monitoring.W_static) %>%
    tidyr::pivot_longer(-site_id, names_sep = "\\.", names_to = c("origin", ".value")) %>%
    dplyr::mutate(W_static = dplyr::na_if(W_static, 0)) %>%
    dplyr::filter(!is.na(W_static))

  # import other static water level data provided by Sebastian Schimmelpfennig
  # origin: H2O2-Messungen, Kurzpumpversuche, Ergiebigkeitsmessungen
  df_W_static_2 <- read_csv(paths$data_W_static, skip = 30) %>%
    select_rename_cols(renamings$main, "old_name", "new_name_en") %>%
    dplyr::mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    dplyr::select(site_id, origin, date, W_static)

  # combine both data sources
  df_W_static <- dplyr::bind_rows(list(df_W_static_1, df_W_static_2)) %>%
    tidyr::drop_na(site_id, date, W_static) %>% # one date missing
    dplyr::arrange(site_id, date) %>%
    dplyr::rename(W_static.origin = origin)

  summary <- df_W_static %>% dplyr::group_by(site_id) %>%
    dplyr::summarise(n_valid = sum(!is.na(W_static))) %>%
    dplyr::arrange(dplyr::desc(n_valid))


  # combine quantity data and extra static water level data
  df_Q_W <- df_Q %>%
    dplyr::mutate(W_static.origin = ifelse(
      !is.na(W_static), "quantity_measurements", NA
    )) %>%
    dplyr::bind_rows(df_W_static) %>%
    dplyr::arrange(site_id, date)


  # Remove duplicate across site_id and date
  df_Q_W <- df_Q_W %>%
    dplyr::distinct(site_id, date, .keep_all = TRUE)

  frequency_table(df_Q_W[!is.na(df_Q_W$W_static), "W_static.origin"])

  # remove rows for wells with no static water level data at all
  df_Q_W <- df_Q_W %>%
    dplyr::group_by(site_id) %>%
    dplyr::filter(!all(is.na(W_static)))


  # check number of non-NA values per site
  summary2 <- df_Q_W %>% dplyr::group_by(site_id) %>%
    dplyr::summarise(n_valid = sum(!is.na(W_static))) %>%
    dplyr::count(n_valid >= 1)


  # interpolate and fill up static water level
  df_Q_W_new <- interpolate_and_fill(
    df_Q_W, x_col = "date", y_col = "W_static",
    group_by_col = "site_id", origin_col = "W_static.origin"
  )


  # show origin of data
  frequency_table(df_Q_W_new[!is.na(df_Q_W_new$W_static), "W_static.origin"])

  # group types for W_static
  df_Q_W_new <- df_Q_W_new %>%
    dplyr::mutate(W_static.origin =
                    forcats::fct_collapse(
                      W_static.origin,
                      measured = setdiff(unique(W_static.origin),
                                         c("interpolated", "filled up"))) %>%
                    forcats::fct_relevel("measured", "interpolated", "filled up"),
                  full_data_set = !is.na(Q) & !is.na(W_dynamic)
    )


  # clean outliers and remove NA
  df_Q_W_new <- df_Q_W_new %>%
    dplyr::mutate(Q = ifelse(Q > 1000 | Q == 0, NA, Q),
                  W_dynamic = ifelse(W_dynamic > 50 | W_dynamic == 0, NA, W_dynamic)
    ) %>%
    tidyr::drop_na(Q, W_dynamic) %>%
    dplyr::select(-W_static.incomplete)


  # calculate Qs and remove negative values
  df_Q_W_new <- df_Q_W_new %>%
    dplyr::mutate(Qs = Q / (W_dynamic - W_static))


  # join with Qs from operational start and calculate Qs_rel
  df_Q_W_new <- df_Q_W_new %>%
    dplyr::left_join(df_wells[, c("site_id",
                                  "well_id",
                                  "operational_start.date",
                                  "operational_start.Qs")],
                     by = "site_id") %>%
    # calculate Qs_rel
    dplyr::mutate(Qs_rel =  Qs / operational_start.Qs) %>%
    dplyr::select(-operational_start.Qs)


    # check ratio Qmom / Qzul
  if (FALSE) {
    df_Q <- df_Q %>%
      dplyr::left_join(df_main[, c("well_id", "admissible_discharge")]) %>%
      dplyr::mutate(Ratio_Qmom_Qzul = Q / admissible_discharge)

    write_csv(df_Q, "Qmom_Qzul_ratio.csv")
  }

}

# MAIN 6: pump test data -------------------------------------------------------

if (FALSE) {

  # select columns from well data to be joined with pump test data
  # for further calculations
  df_wells_operational_start <- df_wells %>%
    dplyr::select("well_id", tidyselect::starts_with("operational_start."))


  # read, rename and clean data ---
  df_pump_tests <- read_csv(paths$data_pump_tests, skip = 2) %>%
    select_rename_cols(renamings$main, "old_name", "new_name_en") %>%
    # filter data for site id 11 (not unique, used for rehabilitated wells)
    dplyr::filter(site_id != 11) %>%
    # delete rows with same site_id, pump test_1.date and pump_test_2.date
    dplyr::distinct_at(c("site_id", "pump_test_1.date", "pump_test_2.date"),
                       .keep_all = TRUE) %>%
    # assign date format to dates
    dplyr::mutate(pump_test_1.date = as.Date(pump_test_1.date, format = "%Y-%m-%d"),
                  pump_test_2.date = as.Date(pump_test_2.date, format = "%Y-%m-%d")) %>%
    # delete row if both values are NA
    dplyr::filter(!(is.na(pump_test_1.date) & is.na(pump_test_2.date))) %>%
    # delete data with pump test date in the future
    dplyr::filter(pump_test_1.date < Sys.Date()| pump_test_2.date < Sys.Date())

  }

  # swap pump test dates 1 and 2 if pump_test_2.date < pump_test_1.date --------

  # check, how many rows have dates in wrong order
  cond <- dwc.wells:::swapped_dates(df_pump_tests)
  dwc.wells:::check_swapped_dates(cond)

  # swap dates
  df_pump_tests <- df_pump_tests %>%
    dplyr::mutate(
      pump_test_1.date_tmp = dplyr::if_else(cond, pump_test_2.date, pump_test_1.date),
      pump_test_2.date_tmp = dplyr::if_else(cond, pump_test_1.date, pump_test_2.date)
    ) %>%
    dplyr::mutate(pump_test_1.date = pump_test_1.date_tmp,
                  pump_test_2.date = pump_test_2.date_tmp) %>%
    dplyr::select(-c(pump_test_1.date_tmp, pump_test_2.date_tmp))

  # check again, how many dates are in wrong order
  dwc.wells:::check_swapped_dates(dwc.wells:::swapped_dates(df_pump_tests))


  # fill up pump test dates and calculate action date --------------------------
  df_pump_tests <- df_pump_tests %>%
    # add date column not containing NAs (required for creating an "action_id")
    dplyr::mutate(
      interval_days = dplyr::if_else(
        !is.na(pump_test_1.date) & !is.na(pump_test_2.date),
        dwc.wells:::real_interval(pump_test_2.date, pump_test_1.date),
        dwc.wells:::default_interval(pump_test_2.date, pump_test_1.date, func = mean)
      ),
      interval_type = dplyr::if_else(
        !is.na(pump_test_1.date) & !is.na(pump_test_2.date), "real", "default"
      ),
      pump_test_1.date = dplyr::if_else(
        is.na(pump_test_1.date) & !is.na(pump_test_2.date),
        pump_test_2.date - interval_days,
        pump_test_1.date
      ),
      pump_test_2.date = dplyr::if_else(
        is.na(pump_test_2.date) & !is.na(pump_test_1.date),
        pump_test_1.date + interval_days,
        pump_test_2.date
      ),
      action_date = pump_test_1.date + ceiling(interval_days / 2)
    )


  # correct ids for replaced wells ---------------------------------------------

  # join relevant ids and construction date from well data
  cols <-  c("site_id", "well_id", "well_id_replaced", "construction_date")
  df_pump_tests <- df_pump_tests %>% dplyr::left_join(df_wells[, cols], by = "site_id")


  # use well id of replaced well if pump test date < construction date
  if (FALSE) {
    ptd1 <- df_pump_tests$pump_test_1.date
    ptd2 <- df_pump_tests$pump_test_2.date
    cd <- df_pump_tests$construction_date
    cond <- (ptd1 < cd & ptd2 < cd | ptd1 < cd & is.na(ptd2) | is.na(ptd1) & ptd2 < cd) & !is.na(cd)
  }

  cond <- which(df_pump_tests$pump_test_1.date < df_pump_tests$construction_date |
                  df_pump_tests$pump_test_2.date < df_pump_tests$construction_date)

  df_pump_tests[cond, "well_id"] <- df_pump_tests[cond, "well_id_replaced"]

  # delete unrequired columns, further on use 'well_id' as join column
  df_pump_tests <- df_pump_tests %>%
    dplyr::select(-c("site_id", "well_id_replaced", "construction_date"))


  # calculate Qs and Qs rel ----------------------------------------------------
  df_pump_tests <- df_pump_tests %>%
    # get well characteristics to calculate Qs_rel
    dplyr::inner_join(df_wells_operational_start, by = "well_id") %>%
    # discard data without Qs as there will be no reference for pump test data
    dplyr::filter(!is.na(operational_start.Qs)) %>%
    # calculate Qs and Qs_rel for pump tests 1 and 2
    dplyr::mutate(pump_test_1.Qs = pump_test_1.Q /
                    (pump_test_1.W_dynamic - pump_test_1.W_static),
                  pump_test_1.Qs_rel =  pump_test_1.Qs / operational_start.Qs,
                  pump_test_2.Qs = pump_test_2.Q /
                    (pump_test_2.W_dynamic - pump_test_2.W_static),
                  pump_test_2.Qs_rel =  pump_test_2.Qs / operational_start.Qs
    )


  # derive action type ---------------------------------------------------------

  df_pump_tests <- df_pump_tests %>%
    # give action id
    dplyr::arrange(well_id, action_date) %>%
    dplyr::group_by(well_id) %>%
    dplyr::mutate(action_id = dplyr::row_number()) %>%
    # check if pump test is associated with regeneration (three types)
    dplyr::mutate(pump_test_2.well_rehab = (well_rehab.general + well_rehab.shock +
                                              well_rehab.hydropulse) != 0,
                  pump_test_2.substitute_pump = substitute_pump != 0,
                  pump_test_2.pressure_sleeve =  pressure_sleeve != 0) %>%
    dplyr::mutate(pump_test_2.comment_liner = ifelse(
      grepl("Liner|liner|Inliner|inliner|Lining|lining", well_rehab.comment), TRUE, FALSE
    ))


  # select relevant columns ----------------------------------------------------
  df_pump_tests <- df_pump_tests %>%
    # select important variables
    dplyr::select("well_id",
                  "action_id",
                  "action_date",
                  tidyselect::starts_with("interval_"),
                  tidyselect::starts_with("operational_start"),
                  tidyselect::starts_with("pump_test")
                  #-tidyselect::ends_with(c("Q", "W_static", "W_dynamic"))
                  )


  # tidy data ------------------------------------------------------------------

  cols_to_longer <- df_pump_tests %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyr::starts_with(c("operational_start", "pump_test"))) %>%
    names()

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
                  action_date = dplyr::if_else(key == "operational_start",
                                             date,
                                             action_date),
                  Qs_rel = dplyr::if_else(key == "operational_start",
                                          1,
                                          Qs_rel)
                  ) %>%
    dplyr::arrange(well_id, action_id) %>%

    # join  dates of operational start to calculate time differences
    dplyr::left_join(df_wells_operational_start %>%
                     dplyr::select(well_id, operational_start.date),
                     by = "well_id") %>%
    dplyr::mutate(days_since_operational_start =
                    as.integer(
                      difftime(date, operational_start.date, units = "days")
                      )) %>%
    # filter rows for Alt-Brunnen
    dplyr::filter(days_since_operational_start >= 0) %>%
    dplyr::group_by(well_id) %>%
    dplyr::mutate(n_rehab = dwc.wells:::cumsum_no_na(well_rehab),
                  n.substitute_pump = dwc.wells:::cumsum_no_na(substitute_pump),
                  n.pressure_sleeve = dwc.wells:::cumsum_no_na(pressure_sleeve),
                  n.comment_liner = dwc.wells:::cumsum_no_na(comment_liner)
                  ) %>%
   dplyr::ungroup() %>%
   dplyr::arrange(well_id, n_rehab, date) %>%
   dplyr::group_by(well_id, n_rehab) %>%
   dplyr::mutate(
     last_rehab.date = min(action_date),
     days_since_last_rehab =  dplyr::if_else(
     n_rehab > 0,
     as.integer(date - last_rehab.date),
     days_since_operational_start
     ))


  # test, if still some negative ages -> no
  nrow(df_pump_tests_tidy[df_pump_tests_tidy$days_since_operational_start < 0,])
  nrow(unique(df_pump_tests_tidy[df_pump_tests_tidy$days_since_operational_start < 0, "well_id"]))


  # recalculate days since operational start and days since last rehab into
  # new variables well_age_years and years_since_last_rehab

  df_pump_tests_tidy <- df_pump_tests_tidy %>%
    dplyr::mutate(well_age_years = days_since_operational_start / 365.25,
                  years_since_last_rehab = days_since_last_rehab / 365.25)


  if (FALSE) {
    write.table(df_pump_tests_tidy[, relevant_cols], file = "pump_test_data.csv", dec = ".", sep = ";", row.names = FALSE)
  }


  # combine pump test data with capacity measurements (virtual pump test, MAIN 2)


  if (FALSE) {

    df_Qs_all <- df_pump_tests_tidy %>%
      dplyr::bind_rows(df_Q_W_new) %>%
      dplyr::arrange(well_id, date) %>%
      dplyr::select(c(dplyr::all_of(relevant_cols), "W_static.origin")) %>%
      dplyr::filter(!is.na(date)) %>%
      dplyr::ungroup()


    # complete data
    df_Qs_all <- df_Qs_all %>%
      tidyr::fill(n_rehab) %>%
      tidyr::fill(last_rehab.date) %>%
      dplyr::mutate(key = dplyr::if_else(!is.na(W_static.origin),
                                         "quantity measurements",
                                         key),
                    key2 = forcats::fct_collapse(
                      key,
                      'pump tests' = c("operational_start", "pump_test_1", "pump_test_2")
                    ),
                    W_static.origin = tidyr::replace_na(W_static.origin, "measured"),
                    days_since_operational_start = as.integer(
                      difftime(date, operational_start.date, units = "days")
                    )) %>%
      dplyr::group_by(well_id, n_rehab) %>%
      dplyr::mutate(days_since_last_rehab =  dplyr::if_else(
        is.na(days_since_last_rehab),
        as.integer(date - min(last_rehab.date)),
        days_since_last_rehab
      )) %>%
      dplyr::ungroup()
  }


  # select columns and join with well data

  relevant_cols <- c(
    "well_id",
    "date",
    "key",
    #"key2",
    "Qs_rel",
    "operational_start.date",
    "days_since_operational_start",
    "well_age_years",
    "n_rehab",
    "last_rehab.date",
    "days_since_last_rehab",
    "years_since_last_rehab")

  df_Qs_all <- df_pump_tests_tidy

  df_Qs_all <- df_Qs_all %>%
    dplyr::select(relevant_cols) %>%
    dplyr::left_join(dplyr::select(df_wells, -"operational_start.date"), by = "well_id")



  frequency_table(df_Qs_all$inliner)
  frequency_table(df_Qs_all$screen_material)

  # handle inliner (but keep them); To check: set
  df_Qs_all <- df_Qs_all %>%
    # filter inliner with unknown inliner date
    dplyr::filter(!((screen_material == "Inliner" | inliner == "Yes") &
                      is.na(inliner.date))) %>%
    # # set screen material to 'Unbekannt' for date < inliner.date
    # dplyr::mutate(screen_material = replace(
    #   screen_material, date < inliner.date, "Unbekannt"
    # ) %>% forcats::fct_drop()) %>%
    # set screen material to 'Unbekannt' if liner (inliner column will be used)
    dplyr::mutate(screen_material = replace(
       screen_material, screen_material == "Inliner", "Unbekannt"
     ) %>% forcats::fct_drop()) %>%
    # set inliner to 'Yes' if date > inliner.date, otherwise to 'No'
    dplyr::mutate(inliner = dplyr::if_else(
      date > inliner.date & !is.na(date) & !is.na(inliner.date),
      "Yes", "No")
    ) %>%
    dplyr::select(-inliner.date) %>%
    as.data.frame()

  # remove invalid data
  df_Qs_all <- df_Qs_all %>%
    dplyr::filter(Qs_rel >= 0 & !is.na(Qs_rel)) %>%
    #dplyr::filter(Qs_rel >= 0 & Qs_rel <= 1) %>%
    dplyr::filter(n_screens != 0)



 if (FALSE) {
   summary(df_Qs_all$kf)
   table(df_Qs_all$operational_state, kf_available = !is.na(df_Qs_all$kf))
   a <- df_Qs_all[,c("well_id", "operational_state", "kf")]
 }

# save data
 save(df_Qs_all, file = "model_data_v1_all.RData")
 write.table(df_Qs_all, file = "model_data_v1_all.csv", dec = ".", sep = ";", row.names = FALSE)
 df_Qs_all_2 <- df_Qs_all %>% dplyr::filter(key != "quantity measurements")
 save(df_Qs_all, file = "model_data_v3_only_pump_tests.RData")
 write.table(df_Qs_all, file = "model_data_v3_only_pump_tests.csv", dec = ".", sep = ";", row.names = FALSE)
 write.table(df_wells, file = "well_data_v2.csv", dec = ".", sep = ";", row.names = FALSE)


 # filter data for Alt-Brunnen
 df <- df_Qs_all
 df$n_rehab <- as.factor(df$n_rehab)


 if (FALSE) {
   write.table(df_model_input, file = "model_input_valid.csv", dec = ".", sep = ";", row.names = FALSE)
  }

}

# MAIN 7: load operational hour data -------------------------------------------

if (FALSE) {

  # read, rename and clean data ---
  df_hours <- read_csv(paths$data_operational_hours, skip = 57) %>%
    select_rename_cols(renamings$main, "old_name", "new_name_en") %>%
    dplyr::mutate(from = as.Date(from, format = "%Y-%m-%d"),
                  to = as.Date(to, format = "%Y-%m-%d"))
  head(df_hours)


  head(df_pump_tests)
  sum(duplicated(df_hours[,c("well_id", "from", "to")]))
  df_pump_tests$pump_test_1.date
  a <- dplyr::left_join(df_pump_tests, df_hours, by = c("well_id", "pump_test_1.date" = "to"))
  summary(a$operational_hours)

  well_ids_Qs <- unique(df_Qs_all$well_id)
  well_ids_hours <- unique(df_hours$well_id)
  sum(!well_ids_hours %in% well_ids_Qs)
  sum(!well_ids_Qs %in% well_ids_hours)
  well_ids_Qs[!well_ids_Qs %in% well_ids_hours]
  length(unique(df_Qs_all$well_id))
}


