
# interpolate_and_fill ---------------------------------------------------------
#' Interpolate and fill up static water level
#'
#' @param df data frame
#' @param x_col x column, e.g. date, to be used for interpolation
#' @param y_col y column, e.g. measured values, to be used for interpolation
#' @param group_by_col grouping variable within which interpolation is done
#' @param origin_col already existing or to be created column with type of value
#'
#'@importFrom stats approx
#'
#' @export
#'
interpolate_and_fill <- function(df, x_col, y_col, group_by_col,
                                 origin_col) {

  # set up column df_origin (either from existing or new)
  if(origin_col %in% colnames(df)) {
    df [, "origin_col"] <- df[, origin_col]
  } else {
    df[!is.na(df[, y_col]), "origin_col"] <- "original"
  }

  # wells with only 1 W_static data point
  df1 <- df %>%
    dplyr::group_by(.data[[group_by_col]]) %>%
    dplyr::filter(sum(!is.na(.data[[y_col]])) == 1) %>%
    dplyr::mutate(y_new = .data[[y_col]]) %>%
    tidyr::fill(y_new, .direction = "updown") %>%
    dplyr::mutate(origin = dplyr::if_else(is.na(.data[[y_col]]), "filled up", origin_col)) %>%
    as.data.frame()

  # wells with at least 2 W_static data points
  df2 <- df %>%
    dplyr::group_by(.data[[group_by_col]]) %>%
    dplyr::filter(sum(!is.na(.data[[y_col]])) >= 2) %>%
    dplyr::group_by(.data[[group_by_col]]) %>%
    dplyr::mutate(y_new = approx(
      .data[[x_col]], .data[[y_col]], .data[[x_col]], rule = 1)$y) %>%
    dplyr::mutate(origin = dplyr::case_when(
      is.na(.data[[y_col]]) & !is.na(y_new) ~ "interpolated",
      is.na(.data[[y_col]]) & is.na(y_new) ~ "filled up"
    )) %>%
    dplyr::mutate(origin = ifelse(!is.na(.data[[y_col]]), origin_col, origin)) %>%
    tidyr::fill(y_new, .direction = "updown") %>%
    as.data.frame()

  # combine the two datasets again and calculate Qs
  df <- df1 %>% dplyr::bind_rows(df2) %>%
    dplyr::arrange(.data[[group_by_col]], .data[[x_col]])

  # exchange column names
  df[, paste0(y_col, ".incomplete")] <- df[, y_col]
  df[, y_col] <- df$y_new
  df[, origin_col] <- df$origin


  # delete temporary columns
  df <- df %>% dplyr::select(-c(y_new, origin, origin_col))

  df

}


# interpolate_Qs ---------------------------------------------------------------

#' Interpolates Qs time series data to a given time interval
#'
#' @param df data frame with date and Qs measurements
#' @param interval_days interval for interpolation
#' @export
#'
interpolate_Qs <- function(df, interval_days = 1) {

  # get min and max dates per well
  min_max_dates <- df %>% group_by(well_id) %>%
    summarise(from = min(date), to = max(date))

  # create list with one data frame per well with complete date vector
  dates <- lapply(min_max_dates$well_id, function(x) {
    data.frame(well_id = x,
               date = seq(min_max_dates$from[min_max_dates$well_id == x],
                          min_max_dates$to[min_max_dates$well_id == x],
                          interval_days)
    )
  })

  # bind back to data frame
  dates_df <- bind_rows(dates)

  # join and interpolate
  df_interpol <- dates_df %>%
    full_join(df[, c("well_id", "date", "Qs_rel")],
              by = c("well_id", "date")) %>%
    mutate(Qs_rel = zoo::na.approx(Qs_rel)) %>%
    mutate(well_id = as.character(well_id))

  df_interpol
}


# load_renamings_excel ---------------------------------------------------------

#' load renaming table from original excel file
#'
#' @param infile full path to excel file
#' @param sheet sheet name
#' @param old_name_col name of column with original variable names
#' @param new_name_col name of column with new variable names
#' @importFrom rlang .data
#' @export
#'
load_renamings_excel <- function(infile,
                                 sheet = "DATEN",
                                 old_name_col = "Feld",
                                 new_name_col = "Parametername-R") {

  readxl::read_excel(path = infile, sheet = sheet) %>%
    dplyr::select(.data[[old_name_col]], .data[[new_name_col]]) %>%
    dplyr::rename(old_name = .data[[old_name_col]], new_name = .data[[new_name_col]]) %>%
    tidyr::drop_na() %>%
    as.data.frame()
}


# load_renamings_csv -----------------------------------------------------------

#' load renaming table from original excel file
#'
#' @param infile full path to excel file
#' @importFrom utils read.csv
#' @export
#'
load_renamings_csv <- function(infile) {
  read.csv(file = infile, sep = ";", stringsAsFactors = FALSE, na.strings=c(""))
}

# read_csv ---------------------------------------------------------------------

#' read csv data file exported by Sebastian Schimmelpfennig from db2
#'
#' @param file path to csv file
#' @param header logical, default = TRUE
#' @param fileEncoding default = UTF-8
#' @param skip number of rows to skip, default = 2
#' @param dec decimal separator, default = '.'
#' @param sep columns separator, default = 'tab'
#' @param na.strings string that represents NA, default = "(null)"
#'
#' @export
#' @importFrom utils write.table
#'
read_csv <- function(file, header = TRUE, fileEncoding = "UTF-8",
                     skip = 2, dec = ".", sep = "\t",
                     na.strings = "(null)") {

  read.csv(file = file, header = header, fileEncoding = fileEncoding,
           skip = skip, dec = dec, sep = sep, na.strings = na.strings)
}


# read_ms_access ---------------------------------------------------------------

#' read table from MS Access data base via odbc connection under 64-bit-R
#'
#' @param path_db full path to database
#' @param tbl_name name of database table to be read
#' @export
#' @importFrom kwb.db hsGetTable
read_ms_access <- function(path_db, tbl_name) {

  kwb.db::hsGetTable(mdb = path_db,
                     tbl = tbl_name)

}

read_ms_access_mri <- function(path_db, tbl_name) {
  # start server
  odbc32::start_server(invisible = TRUE)
  # open connection
  con <- odbc32::odbcConnectAccess2007(path_db)
  # show table names
  #odbc32::sqlTables(con, tableType = "TABLE")$TABLE_NAME
  # read data from table, "as.is = TRUE" is required to adopt data formats from ms access
  df <- odbc32::sqlFetch(con, tbl_name,  as.is = TRUE)
  odbc32::stop_server()
  if(!is.data.frame(df)) {
    stop("Table cannot be read. Is it already open and locked?")
  }
  df
}



# read_select_rename (delete) --------------------------------------------------

#' read table from MS Access data base; select and rename columns as defined in
#' renamings table ('old_name' -> 'new_name')
#'
#' @param path_db full path to database
#' @param tbl_name name of database table to be read
#' @param renamings name of data frame with renamings
#' @param old_name_col name of column with original variable names
#' @param new_name_col name of column with new variable names
#' @importFrom stats setNames
#' @export
#'
read_select_rename <- function(path_db, tbl_name, renamings,
                               old_name_col = "old_name",
                               new_name_col = "new_name") {


  # read data
  df <- read_ms_access(path_db, tbl_name)

  # make all column names upper case
  colnames(df) <- toupper(colnames(df))

  # select defined columns in df and renamings
  df <- df[, colnames(df) %in% renamings[!is.na(renamings[, new_name_col]),
                                         old_name_col]]
  renamings <- renamings[renamings[, old_name_col] %in% colnames(df),]

  # rename columns
  colnames(df) <- rename_values(colnames(df), renamings,
                                old_name_col, new_name_col)

  df

  # alternative version
  # df %>% dplyr::rename(setNames(renamings$old_name, renamings$new_name))

}


# select_rename_cols -----------------------------------------------------------
#'
#' selects and renames columns from a data frame according to a reference table
#'
#' @param df data frame with cols to be renamed
#' @param renamings name of data frame with renamings
#' @param old_name_col name of column with original variable names
#' @param new_name_col name of column with new variable names
#' @importFrom stats setNames
#' @export
#'
select_rename_cols <- function(df, renamings,
                               old_name_col = "old_name",
                               new_name_col = "new_name") {


  # make all column names upper case
  colnames(df) <- toupper(colnames(df))

  # select defined columns in df and renamings
  df <- df[, colnames(df) %in% renamings[!is.na(renamings[, new_name_col]),
                                         old_name_col]]
  renamings <- renamings[renamings[, old_name_col] %in% colnames(df),]

  # rename columns
  colnames(df) <- rename_values(colnames(df), renamings,
                                old_name_col, new_name_col)

  df

}

# rename_values ----------------------------------------------------------------

#' rename values of a character vector according to renamings table
#'
#' @param x character vector
#' @param renamings data frame consisting of old and new names
#' @param old_name_col name of column with original variable names
#' @param new_name_col name of column with new variable names
#' @export
#'
rename_values <- function(x,
                          renamings,
                          old_name_col = "old_name",
                          new_name_col = "new_name") {


    old_names <- renamings[, old_name_col]
    new_names <- renamings[, new_name_col]

    new_names[match(x, old_names)]

}


# summarise_marginal_factor_levels ---------------------------------------------

#' summarise factor levels with relative frequency below a threshold
#'
#' @param x factor variable
#' @param perc_threshold percentage threshold under which levels will be summarised
#' @param marginal_name for new summary factor level
#' @export
#'
summarise_marginal_factor_levels <- function(x, perc_threshold, marginal_name) {

  forcats::fct_lump_prop(x,
                         prop = perc_threshold * 0.01,
                         other_level = marginal_name)

}



# tidy_factor ------------------------------------------------------------------
#' turn character into factor, sort factor levels and replace NA level
#'
#' @param x character vector to be turned to factor
#' @param level_sorting sorting of factor levels; two options: "frequency"
#' (default) and "alphabet"; level "Unbekannt" is always always at the end
#'
#' @export
#'
tidy_factor <- function(x, level_sorting = c("frequency", "alphabet")[1]) {

  # turn character to factor
  x <- factor(x)

  # sort according to frequency
  if (level_sorting == "frequency") {
    x <- forcats::fct_infreq(x)
  }

  # handle missing values
  x <- x %>%
    dplyr::na_if("") %>%
    forcats::fct_explicit_na(na_level = "Unbekannt") %>% # replace NA with "Unbekannt"
    forcats::fct_drop()

  # put "Andere" to end
  if ("Andere" %in% levels(x)) {
  x <- forcats::fct_relevel(x, "Andere", after = Inf)
  }

  # put "Unbekannt" to end
  if ("Unbekannt" %in% levels(x)) {
    x <- forcats::fct_relevel(x, "Unbekannt", after = Inf)
  }

  x
}



# frequency_table --------------------------------------------------------------

#' calculate absolute and relative frequencies of categorical varables
#'
#' @param x vector with categorical variable
#' @param perc_digits number of decimal digits for percentages, default = 1
#' @param sort_freq sort according to frequency counts, logical, default: TRUE
#'
#' @export
#'
frequency_table <- function(x, perc_digits = 1, sort_freq = FALSE) {

  df <- data.frame(table(x, useNA = "ifany", deparse.level = 0)) %>%
    dplyr::mutate(perc = round(.data[["Freq"]] / sum(.data[["Freq"]]) * 100, perc_digits)) %>%
    dplyr::rename(value = .data[["Var1"]], n = .data[["Freq"]])

  if (sort_freq) {
    df %>% dplyr::arrange(-.data[["n"]])
  } else {
      df
    }

}


# handle_inliner ---------------------------------------------------------------

handle_inliner <- function(df) {

  # handle inliner (but keep them)
  df <- df %>%
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
    dplyr::mutate(inliner = factor(dplyr::if_else(
      date > inliner.date & !is.na(date) & !is.na(inliner.date),
      "Yes", "No"), levels = c("Yes", "No"))
    ) %>%
    dplyr::select(-inliner.date) %>%
    as.data.frame()

  df
}


# save_data --------------------------------------------------------------------

#' Save data frame in different formats: csv, RData, rds
#'
#' @param Data data frame
#' @param path out path for saving data
#' @param filename core of file name
#' @param formats export formats: "csv", "RData", "rds" or several using 'c'
#'
#' @export
#'
save_data <- function(Data, path, filename, formats = c("csv", "RData", "rds")) {

  if ("csv" %in% formats) {
    write.table(Data, file = file.path(path, sprintf("%s.csv", filename)),
                dec = ".", sep = ";", row.names = FALSE)
  }

  if ("RData" %in% formats) {
    save(Data, file = file.path(path, sprintf("%s.RData", filename)))
  }

  if ("rds" %in% formats) {
    saveRDS(Data, file = file.path(path, sprintf("%s.rds", filename)))
  }
}


# fill_up_na_with_median -------------------------------------------------------

#' Fill up NA values with median of lookup table
#'
#' @param df data frame with NA values
#' @param df_lookup data frame to calculate median values
#' @param matching_id column with ids for which median should be calculated

#' @export
#'

fill_up_na_with_median_from_lookup <- function(df, df_lookup, matching_id = "well_id") {

  na_colnames <- setdiff(names(which(colSums(is.na(df)) > 0)), "well_id_replaced")
  lookup_indices <- df_lookup[, matching_id] %in% df[, matching_id]
  lookup_data <- df_lookup[lookup_indices, na_colnames]
  median_values <- lapply(lookup_data, median, na.rm = TRUE)

  for (colname in na_colnames) {
    n1 <- sum(is.na(df[, colname]))
    df[, colname] <- tidyr::replace_na(df[, colname], median_values[[colname]])
    n2 <- sum(is.na(df[, colname]))
    cat(sprintf("NA values filled up for '%s': %d\n", colname, n1-n2))
  }

  df
}

# replace_na_with_median -------------------------------------------------------

#' Replace NAs with median
#'
#' @param x vector, for which NA should be replaced
#' @export
#' @importFrom stats median
replace_na_with_median <- function(x) {
  x[is.na(x)] <- stats::median(x, na.rm = TRUE)
  x
}
