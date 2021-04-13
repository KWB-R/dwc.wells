
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


# read_ms_access ---------------------------------------------------------------

#' read table from MS Access data base via odbc connection under 64-bit-R
#'
#' @param path_db full path to database
#' @param tbl_name name of database table to be read
#' @export
#'
read_ms_access <- function(path_db, tbl_name) {

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


# read_select_rename -----------------------------------------------------------

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


# replace_NAs_in_factor_var ----------------------------------------------------

#' rename values of a character vector according to renamings table
#'
#' @param x factor with NAs or gaps ("")
#' @export
#'
replace_NAs_in_factor_var <- function(x) {

  # turn NA into "Unbekannt"
  x %>%
    forcats::fct_explicit_na(na_level = "Unbekannt") %>%
    forcats::fct_recode("Unbekannt" = "") %>%
    droplevels() %>%
    forcats::fct_infreq()

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

# replace_NAs_in_factor_vars ---------------------------------------------------

#' rename values of a character vector according to renamings table
#'
#' @param df data frame, with factors to be checked for NAs
#' @export
#'
replace_NAs_in_factor_vars <- function(df) {

  factor_vars <- names(df)[sapply(df, is.factor)]

  for (factor_var in factor_vars) {
    df[, factor_var] <- replace_NAs_in_factor_var(df[, factor_var])
  }

  df

}

