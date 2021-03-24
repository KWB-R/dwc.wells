
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
    tidyr::drop_na()
}


# load_renamings_csv -----------------------------------------------------------

#' load renaming table from original excel file
#'
#' @param infile full path to excel file
#' @importFrom utils read.csv
#' @export
#'
load_renamings_csv <- function(infile) {
  read.csv(file = infile, sep = ";", stringsAsFactors = FALSE)
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

  df

}


# read_select_rename -----------------------------------------------------------

#' read table from MS Access data base; select and rename columns as defined in
#' renamings table ('old_name' -> 'new_name')
#'
#' @param path_db full path to database
#' @param tbl_name name of database table to be read
#' @param renamings name of data frame with renamings
#' @importFrom stats setNames
#' @export
#'
read_select_rename <- function(path_db, tbl_name, renamings) {

  # read data
  df <- read_ms_access(path_db, tbl_name)

  # make all column names upper case
  colnames(df) <- toupper(colnames(df))

  # select defined columns in df and renamings
  df <- df[, colnames(df) %in% renamings$old_name]
  renamings <- renamings[renamings$old_name %in% colnames(df),]

  # rename columns
  df %>% dplyr::rename(setNames(renamings$old_name, renamings$new_name))

}

