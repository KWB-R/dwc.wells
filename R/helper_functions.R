
# write_csv --------------------------------------------------------------------
write_csv <- function(df, filename) {
  write.table(df, file = filename, dec = ".", sep = ";", row.names = FALSE)
}

# Helper functions for artifical date fillup
default_interval <- function(dates_2, dates_1, func = mean) {
  as.integer(abs(round(func(dates_2 - dates_1, na.rm = TRUE), 0)))
}

real_interval <- function(dates_2, dates_1) {
  as.integer(round(dates_2 - dates_1, 0))
}

# finally not used
get_interval <- function(dates_2, dates_1, func = mean) {
  dplyr::if_else(!is.na(.data[[dates_1]]) & !is.na(.data[[dates_2]]),
                 real_interval(.data[[dates_2]], .data[[dates_1]]),
                 default_interval(.data[[dates_2]], .data[[dates_1]], func)
                 )
}


# Helper function for counting number of rehabs
cumsum_no_na <- function (x) {
  cumsum(ifelse(is.na(x), 0, x))
}
