
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


# get logical vector for finding swapped dates
swapped_dates <- function(df) {
  df$pump_test_2.date < df$pump_test_1.date &
    !is.na(df$pump_test_1.date) &
    !is.na(df$pump_test_2.date)
}

# return info / summary on number of rows with swapped dates
check_swapped_dates <- function(cond) {

  n <- sum(cond, na.rm = TRUE)
  cat("number of rows with swapped dates:", n)
  if (n > 0) {
    cat("...\n\n")
    df_pump_tests[cond, c("site_id", "pump_test_1.date", "pump_test_2.date")]
  }
}
