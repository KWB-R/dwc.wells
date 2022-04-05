#' Get Default Pump Test Variables
#'
#' @return vector with column names of pump test variables
#' @export
#'
#' @examples
#' get_pump_test_vars()
get_pump_test_vars <- function() {

  c(
    "well_id",
    "date",
    "key",
    #"key2",
    "Qs_rel",
    "days_since_operational_start",
    "well_age_years",
    "n_rehab",
    "last_rehab.date",
    "time_since_rehab_days",
    "time_since_rehab_years"
  )

}
