#' Data for well capacity prediction
#'
#' A dataset for well capacity prediction created with R script in /data-raw/model_data.R
#'
#' @format A data.frame with 6308 rows and 47 variables:
#' \describe{
#'   \item{well_id}{well id, for info}
#'   \item{date}{date of capacity measurement, for info}
#'   \item{key}{measurement key, e.g. operational_start, pump_test_1, pump_test_2, for info}
#'   \item{Qs_rel}{specific capacity of well relative to operational start condition, output}
#'   \item{days_since_operational_start}{days since operational start, redundant}
#'   \item{well_age_years}{years since operationa start, input, numeric}
#'   \item{to be continued}{...}
#' }
"model_data"
