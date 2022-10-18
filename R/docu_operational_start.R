#' Input Data for Well Capacity Prediction: Operational Start
#'
#' Operational start date for all wells (prepared with R script in
#' /data-raw/prepare_model_data.R)
#'
#' @format A data.frame with 6308 rows and 2 variables:
#' \describe{
#'   \item{well_id}{well id, for info}
#'   \item{operational_start.date}{date of capacity measurement, for info}
#' }
#' @examples
#' head(operational_start, n = 20)
"operational_start"
