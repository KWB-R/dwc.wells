#' Input Data for Well Capacity Prediction: Well Rehabilitations
#'
#' Well rehabilitations for all wells (prepared with R script in
#' /data-raw/prepare_model_data.R)
#'
#' @format A data.frame with 6308 rows and 3 variables:
#' \describe{
#'   \item{well_id}{well id, for info}
#'   \item{n_rehab}{number of well rehabilitations}
#'   \item{rehab_date}{date of well rehabilitation}
#' }
#' @examples
#' head(rehabs, n = 20)
#'
"rehabs"
