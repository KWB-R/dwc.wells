#' Input Data for Well Capacity Prediction
#'
#' A reduced dataset for well capacity prediction created with R script in
#' /data-raw/model_data.R
#'
#' @format A data.frame with 6308 rows and 27 variables:
#' \describe{
#'   \item{well_id}{well id, for info}
#'   \item{date}{date of capacity measurement, for info}
#'   \item{key}{measurement key, e.g. operational_start, pump_test_1, pump_test_2, for info}
#'   \item{Qs_rel}{specific capacity of well relative to operational start condition, output}
#'   \item{days_since_operational_start}{days since operational start, redundant}
#'   \item{well_age_years}{years since operationa start, input, numeric}
#'   \item{construction_year}{year of well construction}
#'   \item{screen_material}{screen material}
#'   \item{diameter}{well diameter (mm)}
#'   \item{drilling_method}{drilling_method}
#'   \item{admissible_discharge}{allowed pumping rate}
#'   \item{operational_start.Qs}{initial Qs at construction}
#'   \item{aquifer_coverage}{confined / unconfined}
#'   \item{W_static.sd}{standard deviation of static water level}
#'   \item{surface_water.distance}{distance to surface water}
#'   \item{n_rehab}{number of well rehabilitations}
#'   \item{time_since_rehab_years}{time since last well rehabilitation in years}
#'   \item{volume_m3_d.mean}{mean daily abstraction volume (m3)}
#'   \item{quality.EC}{water quality: electical conductivity (us/cm)}
#'   \item{quality.D0}{water quality: dissolved oxygen (mg/l)}
#'   \item{quality.Temp}{water quality: temperature (C)}
#'   \item{quality.pH}{water quality: pH}
#'   \item{quality.Redox}{water quality: electical conductivity (us/cm)}
#'   \item{quality.Fe_tot}{water quality: dissolved oxygen (mg/l)}
#'   \item{quality.Mn}{water quality: Mn (mg/l)}
#'   \item{quality.NO3}{water quality: NO3 (mg/l)}
#'   \item{quality.PO4}{water quality: PO4 (mg/l)}
#'   \item{quality.SO4}{water quality: SO4 (mg/l)}
#'   \item{quality.TSS}{water quality: Total Suspended Solids (mg/l)}
#' }
#' @import parsnip tibble rsample
"model_data_reduced"
