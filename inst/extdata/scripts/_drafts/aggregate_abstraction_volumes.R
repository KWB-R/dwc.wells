options(repos = c(
  kwbr = 'https://kwb-r.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Install some packages
install.packages('kwb.db')
install.packages('dwc.wells')

#load("inst/extdata/data_out/model_data.RData")
load(system.file("extdata/data_out/model_data.RData", package = "dwc.wells"))
model_data <- Data
#load("inst/extdata/data_out/volume_data.RData")
load(system.file("extdata/data_out/volume_data.RData", package = "dwc.wells"))
volume_data <- Data %>%
  dplyr::group_by(well_id) %>%
  dplyr::mutate(diff_days = as.numeric(.data$date - dplyr::lag(.data$date,n = 1)),
                volume.cbm_per_day = abstracted_volume / diff_days,
                volume.cbm_per_hour = volume.cbm_per_day / 24)

View(head(volume_data))
full_df <- model_data %>%
  dplyr::select(well_id, date) %>%
  dplyr::group_by(well_id) %>%
  tidyr::complete(tidyr::nesting(well_id), date = tidyr::full_seq(.data$date, 1))

last_rehab <- model_data %>%
  dplyr::select(.data$well_id, .data$last_rehab.date) %>%
  dplyr::filter(!is.na(.data$last_rehab.date)) %>%
  dplyr::mutate(date = .data$last_rehab.date)

volume_data_filled <- full_df %>%
  dplyr::left_join(last_rehab, by = c("well_id", "date")) %>%
  dplyr::left_join(kwb.utils::selectColumns(volume_data,
                                            c("well_id", "date", "volume.cbm_per_day")),
                                            by = c("well_id", "date"))
View(volume_data_filled)

volume_last_rehab <- volume_data_filled %>%
  dplyr::group_by(.data$well_id) %>%
  tidyr::fill(.data$last_rehab.date, .direction = "up") %>%
  dplyr::ungroup() %>%
  dplyr::group_by(.data$well_id, .data$date) %>%
  tidyr::fill(.data$volume.cbm_per_day, .direction = "down") %>%
  dplyr::ungroup() %>%
  dplyr::group_by(.data$well_id, .data$last_rehab.date) %>%
  dplyr::summarise(days_missing = sum(is.na(.data$volume.cbm_per_day)),
                days_available = sum(!is.na(.data$volume.cbm_per_day)),
                days_available_percent = 100 * days_available / (days_missing + days_available),
                total_abstraction = sum(.data$volume.cbm_per_day, na.rm = TRUE))

summary(volume_last_rehab)
View(volume_last_rehab)
