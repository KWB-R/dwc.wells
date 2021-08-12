# observed data issues:
# i) original interval is wrong e.g. 1 for monthly values (well_id: 947 or 1142),
# ii) own interval calculation fails in case of data gaps (well_id: e.g. 5014 or 5015),
# iii) sometimes timestamp has a daily interval but last value of a month
# is the monthly sum (well_id: ),
# data is far out of possible range (e.g. well_id: 13065 or 7902)
# iv) negative volumes (well_id: 5010)


prepare_volume_data <- function(df_wells) {

  # load volume data
  read_ms_access_mri(paths$db, "WV_GMS_TBL_MENGENTABELLE") %>%
    select_rename_cols(renamings$main, "old_name", "new_name_en") %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::arrange(well_id, date) %>%
    # calculate volume per interval
    dplyr::mutate(volume_m3_d = abs(abstracted_volume) / interval,
                  volume_m3_h = volume_m3_d / 24) %>%
    dplyr::select(-c(origin, interval, abstracted_volume)) %>%
    # join admissible_discharge
    dplyr::right_join(df_wells %>% dplyr::select(well_id, admissible_discharge)) %>%
    # discard values far above admissible discharge (remove outlier)
    dplyr::filter(volume_m3_h <= 1.5 * admissible_discharge) %>%
    # aggregate data
    dplyr::group_by(well_id) %>%
    dplyr::summarise(volume_m3_d.mean = mean(volume_m3_d, na.rm = TRUE),
                     volume_m3_d.sd = sd(volume_m3_d, na.rm = TRUE)) %>%
    dplyr::mutate(volume_m3_d.cv = volume_m3_d.sd / volume_m3_d.mean) %>%
    dplyr::mutate(volume_m3_d.cv = tidyr::replace_na(volume_m3_d.cv, 0))

}


if (FALSE) {

  # recalcuate measurement interval and calculate volume per day
  df_volumes <- df_volumes %>%
    dplyr::group_by(well_id) %>%
    # sort according to date
    dplyr::arrange(date, .by_group = TRUE) %>%
    dplyr::mutate(interval_original = interval) %>%
    # recalculate interval between dates
    dplyr::mutate(interval = as.integer(date - dplyr::lag(date), units = 'days')) %>%
    tidyr::fill(interval, .direction = "up") %>%
    # correct interval in case of data gaps
    dplyr::mutate(interval = dplyr::if_else(! interval %in% c(1, 28, 30, 31, 365, 366),
                                            interval_original,
                                            interval)) %>%
    # calculate volume per day
    dplyr::mutate(volume_m3_d = abs(abstracted_volume) / interval) %>%
    # replace outlier values > 200,000 m3/d with median of the well
    dplyr::mutate(volume_m3_d = dplyr::if_else(volume_m3_d > 2 * 10^5,
                                               median(volume_m3_d),
                                               volume_m3_d))

  df_volumes_quantiles <- df_volumes %>%
    # for each well, remove values out of 90% confidence interval (outlier removal)
    dplyr::group_by(well_id) %>%
    dplyr::summarise(q05 = quantile(volume_m3_d, prob = 0.05, na.rm = TRUE),
                     q95 = quantile(volume_m3_d, prob = 0.95, na.rm = TRUE))

}
