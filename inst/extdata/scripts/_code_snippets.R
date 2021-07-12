# old code snipped for interpolating W_static, later put into function

if (FALSE) {
  # wells with only 1 W_static data point
  df_Q_W_1 <- df_Q_W %>%
    dplyr::group_by(site_id) %>%
    dplyr::filter(sum(!is.na(W_static)) == 1) %>%
    dplyr::mutate(W_static_2 = W_static) %>%
    tidyr::fill("W_static_2", .direction = "updown") %>%
    dplyr::mutate(W_static.origin = dplyr::if_else(
      is.na(W_static), "filled up", W_static.origin
    )) %>%
    as.data.frame()


  # wells with at least 2 W_static data points
  df_Q_W_2 <- df_Q_W %>%
    dplyr::group_by(site_id) %>%
    dplyr::filter(sum(!is.na(W_static)) >= 2) %>%
    dplyr::group_by(site_id) %>%
    dplyr::mutate(W_static_2 = approx(date, W_static, date, rule = 1)$y) %>%
    #dplyr::mutate(W_static_2 = approx(date, W_static, date, rule = 2)$y) %>%
    dplyr::mutate(W_static.origin = dplyr::case_when(is.na(W_static) & !is.na(W_static_2) ~ "interpolated",
                                                     is.na(W_static) & is.na(W_static_2) ~ "filled up",
                                                     !is.na(W_static) ~ W_static.origin)) %>%
    tidyr::fill(W_static_2, .direction = "updown") %>%
    as.data.frame()

  # combine the two datasets again and calculate Qs
  df_Q_W_new <- df_Q_W_1 %>% dplyr::bind_rows(df_Q_W_2) %>%
    dplyr::arrange(site_id, date)
}
