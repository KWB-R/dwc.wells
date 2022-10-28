library(dwc.wells)
wells <- dwc.wells::model_data %>%
  dplyr::select(.data$well_id,
                .data$date,
                .data$Qs_rel) %>%
  dplyr::group_by(.data$well_id,
                  .data$date) %>%
  dplyr::summarise(Qs_rel = mean(.data$Qs_rel)) %>%
  dplyr::ungroup()
  #dplyr::filter(.data$well_function == "Betriebsbrunnen")

single_val_per_well <- wells %>%
  dplyr::count(well_id) %>%
  dplyr::filter(n == 1) %>%
  dplyr::pull(well_id)

wells_singlevar <- wells[wells$well_id %in% single_val_per_well,] %>%
  dplyr::mutate(sim_year = as.integer(format(.data$date, format = "%Y"))) %>%
  dplyr::select(-date)

wells <- wells[!wells$well_id %in% single_val_per_well,]


obs <- split(wells,
             wells$well_id)

obs_list <- lapply(obs, function(df) {
  df[, -1L] %>%
  as.data.frame() %>%
  dplyr::mutate(date = as.POSIXct(.data$date, tz = "UTC")) %>%
  kwb.base::hsFillUp(tsField = "date", step_s = 86400L) %>%
    dplyr::mutate(sim_year = as.integer(format(.data$date, format = "%Y"))) %>%
    dplyr::group_by(.data$sim_year) %>%
    dplyr::summarise(Qs_rel = mean(.data$Qs_rel, na.rm = TRUE))
})

tmp <- obs$`162`[, -1L] %>%
  as.data.frame() %>%
  dplyr::mutate(date = as.POSIXct(.data$date, tz = "UTC")) %>%
kwb.base::hsFillUp(tsField = "date", step_s = 86400L)


plot(tmp$date, tmp$Qs_rel)
points(tmp$date, tmp$Qs_rel_orig, col = "red")

obs_df <- kwb.utils::rbindAll(obs_list, nameColumn = "well_id", namesAsFactor = FALSE) %>%
  dplyr::mutate(well_id = as.integer(well_id)) %>%
  dplyr::bind_rows(wells_singlevar) %>%
  dplyr::select(well_id, sim_year, Qs_rel) %>%
  dplyr::arrange(well_id, sim_year)

View(obs_df)

opstart <- model_data %>%
  dplyr::count(well_id, operational_start.Qs) %>%
  dplyr::select(-n)


volume_mean <- model_data %>%
  dplyr::count(well_id, volume_m3_d.mean) %>%
  dplyr::select(-n)

admissible_discharge <- model_data %>%
  dplyr::count(well_id, admissible_discharge) %>%
  dplyr::select(-n)

wells_meta <- opstart %>%
  dplyr::left_join(volume_mean) %>%
  dplyr::left_join(admissible_discharge)



obs_new <- obs_df %>%
  dplyr::left_join(wells_meta,
                   by = c("well_id")) %>%
  dplyr::mutate(
    Qs_rel_observed = .data$Qs_rel,
    Qs_observed = .data$Qs_rel * .data$operational_start.Qs / 100) %>%
  dplyr::select(well_id, sim_year, Qs_rel_observed,  Qs_observed, operational_start.Qs, volume_m3_d.mean, admissible_discharge)


predictions <- dwc.wells::get_predictions(model = xgb_fit,
                                          sim_data = dwc.wells::sim_data_donothing)


pred_new <- predictions %>%
  dplyr::select(.data$well_id,
                .data$sim_date,
                .data$operational_start.Qs,
                .data$Qs_rel,
                .data$admissible_discharge,
                .data$volume_m3_d.mean) %>%
  dplyr::mutate(sim_year = as.integer(format(.data$sim_date, format = "%Y")),
                Qs_start = .data$operational_start.Qs,
                Qs_prediction = .data$Qs_rel * .data$operational_start.Qs / 100) %>%
  dplyr::rename(Qs_rel_prediction = .data$Qs_rel)


tab <- pred_new %>%
  dplyr::count(well_id, sim_year) %>%
  dplyr::select(-n) %>%
  dplyr::mutate(prediction = TRUE) %>%
  dplyr::left_join(obs_new %>%
                     dplyr::count(well_id, sim_year) %>%
                     dplyr::select(-n) %>%
                     dplyr::mutate(observation = TRUE)
  ) %>%
  dplyr::mutate(observation = dplyr::if_else(is.na(.data$observation),
                                             FALSE,
                                             .data$observation),
                both = .data$prediction + .data$observation)


## Well observations without interpolations

observations_no_interpolation <- function(method = "paired") {

  wells_tmp <-  wells %>%
    dplyr::mutate(sim_year = format(.data$date,
                                format = "%Y") %>%
                    as.integer())


  if (method == "paired") {
    return(lapply(unique(wells_tmp$sim_year), function(year) {
      well_ids <- tab$well_id[tab$both == 2 & tab$sim_year == year]
      wells_tmp  %>%
        dplyr::filter(.data$sim_year == year,
                      .data$well_id %in% well_ids) %>%
        dplyr::count(.data$well_id, .data$sim_year) %>%
        dplyr::select(-.data$n) %>%
        dplyr::count(.data$sim_year) %>%
        dplyr::rename(n_wells_observed_no_interpolation = .data$n)}
    ) %>%
      dplyr::bind_rows() %>%
      dplyr::arrange(.data$sim_year))
  }

  wells_tmp %>%
    dplyr::count(.data$well_id, .data$sim_year) %>%
    dplyr::select(-.data$n) %>%
    dplyr::count(.data$sim_year) %>%
    dplyr::rename(n_wells_observed_no_interpolation = .data$n) %>%
    dplyr::arrange(.data$sim_year)

}

obs_nointerpol_paired <- observations_no_interpolation("paired")
obs_nointerpol_nonpaired <- observations_no_interpolation("non-paired")


obs_nointerpol <- obs_nointerpol_paired  %>%
  dplyr::rename(n_wells_observed_no_interpolation_paired = .data$n_wells_observed_no_interpolation) %>%
  dplyr::full_join(obs_nointerpol_nonpaired  %>%
                     dplyr::rename(n_wells_observed_no_interpolation_nonpaired = .data$n_wells_observed_no_interpolation)
  )

View(obs_nointerpol)

gg0 <- obs_nointerpol %>%
  tidyr::pivot_longer(cols = - .data$sim_year) %>%
ggplot2::ggplot(ggplot2::aes(x = .data$sim_year,
                             y = .data$value,
                             col = .data$name)) +
  #ggplot2::facet_wrap(~ .data$name, nrow = 4, ncol = 1, scales = "free_y") +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::theme_bw()

htmlwidgets::saveWidget(plotly::ggplotly(gg0),
                        "observations_no-interpolation.html")

### Well in observation and prediction year
pred_obs_wells <- tab[tab$both == 2,] %>%
  dplyr::count(sim_year)



waterdemand_yearly <- 215000000

prep_observation_summary <- function(method = "paired")  {
  if(method == "paired") {
    pred_new_summary_list <- lapply(unique(pred_obs_wells$sim_year),
                                    function(year) {

                                      well_ids <- tab$well_id[tab$both == 2 & tab$sim_year == year]
                                      obs_new %>%
                                        dplyr::filter(.data$sim_year == year,
                                                      .data$well_id %in% well_ids) %>%
                                        dplyr::group_by(sim_year) %>%
                                        dplyr::summarise(admissible_discharge = sum(admissible_discharge * volume_m3_d.mean)/sum(volume_m3_d.mean),
                                                         Qs_observed = sum(Qs_observed * volume_m3_d.mean)/sum(volume_m3_d.mean),
                                                         Qs_start = sum(operational_start.Qs * volume_m3_d.mean)/sum(volume_m3_d.mean),
                                                         Q_observed_m3h_drawdown.2.4m = 2.4 * Qs_observed,
                                                         Q_observed_m3h_drawdown.2.8m = 2.8 * Qs_observed,
                                                         n_wells_observed = dplyr::n(),
                                                         observed_hourly_production_rate_drawdown.2.4m = Q_observed_m3h_drawdown.2.4m * n_wells_observed,
                                                         observed_hourly_production_rate_drawdown.2.8m = Q_observed_m3h_drawdown.2.8m * n_wells_observed
                                                         ) %>%
                                        dplyr::mutate(Qs_rel_observed = 100 * Qs_observed / Qs_start,
                                                      drawdown_qs_observed_at_admissible_discharge = admissible_discharge / Qs_observed,
                                                      admissible_max_hourly_production_rate = n_wells_observed * admissible_discharge)



                                    })
    return(dplyr::bind_rows(pred_new_summary_list))
  }

  obs_new %>%
    dplyr::group_by(sim_year) %>%
    dplyr::summarise(admissible_discharge = sum(admissible_discharge * volume_m3_d.mean)/sum(volume_m3_d.mean),
                     Qs_observed = sum(Qs_observed * volume_m3_d.mean)/sum(volume_m3_d.mean),
                     Qs_start = sum(operational_start.Qs * volume_m3_d.mean)/sum(volume_m3_d.mean),
                     Q_observed_m3h_drawdown.2.4m = 2.4 * Qs_observed,
                     Q_observed_m3h_drawdown.2.8m = 2.8 * Qs_observed,
                     n_wells_observed = dplyr::n(),
                     observed_hourly_production_rate_drawdown.2.4m = Q_observed_m3h_drawdown.2.4m * n_wells_observed,
                     observed_hourly_production_rate_drawdown.2.8m = Q_observed_m3h_drawdown.2.8m * n_wells_observed) %>%
    dplyr::mutate(Qs_rel_observed = 100 * Qs_observed / Qs_start,
                  drawdown_qs_observed_at_admissible_discharge = admissible_discharge / Qs_observed,
                  admissible_max_hourly_production_rate = n_wells_observed * admissible_discharge)

}

prep_prediction_summary <- function(method = "paired")  {
if(method == "paired") {
pred_new_summary_list <- lapply(unique(pred_obs_wells$sim_year),
       function(year) {

  well_ids <- tab$well_id[tab$both == 2 & tab$sim_year == year]
  #pred_new_summary <- pred_new %>%
  pred_new %>%
  ## dplyr::filter(sim_year <= 2030) %>%
  dplyr::filter(.data$sim_year == year,
                .data$well_id %in% well_ids) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(.data$sim_year) %>%
  dplyr::summarise(
    admissible_discharge = sum(admissible_discharge * volume_m3_d.mean)/sum(volume_m3_d.mean),
    Qs_start = sum(Qs_start * volume_m3_d.mean)/sum(volume_m3_d.mean),
    Qs_prediction = sum(Qs_prediction * volume_m3_d.mean)/sum(volume_m3_d.mean),
    Q_prediction_m3h_drawdown.2.4m = 2.4 * Qs_prediction,
    Q_prediction_m3h_drawdown.2.8m = 2.8 * Qs_prediction,
    n_wells = dplyr::n()
    ) %>%
  dplyr::mutate(
    waterdemand_hourly = waterdemand_yearly/ 365/24,
    admissible_max_hourly_production_rate = n_wells * admissible_discharge,
    drawdown_qs_prediction_at_admissible_discharge = admissible_discharge / Qs_prediction,
    drawdown_qsstart_at_admissible_discharge = admissible_discharge / Qs_start,
    Qs_rel_prediction = 100 * Qs_prediction / Qs_start)
})

return(dplyr::bind_rows(pred_new_summary_list))
}

  pred_new %>%
    dplyr::filter(sim_year <= 2030) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$sim_year) %>%
    dplyr::summarise(
      admissible_discharge = sum(admissible_discharge * volume_m3_d.mean)/sum(volume_m3_d.mean),
      Qs_start = sum(Qs_start * volume_m3_d.mean)/sum(volume_m3_d.mean),
      Qs_prediction = sum(Qs_prediction * volume_m3_d.mean)/sum(volume_m3_d.mean),
      Q_prediction_m3h_drawdown.2.4m = 2.4 * Qs_prediction,
      Q_prediction_m3h_drawdown.2.8m = 2.8 * Qs_prediction,
      n_wells = dplyr::n(),
      predicted_hourly_production_rate_drawdown.2.4m = Q_prediction_m3h_drawdown.2.4m * n_wells,
      predicted_hourly_production_rate_drawdown.2.8m = Q_prediction_m3h_drawdown.2.8m * n_wells,
    ) %>%
    dplyr::mutate(
      waterdemand_hourly = waterdemand_yearly/ 365/24,
      admissible_max_hourly_production_rate = n_wells * admissible_discharge,
      drawdown_qs_prediction_at_admissible_discharge = admissible_discharge / Qs_prediction,
      drawdown_qsstart_at_admissible_discharge = admissible_discharge / Qs_start,
      Qs_rel_prediction = 100 * Qs_prediction / Qs_start)

}

method <- "non-paired"

obs_new_summary <- prep_observation_summary(method = method)
pred_new_summary <- prep_prediction_summary(method = method)

obs_new_summary[,c("sim_year", "n_wells_observed")] %>%
  dplyr::full_join(pred_new_summary[,c("sim_year", "n_wells")]) %>%
  View()

gg1 <- pred_new_summary  %>%
  dplyr::select(tidyselect::all_of(c("sim_year",
                                     "admissible_max_hourly_production_rate",
                                     "predicted_hourly_production_rate_drawdown.2.4m",
                                     "predicted_hourly_production_rate_drawdown.2.8m",
                                     "Qs_rel_prediction", "Qs_prediction",
                                     "Q_prediction_m3h_drawdown.2.4m",
                                     "Q_prediction_m3h_drawdown.2.8m",
                                     "Qs_start", "n_wells"))) %>%
  tidyr::pivot_longer(cols = c("admissible_max_hourly_production_rate",
                               "predicted_hourly_production_rate_drawdown.2.4m",
                               "predicted_hourly_production_rate_drawdown.2.8m",
                               "Qs_rel_prediction",
                               "Qs_prediction",
                               "Q_prediction_m3h_drawdown.2.4m",
                               "Q_prediction_m3h_drawdown.2.8m",
                               "Qs_start",
                               "n_wells")) %>%
ggplot2::ggplot(ggplot2::aes(x = .data$sim_year,
                             y = .data$value)) +
  ggplot2::facet_wrap(~ .data$name, nrow = 9, ncol = 1, scales = "free_y") +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::theme_bw()

htmlwidgets::saveWidget(plotly::ggplotly(gg1),
                        sprintf("%s_predictions_all.html",
                                method))


gg11 <- obs_new_summary  %>%
  dplyr::select(tidyselect::all_of(c("sim_year",
                                     "admissible_max_hourly_production_rate",
                                     "observed_hourly_production_rate_drawdown.2.4m",
                                     "observed_hourly_production_rate_drawdown.2.8m",
                                     "Q_observed_m3h_drawdown.2.4m",
                                     "Q_observed_m3h_drawdown.2.8m",
                                     "Qs_rel_observed", "Qs_observed", "Qs_start", "n_wells_observed"))) %>%
  tidyr::pivot_longer(cols = c("admissible_max_hourly_production_rate",
                               "observed_hourly_production_rate_drawdown.2.4m",
                               "observed_hourly_production_rate_drawdown.2.8m",
                               "Q_observed_m3h_drawdown.2.4m",
                               "Q_observed_m3h_drawdown.2.8m",
                               "Qs_rel_observed", "Qs_observed", "Qs_start", "n_wells_observed")) %>%
  ggplot2::ggplot(ggplot2::aes(x = .data$sim_year,
                               y = .data$value)) +
  ggplot2::facet_wrap(~ .data$name, nrow = 9, ncol = 1, scales = "free_y") +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::theme_bw()

htmlwidgets::saveWidget(plotly::ggplotly(gg11),
                        sprintf("%s_observations.html", method))


gg2 <- pred_new_summary %>%
  dplyr::left_join(obs_new_summary[,c("sim_year", "Qs_observed")]) %>%
  dplyr::select(.data$sim_year,
                .data$Qs_prediction,
                .data$Qs_start,
                .data$Qs_observed) %>%
  tidyr::pivot_longer(cols = c("Qs_prediction", "Qs_observed", "Qs_start")) %>%
  ggplot2::ggplot(ggplot2::aes(x = .data$sim_year,
                               y = .data$value,
                               col = .data$name)) +
  #ggplot2::facet_wrap(~ .data$name, nrow = 4, ncol = 1, scales = "free_y") +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::theme_bw()

htmlwidgets::saveWidget(plotly::ggplotly(gg2),
                        sprintf("%s_preds-obs_qs_qs-start.html", method))

gg3 <- pred_new_summary %>%
  dplyr::left_join(obs_new_summary[,c("sim_year", "Qs_rel_observed")]) %>%
  dplyr::select(.data$sim_year,
                .data$Qs_rel_prediction,
                .data$Qs_rel_observed) %>%
  tidyr::pivot_longer(cols = c("Qs_rel_prediction", "Qs_rel_observed")) %>%
  ggplot2::ggplot(ggplot2::aes(x = .data$sim_year,
                               y = .data$value,
                               col = .data$name)) +
  #ggplot2::facet_wrap(~ .data$name, nrow = 4, ncol = 1, scales = "free_y") +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::theme_bw()

htmlwidgets::saveWidget(plotly::ggplotly(gg3),
                        sprintf("%s_preds-obs_Qs-rel.html", method))


gg4 <- pred_new_summary %>%
  dplyr::rename(n_wells_prediction = .data$n_wells) %>%
  dplyr::left_join(obs_new_summary[,c("sim_year", "n_wells_observed")]) %>%
  dplyr::select(.data$sim_year,
                .data$n_wells_prediction,
                .data$n_wells_observed) %>%
  tidyr::pivot_longer(cols = c("n_wells_prediction", "n_wells_observed")) %>%
  ggplot2::ggplot(ggplot2::aes(x = .data$sim_year,
                               y = .data$value,
                               col = .data$name)) +
  #ggplot2::facet_wrap(~ .data$name, nrow = 4, ncol = 1, scales = "free_y") +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::theme_bw()

htmlwidgets::saveWidget(plotly::ggplotly(gg4),
                        sprintf("%s_preds-obs_nwells.html", method))


gg5 <- pred_new_summary  %>%
  dplyr::left_join(obs_new_summary[,c("sim_year", "drawdown_qs_observed_at_admissible_discharge")]) %>%
  dplyr::select(tidyselect::all_of(c("sim_year",
                                     "drawdown_qs_prediction_at_admissible_discharge",
                                     "drawdown_qs_observed_at_admissible_discharge",
                                     "drawdown_qsstart_at_admissible_discharge"))) %>%
  tidyr::pivot_longer(cols = c("drawdown_qs_prediction_at_admissible_discharge",
                               "drawdown_qs_observed_at_admissible_discharge",
                               "drawdown_qsstart_at_admissible_discharge")) %>%
  ggplot2::ggplot(ggplot2::aes(x = .data$sim_year,
                               y = .data$value,
                               col = .data$name)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::theme_bw()

plotly::ggplotly(gg5)

htmlwidgets::saveWidget(plotly::ggplotly(gg5),
                        sprintf("%s_preds-obs_all_drawdown_qs_qs-start_at_admissible_discharge.html",
                                method))

