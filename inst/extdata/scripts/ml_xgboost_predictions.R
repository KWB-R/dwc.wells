
source("inst/extdata/scripts/global.R")
source("inst/extdata/scripts/ml_resampling.R")
source("inst/extdata/scripts/ml_xgboost.R")

sim_reference_date <- as.Date("2021-05-01")
#sim_reference_date <- Sys.Date()

# filter only Betriebsbrunnen
df <- well_feature_data %>% dplyr::filter(well_function == "Betriebsbrunnen" &
                                    operational_state == "betriebsbereit")

df <- dwc.wells:::fill_up_na_with_median_from_lookup(df, df)

# drop unused factor levels
df <- df %>% droplevels()

# prepare prediction data ---

# make predictions for other data ------
names(df_test)[!names(df_test) %in% names(df)]

df_wells <- dwc.wells:::prepare_well_data(paths$data_wells, renamings)

rehab_data <- prepare_pump_test_data_1(paths$data_pump_tests, renamings, df_wells) %>%
  dplyr::filter(pump_test_2.well_rehab) %>%
  dplyr::group_by(well_id) %>%
  dplyr::mutate(n_rehab = as.factor(dwc.wells:::cumsum_no_na(pump_test_2.well_rehab))) %>%
  dplyr::select(well_id, operational_start.date, action_date, n_rehab) %>%
  dplyr::ungroup()

# get well id and operational start date

well_start_dates <- df %>%
  dplyr::select(well_id, operational_start.date)  %>%
  unique()

# get well id and age combinations
well_ages <- df %>%
  dplyr::group_by(well_id) %>%
  tidyr::expand(well_age_years = seq.int(0, 60, 1))

# combine both and calculate sim dates
library(lubridate)
well_ages_dates <- well_start_dates %>%
  dplyr::left_join(well_ages) %>%
  dplyr::mutate(sim_date = operational_start.date %m+% years(well_age_years)) %>%
  dplyr::select(-operational_start.date)

# tmp data 1
sim_data_tmp1 <- rehab_data %>%
  dplyr::rename(sim_date = action_date) %>%
  dplyr::mutate(well_age_years = NA) %>%
  dplyr::select(well_id, sim_date, well_age_years, n_rehab)

# tmp data 2
sim_data_tmp2 <- well_ages_dates %>%
  dplyr::mutate(n_rehab = as.factor(NA)) %>%
  dplyr::select(well_id, sim_date, well_age_years, n_rehab)

# create sim data
sim_data_base <- rbind(sim_data_tmp1, sim_data_tmp2) %>%
  dplyr::arrange(well_id, sim_date) %>%
  dplyr::left_join(rehab_data %>%
                     dplyr::select(well_id, action_date, n_rehab)) %>%
  dplyr::group_by(well_id) %>%
  tidyr::fill(c(n_rehab, action_date), .direction = "down") %>%
  dplyr::mutate(time_since_rehab_years = lubridate::time_length(sim_date - action_date, "years")) %>%
  dplyr::mutate(n_rehab = ifelse(is.na(n_rehab), 0, n_rehab),
         time_since_rehab_years = ifelse(is.na(time_since_rehab_years),
                                         well_age_years,
                                         time_since_rehab_years)) %>%
  dplyr::mutate(type = ifelse(sim_date < sim_reference_date, "past", "future")) %>%
  dplyr::filter(!is.na(well_age_years)) %>%
  dplyr::select(-action_date) %>%
  dplyr::ungroup()


# create sim data
sim_data_donothing <- sim_data_base %>%
  dplyr::left_join(df, by = "well_id") %>%
  data.frame() %>%
  dplyr::select(well_id, sim_date, type, all_of(model_features)) %>%
  # remove correlated variables
  dplyr::select(-c(well_depth, quality.DR, quality.P_tot,
            volume_m3_d.sd, waterworks, surface_water)) %>%
  # remove unimportant variables
  dplyr::select(-c(n_screens, filter_length, quality.Cu, inliner)) %>%
  # set inliner in screen_material to "Unbekannt
  dplyr::mutate(screen_material = replace(
    screen_material, screen_material == "Inliner", "Unbekannt"
  ) %>% forcats::fct_drop())
usethis::use_data(sim_data_donothing)

sim_data_pred <- predict(xgb_fit, sim_data) %>%
  dplyr::mutate(.pred = ifelse(.pred < 0, 0, .pred))
names(sim_data_pred) <- "Qs_rel"
predictions <- cbind(sim_data, sim_data_pred)
length(unique(predictions$well_id))


# plot 1: all predictions as points, one plot per well -------------------------

p <- ggplot(predictions, aes(x = well_age_years, y = Qs_rel)) +
  scale_x_continuous(limits = c(0, 40)) +
  scale_y_continuous(limits = c(0, 100), oob = scales::rescale_none) +
  geom_point(alpha = 0.5) +
  facet_wrap(~well_id)

ggsave("xgb_plots_points_betriebsbereit.png", p, dpi = 600, width = 30, height = 20)



# plot 2: all predictions as individual lines in one plot ----------------------

p2 <- ggplot(predictions, aes(x = well_age_years, y = Qs_rel, col = type, group = well_id)) +
  geom_line(alpha = 0.05) +
  scale_x_continuous(limits = c(0, 40)) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Well age [yrs]", y = "Specific capacity [%]") +
  sema.berlin.utils::my_theme(legend.position = "none")

p2
getwd()

ggsave("xgb_plot_multi_line_betriebsbereit_bis_40_past_future.png", dpi = 600, width = 4.5, height = 3)


# plot mean prediction vs. confidence interval ---------------------------------

p3 <- ggplot(predictions, aes(x = well_age_years, y = Qs_rel)) +
  scale_x_continuous(limits = c(0, 40)) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Well age [yrs]", y = "Specific capacity [%]") +
  # stat_summary(geom = "ribbon", fun.data = mean_cl_normal,
  #              fun.args = list(conf.int = 0.9999), fill = "lightblue") +
  stat_summary(geom = "ribbon", fun.data = "median_hilow", fill = "lightblue") +
  stat_summary(geom = "line", fun = mean) +
  sema.berlin.utils::my_theme()

p3

ggsave("xgb_plot_mean_line_and_95_conf_int_betriebsbereit_bis_40.png", p3, dpi = 600, width = 4.5, height = 3)


# plot 4: predictions for two selected wells with colors for past / future -----

p4 <- ggplot(filter(predictions, well_id %in% c(1161, 5837)),
             aes(x = well_age_years, y = Qs_rel, col = type)) +
  geom_line(alpha = 0.5) +
  scale_x_continuous(limits = c(0, 40)) +
  scale_y_continuous(limits = c(NA, 100), breaks = scales::pretty_breaks(6)) +
  facet_wrap(~well_id) +
  labs(x = "Well age [yrs]", y = "Specific capacity [%]") +
  sema.berlin.utils::my_theme(legend.position = "none")

p4

ggsave("xgb_plot_multi_line_betriebsbereit_bis_40_past_future_2wells.png",
       dpi = 600, width = 6, height = 3)


# plot predictions vs. observations for two selected wells ---------------------

df <- model_data %>% filter(well_id %in% c(1161, 5837)) %>%
  mutate(n_rehab = as.factor(n_rehab))
df_pred <- predictions %>% filter(well_id %in% c(1161, 5837))

p5 <- ggplot2::ggplot(df, ggplot2::aes(x = well_age_years,
                                      y = Qs_rel, col = n_rehab)) +
  ggplot2::geom_point() +
  #ggplot2::geom_line(lty = 2) +
  #ggplot2::geom_line(ggplot2::aes(group = "all")) +
  ggplot2::geom_line(data = df_pred, aes(x = well_age_years,
                                         y = Qs_rel,
                                         col = NULL,
                                         lty = 'unity line'
  ), alpha = 0.5) +
  scale_x_continuous(limits = c(0, 40)) +
  scale_y_continuous(limits = c(NA, 120), breaks = scales::pretty_breaks(6), oob = scales::rescale_none) +
  ggplot2::scale_color_manual(values = rev(RColorBrewer::brewer.pal(length(levels(df$n_rehab)), "RdYlGn"))) +
  #ggplot2::scale_color_manual(values = rev(scales::hue_pal()(length(levels(df$n_rehab))))) +
  sema.berlin.utils::my_theme(legend.position = "top") +
  ggplot2::guides(color = ggplot2::guide_legend(nrow = 1)) +
  ggplot2::labs(color = "observations (by number of rehabs):",
                x = "Well age [yrs]",
                y = "Specific capacity [%]",
                linetype = "predictions") +
  facet_wrap(~well_id, labeller = purrr::partial(label_both, sep = ": "))

p5
ggsave("plot_observations_vs_predictions_xgboost_v3.png", dpi = 600, width = 9, height = 4)


# plot predictions vs. observations for one selected well ----------------------

df <- model_data %>% filter(well_id %in% c(5837)) %>%
  mutate(n_rehab = as.factor(n_rehab))
df_pred <- predictions %>% filter(well_id %in% c(5837))

p5 <- ggplot2::ggplot(df, ggplot2::aes(x = well_age_years,
                                       y = Qs_rel, col = n_rehab)) +
  ggplot2::geom_point() +
  ggplot2::geom_line(ggplot2::aes(group = "all")) +
  ggplot2::geom_line(data = df_pred, aes(x = well_age_years,
                                         y = Qs_rel,
                                         col = NULL,
                                         lty = 'unity line'
  ), alpha = 0.5) +
  scale_x_continuous(limits = c(0, 40)) +
  scale_y_continuous(limits = c(NA, 120), breaks = scales::pretty_breaks(6), oob = scales::rescale_none) +
  ggplot2::scale_color_manual(values = rev(RColorBrewer::brewer.pal(length(levels(df$n_rehab)), "RdYlGn"))) +
  #ggplot2::scale_color_manual(values = rev(scales::hue_pal()(length(levels(df$n_rehab))))) +
  sema.berlin.utils::my_theme() +
  #ggplot2::guides(color = ggplot2::guide_legend(nrow = 1)) +
  ggplot2::labs(color = "observations\n(by number of rehabs):",
                x = "Well age [yrs]",
                y = "Specific capacity [%]",
                linetype = "predictions") +
  facet_wrap(~well_id, labeller = purrr::partial(label_both, sep = ": "))

p5
ggsave("plot_observations_vs_predictions_xgboost_v3b.png", dpi = 600, width = 6, height = 3.5)
