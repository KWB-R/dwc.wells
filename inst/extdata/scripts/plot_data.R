
library(dwc.wells)
library(ggplot2)


# MAIN 0: histogram of static water level measurements and data origin ---------

if (FALSE) {
# plot histogram
plot_histogram <- function(df) {
  ggplot2::ggplot(df, ggplot2::aes(
    x = W_static, fill = forcats::fct_rev(W_static.origin), alpha = full_data_set)) +
    ggplot2::geom_histogram(position = "stack", boundary = 0) +
    ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(3, "Set2")) +
    ggplot2::scale_alpha_manual(values = c(0.4, 1.0)) +
    sema.berlin.utils::my_theme() +
    ggplot2::labs(fill = "Type", alpha = "Data complete?", x = "static water level")
}

plot_histogram(df_Q_W_new)
ggplot2::ggsave("W_static_interpolation_m20k1_f15k8_i64k2.png", dpi = 600, width = 5, height = 3)


# histogram of Qs_rel vs. n_rehab ----------------------------------------------

ggplot(df, aes(x = Qs_rel, fill = factor(n_rehab))) +
  geom_histogram(position = "fill", boundary = 0, binwidth = 0.1) +
  #scale_x_continuous(limit = c(-0.5, 3)) +
  scale_x_continuous(limit = c(0, 1), labels = scales::percent, breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks()) +
  #ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(8, "Set2")) +
  sema.berlin.utils::my_theme() +
  labs(fill = "n_rehab:")

ggsave("plot_Qs_rel_vs_n_rehabs.png", dpi = 600, width = 5, height = 3)

}


# MAIN 1: correlation of Qs vs. other variables plots --------------------------

if (FALSE) {

correlation_plots <- lapply(model_features, function(x) {
  correlation_plot(df = df, x = x, title = model_features_with_plot_names[x])
})

multiplots <- cowplot::plot_grid(plotlist = correlation_plots,
                                  nrow = 5, ncol = 7, align = "hv", scale = 0.9)


ggplot2::ggsave("correlation_plots_v2.png", multiplots, dpi = 600, width = 25, height = 20)


# save individual plots

lapply(correlation_plots, function(x) {
  ggplot2::ggsave(filename = paste0(gsub("\\.", "_", names(x$labels$title)), ".png"),
                  plot = x,
                  dpi = 600,
                  width = 6,
                  height = 4)
})

}


# MAIN 2: Plots zu Qs per well over time ---------------------------------------

if (FALSE) {

# filter ---
df2 <- df %>% dplyr::filter(key2 == "pump tests") %>%
  dplyr::mutate(key2 = forcats::fct_drop(key2))
length(unique(df2$site_id))
df$key2 <- "pump tests"

df$facet_lab <- paste0("well id: ", df$well_id, " (year: ", df$construction_year,
                       ifelse(!is.na(df$well_id_replaced),
                              paste0(", old well id: ", df$well_id_replaced),
                              ""),
                       ")")

pdf("Qsrel_over_time_pump_tests.pdf", 16, 9)
pdf("Qsrel_over_time_all.pdf", 16, 9)
pdf("Qsrel_over_time_with_old_well_info.pdf", 16, 9)


for (i in seq(1, length(unique(df$well_id)), 12)) {

  print(plot_Qs_over_time(df[df$well_id %in% unique(df$well_id)[i:(i + 11)], ]) +
          facet_wrap(~ facet_lab, scales = "free", ncol = 4)
          #facet_wrap(~ well_id, scales = "free", labeller = label_both, ncol = 4)
  )

  print(paste("pdf page", (i+11) / 12, "printed."))
}

dev.off()

# plots for selected well ids
plot_Qs_over_time(df[df$well_id %in% c(1081, 3258, 1084, 3259), ], xmax = 15) +
  facet_wrap(~ facet_lab, scales = "free_x", ncol = 2, dir = "v") +
  ggplot2::theme(strip.text.x = ggplot2::element_text(size = 9))

ggsave("example_replaced_wells.png", dpi = 600, width = 8, height = 6)

old_well_ids <- unique(df$well_id_replaced)
old_well_ids[old_well_ids %in% df$well_id]
a <- df[, c("well_id", "construction_year", "well_id_replaced", "Qs_rel")]


# plot two wells in comparison ---

library(dplyr)
library(dwc.wells)
site_ids <- c(4060070, 11020030)
well_ids <- c(1161, 5837)
df2 <- df %>%
  dplyr::filter(site_id %in% site_ids) %>%
  droplevels()
df$n_rehab <- as.factor(df$n_rehab)

plot_Qs_over_time(df2, xmax = 40, legend_position =  "right") +
  facet_wrap(~well_id, scales = "free", labeller = label_both, nrow = 1)

ggsave("Qs_over_time_two_example_wells_v2.png", dpi = 600, width = 8, height = 3)

}



# plot Qs-data for all wells as heatmap ----------------------------------------

if (FALSE) {

  # requires 'model_data'

  # select data
  df <- Data %>% select(well_id, well_name, date, Qs_rel, waterworks, well_gallery)

  # arrange data
  #df <- df %>% arrange(waterworks, well_gallery, well_id, date)

  # interpolate
  df_interpol <- interpolate(df, 1)

  waterworks_well_ids <- df %>% group_by(waterworks) %>%
    summarise(well_id = as.character(unique(well_id)))

  colours <- sema.berlin.utils::get_bwb_colours()[c(2,3,5)]

  dummy_labels <- c("",
                    " ",
                    "  ",
                    "   ",
                    "    ",
                    "     ",
                    "      ",
                    "       ",
                    "        ",
                    "         ",
                    "          ",
                    "           ",
                    "            ",
                    "             ",
                    "              ",
                    "               ",
                    "                ",
                    "                 ",
                    "                  ",
                    "                   ")

  pdf("Qsrel_over_time_heatmap_raster.pdf", width = 9, height = 5)

  for (waterwork in unique(waterworks_well_ids$waterworks)) {

    #pdf(sprintf("Qsrel_over_time_heatmap_%s.pdf", waterwork), 9, 5)

    well_ids <- waterworks_well_ids %>% filter(waterworks == waterwork) %>% pull(well_id)

    for (i in seq(1, length(well_ids), 20)) {

      well_ids_plot <- well_ids[i:(i + 19)]

      Data <- df_interpol %>% filter(well_id %in% well_ids_plot)

      print(
        ggplot2::ggplot(Data, ggplot2::aes(x = date, y = well_id, fill = Qs_rel)) +
          ggplot2::geom_raster() +
          ggplot2::scale_fill_gradient2(low = unname(colours["red"]),
                                        mid = unname(colours["yellow"]),
                                        high = unname(colours["green"]),
                                        midpoint = 0.5,
                                        na.value = "grey90",
                                        limits = c(0, 1),
                                        breaks = seq(0, 1, 0.2),
                                        labels = paste0(seq(0, 100, 20), "%"),
                                        oob = scales::squish,
                                        guide = guide_colourbar(reverse = TRUE)) +
          ggplot2::scale_y_discrete(limits = function(x) {rev(c(x, dummy_labels)[1:20])},
                                    labels = function(x) {stringr::str_sub(stringr::str_pad(x, 5, "left"), 1, 5)},
                                    expand = c(0.05, 0.05)) +
          ggplot2::scale_x_date(limits = as.Date(c("1960-01-01", "2021-12-31")),
                                breaks = scales::pretty_breaks(8)) +
          ggplot2::labs(y = "well_id", x = "Years", fill = "Specific\ncapacity",
                        title = waterwork) +
          sema.berlin.utils::my_theme() +
          ggplot2::theme(panel.grid.major = ggplot2::element_line(),
                         legend.position = "top",
                         legend.key.width = ggplot2::unit(2, "cm"),
                         legend.key.height = ggplot2::unit(0.35, "cm"),
                         legend.spacing.x = unit(0.8, "cm"),
                         axis.title = element_text(face = "plain"),
                         legend.title = element_text(face = "plain"),
                         title = element_text(size = 12)) +
          geom_hline(yintercept = seq(0.5, 20, 1), color = "white", lwd = 1)
      )

      print(paste("Data for", i + length(well_ids_plot) - 1, "well(s) of",  waterwork, "plotted."))
    }

    #dev.off()

  }

  dev.off()

  ggsave("example_plot_Qs_over_time_heatmap.png", width = 10, height = 5, dpi = 600)
  ggsave("example_plot_Qs_over_time_heatmap_v2.png", width = 8, height = 5, dpi = 600)

}



# MAIN 3: plots of data distribution -------------------------------------------

# required data set: df_wells

if (FALSE) {

  nums <- sunlist(lapply(df_well_features, is.numeric))
  df_well_features_num <- df_well_features[, nums] %>%
    dplyr::select(- c(well_id, well_id_replaced))
  df_well_features_cat <- df_well_features[, !nums] %>%
    dplyr::select(-c("well_name", tidyr::ends_with("date")))

  well_features_num <- model_features_with_plot_names[names(df_well_features_num)]
  well_features_cat <- model_features_with_plot_names[names(df_well_features_cat)]

  plots_cat <- lapply(names(well_features_cat), function(x) {
    plot_frequencies(df_well_features, x, well_features_cat[x], 0.1)
  })


  plots_num <- lapply(names(well_features_num), function(x) {
    plot_distribution(df_well_features, x, title = well_features_num[x],
                      vertical_x_axis_labels = FALSE)
  })

  # cowplot
  plots <- cowplot::plot_grid(plotlist = c(plots_cat, plots_num),
                              nrow = 5, ncol = 6, align = "hv", scale = 0.9)

  # save overview plot
  ggplot2::ggsave("well_feature_distribution.png", plot = plots, width = 25,
                  height = 20, dpi = 600)


  # save all plots individually
  lapply(c(plots_cat, plots_num), function(x) {
    ggplot2::ggsave(filename = paste0(gsub("\\.", "_", names(x$labels$subtitle)), ".png"),
                    plot = x,
                    dpi = 600,
                    width = 6,
                    height = 4)
  })

}

# MAIN 4: plots for Qmom-Qzul relation -----------------------------------------

if (FALSE) {
  # required data set: df_Q_monitoring

  # distribution
  p1 <- ggplot2::ggplot(df_Q_monitoring, ggplot2::aes(x = ratio_Q_admissible_discharge,
                                           y = stat(count) / sum(stat(count)))) +
    ggplot2::geom_histogram(binwidth = 0.1, fill = "grey", col = "white", boundary = 1) +
    ggplot2::scale_x_continuous(limits = c(0, 2)) +
    ggplot2::scale_y_continuous(name = "Percentage",
                                breaks = scales::pretty_breaks(),
                                labels = scales::percent_format(accuracy = 1)) +
    sema.berlin.utils::my_theme()
  plotly::ggplotly(p1)

  # cumulative distribution
  l <- lapply(seq(0, 2, 0.1), function(x) table(df_Q_monitoring$ratio_Q_admissible_discharge > x))
  names(l) <- sprintf("%3.1f", seq(0, 2, 0.1))
  df <- data.frame(do.call("rbind", l))
  colnames(df) <- c("valid", "invalid")
  df$threshold <- rownames(df)
  df <- tidyr::pivot_longer(data = df, cols = c("valid", "invalid"))
  #df$name <- factor(df$name, levels = c("valid", "invalid"))
  df$name <- factor(df$name, levels = c("invalid", "valid"))

  p2 <- ggplot2::ggplot(df, ggplot2::aes(x = threshold, y = value, fill = name)) +
    ggplot2::geom_bar(stat = "identity", position = "fill") +
    sema.berlin.utils::my_theme(legend.position = "top",
                                axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    ggplot2::scale_fill_manual(values = c("coral", "darkseagreen3")) +
    ggplot2::scale_x_discrete(expand = c(0.05, 0.05)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(),
                                breaks = scales::pretty_breaks()) +
    ggplot2::labs(x = 'Threshold "Qmom/Qzul"', y = "Percentage", fill = "")

  p2

  ggplot2::ggsave("Qmom_Qzul_threshold.png", p2, dpi = 600, height = 4, width = 6)
  p2

  cowplot::plot_grid(p1, p2)


  # plot median Q_mom per well ---------------------------------------------------

  # aggregate data
  df_Q_agg <- dplyr::filter(df_Q_monitoring, Q < 1000) %>%
    dplyr::group_by(well_id) %>%
    dplyr::summarise(Q_median = median(Q, na.rm = TRUE),
                     Q_stddev = sd(Q, na.rm = TRUE),
                     number = dplyr::n()) %>%
    tidyr::drop_na()


  # plot Q measurements
  ggplot2::ggplot(df_Q_agg, ggplot2::aes(x = Q_median)) +
    ggplot2::geom_histogram(fill = "lightblue", binwidth = 5) +
    sema.berlin.utils::my_theme() +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::labs(x = "Q_obs_median [m³/h]", y = "Frequency [-]")
  summary(df_Q_agg)

  ggplot2::ggsave("histogram_Ergiebigkeit_Q_obs.png", width = 4, height = 2.5, dpi = 600)


  # plots of quality measurements ------------------------------------------------

  # requires df_quality_agg_long

  ggplot2::ggplot(df_quality_agg_long, ggplot2::aes(x = "", y = Wert)) +
    ggplot2::geom_boxplot(width = 0.3) +
    ggplot2::facet_wrap(~paste0(Parameter, "\n", "[", Einheit, "]"),
                        scales = "free_y", nrow = 1) +
    ggplot2::labs(x = "", y = "Werte") +
    sema.berlin.utils::my_theme() +
    ggplot2::theme(strip.text.x = ggplot2::element_text(size = 11, hjust = 0.5),
                   axis.ticks.x = ggplot2::element_blank())

  ggplot2::ggplot(df_quality, ggplot2::aes(x = "", y = Wert)) +
    ggplot2::geom_boxplot(width = 0.3) +
    ggplot2::facet_wrap(id_Brunnen~paste0(Parameter, "\n", "[", Einheit, "]"),
                        scales = "free_y", nrow = 1) +
    ggplot2::labs(x = "", y = "Werte") +
    sema.berlin.utils::my_theme() +
    ggplot2::theme(strip.text.x = ggplot2::element_text(size = 11, hjust = 0.5),
                   axis.ticks.x = ggplot2::element_blank())

  ggplot2::ggsave("plot_quality_all_wells.png", width = 15, height = 5000, dpi = 600)
  getwd()

}
