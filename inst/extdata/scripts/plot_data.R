library(ggplot2)

# histogram of static water level measurements and data origin -----------------

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



# correlation of Qs vs. other variables plots ----------------------------------

# well age
p1 <- correlation_plot(dplyr::filter(df, n_rehab == 0), x = "well_age_years") +
  labs(title = "well age in years")

# material
p2 <- correlation_plot(df, x = "screen_material") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# number of screens
p3 <- correlation_plot(df, x = "n_screens") +
  scale_x_continuous(breaks = seq.int(0,7,1))

# diameter
p4 <- correlation_plot(df, x = "diameter") + scale_x_continuous(limits = c(0, NA))

# n_rehab
p5 <- correlation_plot(df, x = "n_rehab")

# days since last rehab
p6 <- correlation_plot(df, x = "years_since_last_rehab")

# waterworks
p7 <- correlation_plot(df, x = "waterworks") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# aquifer
p8 <- correlation_plot(df, x = "aquifer_coverage") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# surface water
p9 <- correlation_plot(df, x = "surface_water") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

cowplot::plot_grid(plotlist = list(p1, p2, p3, p4, p5, p6, p7, p8, p9),
                   nrow = 3, ncol = 3, align = "hv", scale = 0.9)

ggsave("correlation_plots_v2.png", dpi = 600, width = 10, height = 10)


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


# Plots zu Qs per well over time -----------------------------------------------

# filter ---
df2 <- df %>% dplyr::filter(key2 == "pump tests") %>%
  dplyr::mutate(key2 = forcats::fct_drop(key2))
length(unique(df2$site_id))

pdf("Qsrel_over_time_pump_tests.pdf", 16, 9)
pdf("Qsrel_over_time_all.pdf", 16, 9)

for (i in seq(1, length(unique(df$site_id)), 12)) {

  print(plot_Qs_over_time(df[df$site_id %in% unique(df$site_id)[i:(i + 11)], ]) +
          facet_wrap(~site_id, scales = "free", labeller = label_both, ncol = 4)
  )

  print(paste("pdf page", (i+11) / 12, "printed."))
}
dev.off()


# plot example one well
df_test <- df %>% dplyr::filter(site_id == 4020180) %>%
  dplyr::mutate(type = "pump tests and quantity measurements") %>%
  dplyr::bind_rows(df %>% dplyr::filter(site_id == 4020180 & key2 == "pump tests") %>%
                     dplyr::mutate(type = "only pump tests")) %>%
  dplyr::filter(days_since_operational_start >= 0)

plot_Qs_over_time(df_test, xmax = 30, legend_position =  "right") +
  facet_wrap(~type, ncol = 1, scales = "free")

ggsave("Qsrel_over_time_site_id_4020180_pump_tests_vs_quantity_meas.png", dpi = 600, width = 6, height = 4)



# plots of data distribution ---------------------------------------------------

# required data set: df_wells

if (FALSE) {

  variables_cat <- c("operational_state",
                     "screen_material",
                     "site",
                     "aquifer_confinement",
                     "surface_water")


  plots_cat <- lapply(variables_cat, function(x) {
    plot_frequencies(df_main, x, gsub("_", " ", x), 0.05)
  })


  plot_num_1 <- plot_distribution(Data = df_main,
                                  variable = "operational_start.year",
                                  title = "operational start",
                                  vertical_x_axis_labels = FALSE) +
    ggplot2::scale_x_continuous(limits = c(1920, 2020),
                                breaks = scales::pretty_breaks())

  plot_num_2 <- plot_distribution(Data = df_main,
                                  variable = "operational_start.Qs",
                                  title = "specific capacity at operational start",
                                  vertical_x_axis_labels = FALSE)

  plot_num_3 <- plot_frequencies(Data = df_main,
                                 variable = "number_screens",
                                 title = "number of screens",
                                 offset_perc_labels = 0.05,
                                 vertical_x_axis_labels = FALSE)

  plot_num_4 <- plot_distribution(Data = df_main,
                                  variable = "screen_diameter",
                                  binwidth = 50,
                                  title = "diameter",
                                  vertical_x_axis_labels = FALSE)

  plots <- cowplot::plot_grid(plotlist = c(plots_cat,
                                           list(plot_num_1, plot_num_2,
                                                plot_num_3, plot_num_4)),
                              nrow = 3, align = "hv", scale = 0.9)

  ggplot2::ggsave("frequency_plots_en.png", plot = plots, width = 20,
                  height = 20, dpi = 600)

  getwd()

  htmlwidgets::saveWidget(widget = plotly::ggplotly(plot_num_1),
                          file = "frequency_plot_Inbetriebnahme.html",
                          selfcontained = TRUE)

  htmlwidgets::saveWidget(widget = plotly::ggplotly(plot_num_2),
                          file = "frequency_plot_Qs_neu.html",
                          selfcontained = TRUE)

  htmlwidgets::saveWidget(widget = plotly::ggplotly(plot_num_4),
                          file = "frequency_plot_Durchmesser.html",
                          selfcontained = TRUE)
}


# plots for Qmom-Qzul relation -------------------------------------------------

# required data set: df_Q

# distribution
p1 <- ggplot2::ggplot(df_Q, ggplot2::aes(x = Ratio_Qmom_Qzul, y = stat(count) / sum(stat(count)))) +
  ggplot2::geom_histogram(binwidth = 0.1, fill = "grey", col = "white", boundary = 1) +
  ggplot2::scale_x_continuous(limits = c(0, 2)) +
  ggplot2::scale_y_continuous(name = "Percentage",
                              breaks = scales::pretty_breaks(),
                              labels = scales::percent_format(accuracy = 1)) +
  sema.berlin.utils::my_theme()
plotly::ggplotly(p1)

# cumulative distribution
l <- lapply(seq(0, 2, 0.1), function(x) table(df_Q$Ratio_Qmom_Qzul > x))
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
df_Q_agg <- dplyr::filter(df_Q, Qmom < 1000) %>%
  dplyr::group_by(id_Brunnen) %>%
  dplyr::summarise(Qmom_median = median(Qmom, na.rm = TRUE),
                   std_dev = sd(Qmom, na.rm = TRUE),
                   number = dplyr::n()) %>%
  tidyr::drop_na()


# plot Q measurements
ggplot2::ggplot(df_Q_agg, ggplot2::aes(x = Qmom_median)) +
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
