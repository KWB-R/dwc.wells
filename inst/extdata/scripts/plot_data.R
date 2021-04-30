

# plots of data distribution ---------------------------------------------------

# required data set: df_main

if (FALSE) {

  variables_cat <- c("Zustand_Brunnen",
                     "Filtermaterial",
                     "Gebiet",
                     "Spannung",
                     "Gewaesser")


  plots_cat <- lapply(variables_cat, function(x) {
    plot_frequencies(df_main, x, gsub("_", " ", x), 0.05)
  })


  plot_frequencies(df_main, "Gewaesser")

  ggplot2::ggsave("frequency_plot_Gewaesser_v3.png", width = 5,
                  height = 6, dpi = 600)


  plot_num_1 <- plot_distribution(Data = df_main,
                                  variable = "Jahr_Inbetriebnahme",
                                  title = "Jahr Inbetriebnahme",
                                  vertical_x_axis_labels = FALSE) +
    ggplot2::scale_x_continuous(limits = c(1920, 2020),
                                breaks = scales::pretty_breaks())

  plot_num_2 <- plot_distribution(Data = df_main,
                                  variable = "Qs_neu",
                                  title = "Qs (neu)",
                                  vertical_x_axis_labels = FALSE)

  plot_num_3 <- plot_frequencies(Data = df_main,
                                 variable = "Filteranzahl",
                                 title = "Filteranzahl",
                                 offset_perc_labels = 0.05,
                                 vertical_x_axis_labels = FALSE)


  plot_num_4 <- plot_distribution(Data = df_main,
                                  variable = "Durchmesser",
                                  binwidth = 50,
                                  title = "Durchmesser",
                                  vertical_x_axis_labels = FALSE)

  plots <- cowplot::plot_grid(plotlist = c(plots_cat,
                                           list(plot_num_1, plot_num_2,
                                                plot_num_3, plot_num_4)),
                              nrow = 3, align = "hv", scale = 0.9)

  ggplot2::ggsave("frequency_plots_v4.png", plot = plots, width = 20,
                  height = 20, dpi = 600)


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
p1 <- ggplot2::ggplot(df_Q, ggplot2::aes(x = Qmom/Qzul, y = stat(count) / sum(stat(count)))) +
  ggplot2::geom_histogram(binwidth = 0.1, fill = "grey", col = "white", boundary = 1) +
  ggplot2::scale_x_continuous(limits = c(0, 2)) +
  ggplot2::scale_y_continuous(name = "Percentage",
                              breaks = scales::pretty_breaks(),
                              labels = scales::percent_format(accuracy = 1)) +
  sema.berlin.utils::my_theme()
plotly::ggplotly(p1)

# cumulative distribution
l <- lapply(seq(0, 2, 0.1), function(x) table(df_Q$Ratio > x))
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
