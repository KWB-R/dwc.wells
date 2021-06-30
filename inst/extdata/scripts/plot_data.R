
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

df <- df_Qs_all
df$n_rehab <- as.factor(df$n_rehab)


well_vars <- list(
  "well_age_years" = "Well age in years",
  "construction_year" = "Construction year",
  "operational_start.year" = "Year of operational start",
  "n_rehab" = "Number of rehabs",
  "years_since_last_rehab" = "Years since last rehab",
  "waterworks" = "Waterworks",
  "aquifer_coverage" = "Aquifer coverage",
  "screen_material" = "Filter material",
  "inliner" = "Inliner",
  "n_screens" = "Number of filter screens",
  "surface_water" = "Surface water",
  "surface_water.distance" = "Distance to surface water [m]",
  "admissible_discharge" = "Admissible discharge [m³/h]",
  "operational_start.Qs" = "Initial specific capacity [m³/(h, m)]",
  "diameter" = "Diameter [mm]",
  #"quality.Cu" = "Cu concentration [mg/L]",
  #"quality.DR" = "Dry residues [mg/L]",
  #"quality.Fe_tot" = expression(paste("Fe"[tot], " concentration in mg/L")),
  "quality.Fe_tot" = "Fe (tot) concentration [mg/L]",
  #"quality.Mn" = "Mn concentration [mg/L]",
  #"quality.NO3" =  "NO3 concentration [mg/L]",
  #"quality.P_tot" = "P (tot) concentration [mg/L]",
  #"quality.pH" = "pH",
  #"quality.PO4" = expression(paste("PO"[4], " concentration [mg/L]")),
  #"quality.PO4" = "PO4 concentration [mg/L]",
  "quality.Redox" = "Redox potential [mV]",
  #"quality.SO4" = "SO4 concentration [mg/L]",
  #"quality.Temp" = "Temperature [°C]",
  #"quality.TSS" = "TSS concentration [mg/L]"
  )


correlation_plots <- lapply(names(well_vars), function(x) {
  correlation_plot(df = df, x = x, title = well_vars[x])
})

multiplots2 <- cowplot::plot_grid(plotlist = correlation_plots,
                align = "hv", scale = 0.9)


ggsave("correlation_plots_v2.png", multiplots, dpi = 600, width = 25, height = 20)

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
pdf("Qsrel_over_time_v3.pdf", 16, 9)


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

# MAIN 3: plots of data distribution -------------------------------------------

# required data set: df_wells

if (FALSE) {

  str(df_wells_1)

  # Version 1: with nice labels ---

  well_vars_cat <- list("well_function" = "Well function",
                     "operational_state" = "Operational state",
                     "waterworks" = "Waterworks",
                     "aquifer_coverage" = "Aquifer coverage",
                     "screen_material" = "Filter material",
                     "inliner" = "Inliner",
                     "n_screens" = "Number of filter screens",
                     "surface_water" = "Surface water",
                     "surface_water.distance" = "Distance to surface water [m]")



  well_vars_num <- list("operational_start.year" = "Year of operational start",
                     "admissible_discharge" = "Admissible discharge [m³/h]",
                     "operational_start.Qs" = "Initial specific capacity [m³/(h, m)]",
                     "diameter" = "Diameter [mm]",
                     "quality.Cu" = "Cu concentration [mg/L]",
                     "quality.DR" = "Dry residues [mg/L]",
                     #"quality.Fe_tot" = expression(paste("Fe"[tot], " concentration in mg/L")),
                     "quality.Fe_tot" = "Fe (tot) concentration [mg/L]",
                     "quality.Mn" = "Mn concentration [mg/L]",
                     "quality.NO3" =  "NO3 concentration [mg/L]",
                     "quality.P_tot" = "P (tot) concentration [mg/L]",
                     "quality.pH" = "pH",
                     #"quality.PO4" = expression(paste("PO"[4], " concentration [mg/L]")),
                     "quality.PO4" = "PO4 concentration [mg/L]",
                     "quality.Redox" = "Redox potential [mV]",
                     "quality.SO4" = "SO4 concentration [mg/L]",
                     "quality.Temp" = "Temperature [°C]",
                     "quality.TSS" = "TSS concentration [mg/L]")


  plots_cat <- lapply(names(well_vars_cat), function(x) {
    plot_frequencies(df_wells, x, well_vars_cat[x], 0.1)
  })


  plots_num <- lapply(names(well_vars_num), function(x) {
    plot_distribution(df_wells, x, title = well_vars_num[x], vertical_x_axis_labels = FALSE)
  })

  # cowplot
  plots <- cowplot::plot_grid(plotlist = c(plots_cat, plots_num),
                              nrow = 4, align = "hv", scale = 0.9)

  # save overview plot
  ggplot2::ggsave("_frequency_plots_all.png", plot = plots, width = 30,
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

}
