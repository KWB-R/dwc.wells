# load package, paths and variable sets from global.R --------------------------

source("inst/extdata/scripts/global.R")

# helper function
classify_Qs_3classes <- function(x) {
  cut(x,
      breaks = c(-Inf, 50, 80, Inf),
      labels = c("low (< 50%)","moderate (50-80%)", "high (>= 80%)"))
}

# read and prepare pump test, wide format ---
df_wells <- dwc.wells:::prepare_well_data(paths$data_wells, renamings)
df1 <- prepare_pump_test_data_1(paths$data_pump_tests, renamings, df_wells)


# prepare data for plotting ---
df2 <- df1 %>% dplyr::filter(pump_test_2.well_rehab) %>%
  dplyr::group_by(well_id) %>%
  dplyr::mutate(n_rehab = as.factor(dwc.wells:::cumsum_no_na(pump_test_2.well_rehab))) %>%
  dplyr::select(well_id, pump_test_1.date, pump_test_1.Qs_rel,
                pump_test_2.date, pump_test_2.Qs_rel, n_rehab) %>%
  dplyr::mutate(
    pump_test_1.Qs_rel_cat = classify_Qs_3classes(pump_test_1.Qs_rel),
    pump_test_2.Qs_rel_cat = classify_Qs_3classes(pump_test_2.Qs_rel),
    Qs_rel_increase = pump_test_2.Qs_rel - pump_test_1.Qs_rel) %>%
  dplyr::ungroup()


df3 <- table(df2$n_rehab, df2$pump_test_2.Qs_rel_cat) %>% as.data.frame()

# plot 1

cols <- unname(sema.berlin.utils::get_bwb_colours()[c("green", "yellow", "red")])
ggplot(df3, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = position_fill(reverse = FALSE), width = 0.8) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = rev(cols)) +
  sema.berlin.utils::my_theme() +
  labs(x = "Number of regenerations",
       y = "Percentage of observations",
       fill = "Specific capacity",
       title = "Specific capacity after regeneration")

ggsave("specific_capacity_after_regeneration.png", dpi = 600, width = 6, height = 3)


# plot 1b

df3b <- table(df2$pump_test_2.Qs_rel_cat) %>% as.data.frame()

cols <- unname(sema.berlin.utils::get_bwb_colours()[c("green", "yellow", "red")])

ggplot(df3b, aes(x = "All data", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = position_fill(reverse = FALSE), width = 0.8) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = rev(cols)) +
  sema.berlin.utils::my_theme() +
  labs(x = "",
       y = "Percentage of observations",
       fill = "Specific capacity",
       title = "Specific capacity after regeneration")

ggsave("specific_capacity_after_regeneration_all.png", dpi = 600, width = 4, height = 3)


# plot 1c

df3c <- rbind(
  data.frame(type = "before", table(df2$pump_test_1.Qs_rel_cat)),
  data.frame(type = "after", table(df2$pump_test_2.Qs_rel_cat))
)

df3c$type <- factor(df3c$type, levels = c("before", "after"))

ggplot(df3c, aes(x = type, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = position_fill(reverse = FALSE), width = 0.8) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = rev(cols)) +
  sema.berlin.utils::my_theme() +
  labs(x = "",
       y = "Percentage of observations",
       fill = "Specific capacity",
       title = "Specific capacity before / after regeneration")

ggsave("specific_capacity_before_after_regeneration.png", dpi = 600, width = 5, height = 3)


# plot 1d

df3d <- rbind(
  data.frame(type = "before", table(df2$n_rehab, df2$pump_test_1.Qs_rel_cat)),
  data.frame(type = "after", table(df2$n_rehab, df2$pump_test_2.Qs_rel_cat))
)

df3d$type <- factor(df3d$type, levels = c("before", "after"))

df3d %>% filter(Var1 == 3)

cols <- unname(sema.berlin.utils::get_bwb_colours()[c("green", "yellow", "red")])
p <- ggplot(df3d, aes(x = type, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = position_fill(reverse = FALSE), width = 0.8) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = rev(cols)) +
  sema.berlin.utils::my_theme() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        strip.text.x = element_text(size = 11, hjust = 0.5),
        panel.spacing = unit(1, "lines")) +
  facet_wrap(~Var1, nrow = 1) +
  labs(x = "",
       y = "Percentage of observations",
       fill = "Specific capacity",
       subtitle = "Number of regenerations...")

plotly::ggplotly(p)
ggsave("specific_capacity_before_after_n_regenerations.png", dpi = 600, width = 8, height = 3)



# plot 2

ggplot(df2, aes(x = n_rehab, y = Qs_rel_increase)) +
  geom_boxplot(width = 0.6) +
  scale_y_continuous(limits = c(0, 100)) +
  sema.berlin.utils::my_theme() +
  labs(x = "Number of regenerations",
       y = "Specific capacity increase [%]",
       title = "Increase in specific capacity after regeneration")

ggsave("specific_capacity_increase_after_regeneration_boxplot.png", dpi = 600, width = 5, height = 3)


# plot 3

df4 <- df2 %>% group_by(n_rehab) %>%
  dplyr::summarise(mean = mean(Qs_rel_increase, na.rm = TRUE),
                   sd = sd(Qs_rel_increase, na.rm = TRUE))

ggplot(df4, aes(x = n_rehab, y = mean)) +
  geom_bar(stat = "identity", width = 0.6, fill = "deepskyblue3") +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    width = 0.2,
    alpha = 1
  ) +
  sema.berlin.utils::my_theme() +
  labs(x = "Number of regenerations",
       y = "Specific capacity increase [%]",
       title = "Increase in specific capacity after regeneration")

ggsave("specific_capacity_increase_after_regeneration_barplot.png", dpi = 600, width = 5, height = 3)
