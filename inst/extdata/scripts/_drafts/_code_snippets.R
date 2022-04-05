
# check counts per well generation
if (FALSE) {

  frequency_table(ifelse(is.na(df_wells$well_id_replaced), 1, 2)) # count well generation

  df_quality_agg_long <- df_quality_agg %>%
    tidyr::pivot_longer(cols = -1, names_to = "quality.parameter", values_to = "quality.value") %>%
    dplyr::left_join(lookup_par_unit)
}


# non binary split of Qs_rel
if (FALSE) {
  df$Qs_rel_cat <- cut(df$Qs_rel,
                       breaks = c(-Inf, 1/3 * 100, 2/3 * 100, Inf),
                       labels = c("1-low","2-middle","3-high"))
}
