# load volume data
df_volumes <- read_ms_access_mri(paths$db, "WV_GMS_TBL_MENGENTABELLE") %>%
  select_rename_cols(renamings$main, "old_name", "new_name_en") %>%
  #dplyr::select(well_id, date, abstracted_volume) %>%
  dplyr::mutate(date = as.Date(date))

library(dplyr)
df_volumes_agg <- df_volumes %>%
  mutate(vol_m3_d = abstracted_volume / interval) %>%
  group_by(well_id) %>%
  summarise(vol_m3_d_sd = sd(vol_m3_d, na.rm = TRUE),
            vol_m3_d_median = median(vol_m3_d, na.rm = TRUE)
            )



df_model_data <- Data
df_model_data <- df_model_data %>% left_join(df_volumes_agg) %>%
  mutate(vol_m3_d_sd = replace_na_with_median_(vol_m3_d_sd),
         vol_m3_d_median = replace_na_with_median_(vol_m3_d_median))


replace_na_with_median_ <- function(x) {
  x[is.na(x)] <- median(x, na.rm = TRUE)
  x
}

save_data(df_volumes, paths$data_prep_out, "volume_data")
filter(df_volumes, well_id == 11653)

# aggregate volume data
df_volume_year <- df_volumes %>%
  dplyr::mutate(year = lubridate::year(date)) %>%
  dplyr::group_by(well_id, year) %>%
  dplyr::summarise(vol = sum(abstracted_volume)) %>%
  na.omit()

df_volume_year %>% dplyr::filter(well_id == "5006")


# plot data availability
pdf("availability_volume_data_v6.pdf", width = 10, height = 5)
df_volume_year$well_id <- as.character(df_volume_year$well_id)
summary(df_volume_year)
ids <- unique(df_volume_year$well_id)
id_list <- split(ids, sort(rep_len(1:50, length(ids))))
df_op_start_years <- data.frame(well_id = ids) %>%
  dplyr::left_join(df_wells %>%
                     dplyr::select(well_id, operational_start.year) %>%
                     dplyr::mutate(well_id = as.character(well_id)) %>%
                     dplyr::rename(year = operational_start.year))

df_Qs <- data.frame(well_id = ids) %>%
  dplyr::left_join(df[,c("well_id", "date")] %>%
                     dplyr::mutate(well_id = as.character(well_id)) %>%
                     dplyr::mutate(year = lubridate::decimal_date(date))
  )


for (i in 1:length(id_list)) {

  df1 <- df_volume_year %>% dplyr::filter(well_id %in% id_list[[i]])
  df2 <- df_op_start_years %>% dplyr::filter(well_id %in% id_list[[i]])
  df3 <- df_Qs %>% dplyr::filter(well_id %in% id_list[[i]])

  print(ggplot2::ggplot(df1, ggplot2::aes(x = year, y = well_id, fill = vol)) +
          # abstraction volume
          ggplot2::geom_tile() +
          ggplot2::scale_fill_gradient(low = "white",
                                       high = "deepskyblue4",
                                       na.value = "grey90",
                                       limits = c(0, 1000 * 10^3),
                                       breaks = seq(0, 1000, 200) * 10^3,
                                       labels = seq(0, 1000, 200),
                                       oob = scales::squish) +
          # operational start
          ungeviz::geom_vpline(data = df2,
                               mapping = ggplot2::aes(x = year,
                                                      y = well_id,
                                                      fill = NULL),
                               height = 0.5, size = 0.5, color = "orange2") +
          ggplot2::geom_segment(data = df2,
                                mapping = ggplot2::aes(x = year,
                                                       y = well_id,
                                                       xend = year + 1.5,
                                                       yend = well_id,
                                                       fill = NULL),
                                arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "cm"),
                                                       type = "closed"),
                                colour = "orange2") +
          # # Qs measurements
          # ggplot2::geom_point(df3, mapping = ggplot2::aes(x = year, y = well_id, fill = NULL),
          #                    colour = "orange2", size = 0.5) +
          # ggplot2::geom_hline(ggplot2::aes(yintercept = seq(0, length(unique(df2$well_id))) + 0.5),
          #                     color = "grey50") +
          # ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = c(1, 2, 3)),
          #                     color = "grey50", lwd = .5) +
          ggplot2::scale_x_continuous(limits = c(1950, 2021),
                                      breaks = seq.int(1950, 2020, 10)) +
          ggplot2::scale_y_discrete(limits = rev, expand = c(0.05, 0.05)) +
          ggplot2::labs(y = "well_id", fill = "Volume [1000 m³]") +
          sema.berlin.utils::my_theme() +
          ggplot2::theme(panel.grid.major.x = ggplot2::element_line(),
                         legend.position = "top",
                         legend.key.width = ggplot2::unit(2, "cm"))
  )

  print(paste("pdf page", i, "of", length(id_list), "printed."))
}

dev.off()

# compare start of volume data with operational start of wells
df_dates <- Data %>%
  dplyr::select(well_id, date) %>%
  dplyr::mutate(abstracted_volume = NA)

df1 <- rbind(df_volumes, df_dates) %>%
  dplyr::arrange(well_id, date)

a <- df_dates %>% dplyr::group_by(well_id) %>%
  dplyr::summarise(date_start_Qs = min(date))

b <- df_volumes %>% dplyr::group_by(well_id) %>%
  dplyr::summarise(date_start_vol = min(date))

c <- dplyr::left_join(a, b, by = "well_id")

sum(c$date_start_Qs < c$date_start_vol, na.rm = TRUE)
save_data(c, paths$data_prep_out, "comparison_volume")
