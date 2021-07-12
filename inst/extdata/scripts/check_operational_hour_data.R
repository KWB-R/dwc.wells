
# load operational hour data ---------------------------------------------------

if (FALSE) {

  # read, rename and clean data ---
  df_hours <- read_csv(paths$data_operational_hours, skip = 57) %>%
    select_rename_cols(renamings$main, "old_name", "new_name_en") %>%
    dplyr::mutate(from = as.Date(from, format = "%Y-%m-%d"),
                  to = as.Date(to, format = "%Y-%m-%d"))
  head(df_hours)


  head(df_pump_tests)
  sum(duplicated(df_hours[,c("well_id", "from", "to")]))
  df_pump_tests$pump_test_1.date
  a <- dplyr::left_join(df_pump_tests, df_hours, by = c("well_id", "pump_test_1.date" = "to"))
  summary(a$operational_hours)

  well_ids_Qs <- unique(df_Qs_all$well_id)
  well_ids_hours <- unique(df_hours$well_id)
  sum(!well_ids_hours %in% well_ids_Qs)
  sum(!well_ids_Qs %in% well_ids_hours)
  well_ids_Qs[!well_ids_Qs %in% well_ids_hours]
  length(unique(df_Qs_all$well_id))
}
