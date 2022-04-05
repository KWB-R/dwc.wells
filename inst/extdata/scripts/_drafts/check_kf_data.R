# MAIN tmp: append kf values ---------------------------------------------------

if (FALSE) {

  df_kf <- read_csv(file = paths$data_kf, sep = ";", dec = ",", skip = 0)

  df_kf_ <- read_csv(file = paths$data_kf_, skip = 2)
  sum(unique(df_wells$drilling_id) %in% unique(df_kf_$BOHRUNGSNUMMER))
  sum(unique(df_kf_$BOHRUNGSNUMMER) %in% unique(df_wells$drilling_id))
  kf_drilling_ids <- unique(df_kf_$BOHRUNGSNUMMER)
  kf_drilling_ids_wells <- kf_drilling_ids[kf_drilling_ids %in% unique(df_wells$drilling_id)]
  table(df_wells$operational_state, data_available = df_wells$drilling_id %in% kf_drilling_ids)
  table(df_wells$operational_state, useNA = "ifany")


  names(df_kf) <- c("well_name", "kf")
  length(unique(df_kf_$BOHRUNGSNUMMER))
  length(unique(df_kf_$BOHRUNGSNUMMER))
  table(df_wells$operational_state, kf_available = !is.na(df_wells$kf))

  sum(df_kf$well_name %in% df_wells$well_name)
  df_wells <- df_wells %>% dplyr::left_join(df_kf, by = "well_name")
  str(df_wells$kf)
  summary(df_wells$kf)
}
