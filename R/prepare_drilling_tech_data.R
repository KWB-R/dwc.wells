# prepare_drilling_tech_data ---------------------------------------------------

prepare_drilling_tech_data <- function(path, renamings) {

  # read drilling tech data from tsv
  df_drilling_tech <- read_csv(path, skip = 2) %>%
    remove_column_with_duplicated_name() %>%
    select_rename_cols(renamings$main,"old_name", "new_name_en")

  # remove NAs
  df_drilling_tech <- df_drilling_tech[!is.na(df_drilling_tech$drilling_method),]

  # remove duplicates
  #df_drilling_tech <- df_drilling_tech[!duplicated(df_drilling_tech),]
  df_drilling_tech <- df_drilling_tech[!duplicated(df_drilling_tech$drilling_id),]

df_drilling_tech$drilling_method
  # regroup and tidy surface waters
  df_drilling_tech <- df_drilling_tech %>%
    dplyr::mutate(drilling_method =
                    rename_values(.data$drilling_method, renamings$drilling_method) %>%
                    tidy_factor)


  # return data frame with relevant columns
  df_drilling_tech[, c("drilling_id", "drilling_method")]

}
