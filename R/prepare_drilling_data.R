# prepare drilling data --------------------------------------------------------

prepare_drilling_data <- function(path, renamings) {

# read drilling data from csv
# does not work with standard encoding
df_drilling <- read.csv(file = path, header = TRUE, skip = 2, dec = ".",
                        sep = "\t", na.strings = "(null)") %>%
  select_rename_cols(renamings$main, "old_name", "new_name_en")

# adapt encoding for surface water name (otherwise umlaute will not be read)
Encoding(df_drilling$surface_water) <- "UTF-8"


# regroup and tidy surface waters
df_drilling <- df_drilling %>%
  dplyr::mutate(surface_water =
                  rename_values(.data$surface_water, renamings$surface_water) %>%
                  tidy_factor)


# tidy distance to surface water
df_drilling <- df_drilling %>%
  dplyr::mutate(surface_water.distance =
                  tidy_factor(.data$surface_water.distance) %>%
                  factor(levels = c("0-25", "25-50", "50-100", "100-200",
                                    "200-500", "500-1000", ">1000", "Unbekannt"))
  )

df_drilling

}
