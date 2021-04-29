
# write_csv --------------------------------------------------------------------
write_csv <- function(df, filename) {
  write.table(df, file = filename, dec = ".", sep = ";", row.names = FALSE)
}
