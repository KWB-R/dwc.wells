# extdata_file -----------------------------------------------------------------

#' Get Path to File in This Package
#'
#' @param \dots parts of path passed to \code{\link{system.file}}
#' @export
extdata_file <- function(...) {
  system.file("extdata", ..., package = "dwc.wells")
}
