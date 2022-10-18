#' Remove Column with Duplicated Name
#'
#' @param data data frame
#'
#' @return data frame without duplicated column
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom kwb.utils allAreIdentical
#'
remove_column_with_duplicated_name <- function(data)
{
  duplicated_values <- function(x) x[duplicated(x)]

  duplicated_columns <- duplicated_values(names(data))

  n_duplicated <- length(duplicated_columns)

  stopifnot(n_duplicated <= 1L)

  if (n_duplicated == 0L) {
    return(data)
  }

  is_duplicated <- names(data) == duplicated_columns
  stopifnot(kwb.utils::allAreIdentical(data[is_duplicated]))

  data[- which(is_duplicated)[-1L]]
}
