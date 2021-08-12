# classify_Qs ------------------------------------------------------------------

#' Transfor Qs_rel into binary factor with low and high specific capacity
#'
#' @param x vector of Qs_rel values
#' @param split_point threshold for classifying numeric Qs_rel values, default: 80
#' @param class_names class names, default: c("low", "high")
#'
#' @export
#'
classify_Qs <- function(x, split_point = 80, class_names = c("low", "high")) {

  factor(
    dplyr::if_else(x < split_point, class_names[1], class_names[2]),
    levels = class_names)

}

