# plot_frequencies -------------------------------------------------------------
#'  plot frequency distribution of factor variable
#'
#' @param Data Data to be plotted
#' @param variable variable
#' @param title plot title
#' @param offset_perc_labels distance of labels from bars
#' @param vertical_x_axis_labels should x-axis labels be ploted vertically (TRUE / FALSE)
#'
#' @export
#'
plot_frequencies <- function(Data, variable, title = variable, offset_perc_labels = 0.1, vertical_x_axis_labels = TRUE) {

  # count frequencies
  df <- data.frame(table(Data[, variable], useNA = "ifany")) %>%
    dplyr::arrange(-Freq) %>%
    dplyr::mutate(share = Freq / sum(Freq)) %>%
    dplyr::mutate(pos = Freq + max(Freq) * offset_perc_labels) %>%
    dplyr::mutate(factor(Var1, levels = unique(Var1)))

  # plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Var1, y = Freq)) +
    ggplot2::geom_bar(stat = "identity", width = 0.7, fill = "lightblue") +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(share, accuracy = 1), y = pos)
      ) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
    #ggplot2::scale_x_discrete(expand = c(0.1, 0.1)) +
    ggplot2::labs(y = "Frequency", x = "", subtitle = title) +
    sema.berlin.utils::my_theme(
      plot.subtitle = ggplot2::element_text(face = "italic", size = 13)
    )

  if (vertical_x_axis_labels) {
    p <- p +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
  }

  p
}

# plot_distribution ------------------------------------------------------------

#' plot frequency distribution of numerical variable
#'
#' @param Data Data to be plotted
#' @param variable variable
#' @param binwidth binwidrh
#' @param title plot title
#' @param vertical_x_axis_labels should x-axis labels be ploted vertically (TRUE / FALSE)
#' @param boundary left boundary of bars, default: 0
#'
#' @export
#'
plot_distribution <- function(Data, variable, binwidth = NULL, title,
                             vertical_x_axis_labels = TRUE, boundary = 0) {

  # plot
  #p <- ggplot2::ggplot(Data, ggplot2::aes(x = .data[[variable]], y = stat(count) / sum(stat(count)))) +
  p <- ggplot2::ggplot(Data, ggplot2::aes(x = .data[[variable]])) +
    ggplot2::geom_histogram(fill = "lightblue", boundary = boundary, binwidth = binwidth) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
    # ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1),
    #                    breaks = scales::pretty_breaks(n = 6)) +
    ggplot2::labs(y = "Frequency", x = "", subtitle = title) +
    sema.berlin.utils::my_theme(
      plot.subtitle = ggplot2::element_text(face = "italic", size = 13)
    )

  if (vertical_x_axis_labels) {
    p <- p +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
  }

  p
}
