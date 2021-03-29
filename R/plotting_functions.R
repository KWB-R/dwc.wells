plot_frequencies <- function(Data, variable, title, vertical_x_axis_labels = TRUE) {

  # count frequencies
  df <- data.frame(table(Data[, variable], useNA = "ifany")) %>%
    dplyr::arrange(-Freq) %>%
    dplyr::mutate(share = Freq / sum(Freq)) %>%
    dplyr::mutate(pos = Freq + max(Freq) * 0.1) %>%
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


plot_distribution <- function(Data, variable, binwidth = NULL, title,
                             vertical_x_axis_labels = TRUE) {

  # plot
  p <- ggplot2::ggplot(Data, ggplot2::aes(x = .data[[variable]], y = stat(count) / sum(stat(count)))) +
    ggplot2::geom_histogram(fill = "lightblue", boundary = 0, binwidth = binwidth) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       breaks = scales::pretty_breaks(n = 6)) +
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
