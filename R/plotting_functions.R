#  correlation_plot ------------------------------------------------------------

#' plots Qs_rel vs. input variable as box plot (categorical input variable)
#' or scatterplot (numerical input variable)
#'
#' @param df data frame
#' @param x column name of x variable"
#' @param y column name of y variable (default Qs_rel")
#' @param title title (default: gsub("_", " ", x))
#'
#' @export
#' @import ggplot2
#' @importFrom sema.berlin.utils my_theme
correlation_plot <- function(df, x, y = "Qs_rel", title = gsub("_", " ", x)) {

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                breaks = scales::pretty_breaks()) +
    sema.berlin.utils::my_theme() +
    #theme(plot.subtitle = element_text(color = "deepskyblue4", face = "italic")) +
    ggplot2::labs(x = "", title = title)


  if (is.numeric(df[, x])) {
    p <- p +  ggplot2::geom_point(size = 0.8)
  } else {
    p <- p + ggplot2::geom_boxplot(width = 0.5)

    if (max(nchar(unique(as.character(df[, x]))), na.rm = TRUE) > 3) {
      p <- p +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    }
  }

  p
}


#' plot_Qs_over_time
#'
#' @param df data frame
#' @param xmax limit for maximum of x-axis (default: 40)
#' @param legend_position default: "top"
#'
#' @return plot Qs over time
#' @export
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales percent pretty_breaks rescale_none
plot_Qs_over_time <- function(df, xmax = 40, legend_position = "top") {

  p <- ggplot2::ggplot(df, ggplot2::aes_string(x = "well_age_years",
                                        y = "Qs_rel",
                                        col = "n_rehab",
                                        shape = "key2")) +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(group = "all")) +
    ggplot2::scale_color_manual(values = rev(RColorBrewer::brewer.pal(length(levels(df$n_rehab)), "RdYlGn"))) +
    #ggplot2::scale_color_manual(values = rev(scales::hue_pal()(length(levels(df$n_rehab))))) +
    ggplot2::scale_x_continuous(
      limits = c(0, xmax),
      breaks = scales::pretty_breaks()) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1.4),
      labels = scales::percent,
      breaks = scales::pretty_breaks(),
      oob = scales::rescale_none) +
    sema.berlin.utils::my_theme(legend.position = legend_position) +
    ggplot2::labs(color = "n_rehab:", shape = "data type:",
         x = "Well age [yrs]", y = "Qs_rel")

  if (legend_position == "top") {
    p + ggplot2::guides(color = ggplot2::guide_legend(nrow = 1))
  } else {
    p
  }
}

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
plot_frequencies <- function(Data,
                             variable,
                             title = variable,
                             offset_perc_labels = 0.1,
                             vertical_x_axis_labels = TRUE) {

  # count frequencies
  df <- data.frame(table(Data[, variable], useNA = "ifany")) %>%
    dplyr::arrange(-.data[["Freq"]]) %>%
    dplyr::mutate(share = .data[["Freq"]] / sum(.data[["Freq"]])) %>%
    dplyr::mutate(pos = .data[["Freq"]] + max(.data[["Freq"]]) * offset_perc_labels) %>%
    dplyr::mutate(factor(.data[["Var1"]], levels = unique(.data[["Var1"]])))

  # plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["Var1"]], y = .data[["Freq"]])) +
    ggplot2::geom_bar(stat = "identity", width = 0.7, fill = "lightblue") +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(.data[["share"]], accuracy = 1), y = .data[["pos"]])
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
#' @importFrom sema.berlin.utils my_theme
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

