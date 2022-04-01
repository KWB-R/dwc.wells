# correlation_plot ------------------------------------------------------------

#' plots Qs_rel vs. input variable as box plot (categorical input variable)
#' or scatterplot (numerical input variable)
#'
#' @param df data frame
#' @param x column name of x variable"
#' @param y column name of y variable (default Qs_rel")
#' @param title plot title
#'
#' @export
#'
correlation_plot <- function(df, x, y = "Qs_rel", title = gsub("_", " ", x)) {

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::scale_y_continuous(labels = function(x) { paste0(x, "%") },
                                breaks = scales::pretty_breaks()) +
    sema.berlin.utils::my_theme() +
    #theme(plot.subtitle = element_text(color = "deepskyblue4", face = "italic")) +
    ggplot2::labs(x = "", title = title)


  if (is.numeric(df[, x])) {
    p <- p +  ggplot2::geom_point(size = 0.8, shape = 16, colour = "grey20", alpha = 0.3)
  } else {
    p <- p + ggplot2::geom_boxplot(width = 0.5)

    if (max(nchar(unique(as.character(df[, x]))), na.rm = TRUE) > 3) {
      p <- p +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
    }
  }

  p
}


# plot_Qs_over_time -------------------------------------------------------------
plot_Qs_over_time <- function(df, xmax = 40, legend_position = "top") {

  p <- ggplot2::ggplot(df, ggplot2::aes(x = well_age_years, y = Qs_rel,
                                   col = n_rehab, shape = key2)) +
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
#' @param size_perc_labels size of percent labels
#' @param vertical_x_axis_labels should x-axis labels be ploted vertically (TRUE / FALSE)
#'
#' @export
#'
plot_frequencies <- function(Data,
                             variable,
                             title = variable,
                             offset_perc_labels = 0.1,
                             size_perc_labels = 3,
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
      ggplot2::aes(label = scales::percent(.data[["share"]], accuracy = 1),
                   y = .data[["pos"]]),
      size = size_perc_labels
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



# my.corrplot.mixed ------------------------------------------------------------
my.corrplot.mixed <- function(data, cl.lim = c(-1,1), number.cex = 1.0) {
  corrplot::corrplot.mixed(data, lower = "number", upper = "color", outline = TRUE,
                           tl.cex = 1.0, tl.pos = "lt", tl.col = "black", diag = "u",
                           cl.cex = 1.0, cl.ratio = 0.2, number.cex = number.cex, cl.lim = cl.lim)
}


# my.corrplot ------------------------------------------------------------------
my.corrplot <- function(data, cl.lim = c(-1,1)) {
  corrplot::corrplot(data, method = "color", outline = TRUE, tl.cex = 1.1, cl.lim = cl.lim,
                     tl.col = "black", cl.cex = 1.0, cl.ratio = 0.2)
}


# paste_percent ----------------------------------------------------------------
#' Paste percent sign to numbers
#'
#' @param x numeric vector
#' @export
#'
paste_percent <- function(x) { paste0(x, "%") }


# Qs_heatmap_plot --------------------------------------------------------------

#' Heatmap / raster plot for Qs values over time with each well as one line
#'
#' @param df data frame with date, well_id, Qs_rel
#' @param colours 3 colours for low, middle and high colour limits
#' @param dummy_labels dummy labels if there are less wells than expected
#' @param date_limits vector with two date strings in format "yyyy-mm-dd"
#' @param title plot title
#' @param n_wells_per_page number of wells do be shown
#'
#' @export
#'
Qs_heatmap_plot <- function(df, colours, dummy_labels, date_limits, title,
                            n_wells_per_page) {

  ggplot2::ggplot(df, ggplot2::aes(x = .data$date,
                                   y = .data$well_id,
                                   fill = .data$Qs_rel)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradient2(low = unname(colours["red"]),
                                  mid = unname(colours["yellow"]),
                                  high = unname(colours["green"]),
                                  midpoint = 50,
                                  na.value = "grey90",
                                  limits = c(0, 100),
                                  breaks = seq(0, 100, 20),
                                  labels = paste0(seq(0, 100, 20), "%"),
                                  oob = scales::squish,
                                  guide = ggplot2::guide_colourbar(reverse = TRUE)) +
    ggplot2::scale_y_discrete(limits = function(x) {rev(c(x, dummy_labels)[1:n_wells_per_page])},
                              labels = function(x) {stringr::str_sub(stringr::str_pad(x, 5, "left"), 1, 5)},
                              expand = c(0.05, 0.05)) +
    ggplot2::scale_x_date(limits = as.Date(date_limits),
                          breaks = scales::pretty_breaks(8)) +
    ggplot2::labs(y = "well_id", x = "Years", fill = "Specific\ncapacity",
                  title = title) +
    sema.berlin.utils::my_theme() +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(),
                   legend.position = "top",
                   legend.key.width = ggplot2::unit(2, "cm"),
                   legend.key.height = ggplot2::unit(0.35, "cm"),
                   legend.spacing.x = ggplot2::unit(0.8, "cm"),
                   axis.title = ggplot2::element_text(face = "plain"),
                   legend.title = ggplot2::element_text(face = "plain"),
                   title = ggplot2::element_text(size = 12)) +
    ggplot2::geom_hline(yintercept = seq(0.5, 20, 1), color = "white", lwd = 1)

}



# scatterplot ------------------------------------------------------------------
#' scatterplot for comparing numeric predictions with observations
#'
#' @param df_pred data frame obtained with tidymodels::collect_predictions() with
#' columns Qs_rel and .pred
#' @param lines_80perc logical value; shout 80%-lines be drawn?; default = FALSE
#' @param alpha alpha value for point of colours, default: 1
#' @param pointsize size value for points, default: 1
#' @export
#' @import ggplot2
#' @importFrom yardstick rmse rsq
#' @importFrom sema.berlin.utils my_theme
scatterplot <- function(df_pred, lines_80perc = FALSE, alpha = 1, pointsize = 1) {

  # error metrics
  a <- df_pred %>% yardstick::rmse(truth = .data$Qs_rel, estimate = .data$.pred)
  b <- df_pred %>% yardstick::rsq(truth = .data$Qs_rel, estimate = .data$.pred)

  legend_str <- sprintf("r2 = %.2f\nRMSE = %.1f%%", b$.estimate, a$.estimate)

  p <- ggplot2::ggplot(df_pred, ggplot2::aes(x = .data$Qs_rel, y = .data$.pred)) +
    ggplot2::geom_point(pch = 16,
                        col = ggplot2::alpha("black", alpha),
                        size = pointsize) +
    ggplot2::geom_abline(color = 'blue', linetype = 2) +
    ggplot2::scale_x_continuous(lim = c(0, 100), labels = paste_percent) +
    ggplot2::scale_y_continuous(lim = c(0, 100), labels = paste_percent) +
    ggplot2::labs(x = "observations", y = "predictions") +
    sema.berlin.utils::my_theme() +
    ggplot2::annotate("text", x = 100, y = 5, hjust = 1, col = "grey30", size = 3,
             label = legend_str)

  if (lines_80perc) {
    # p + geom_rect(aes(xmin = 0, xmax = 80, ymin = 0, ymax = 80),
    #                   colour = "red", fill = NA, lty = "dashed")
    p +
    ggplot2::geom_hline(yintercept = 80, colour = "red", lty = "dashed") +
    ggplot2::geom_vline(xintercept = 80, colour = "red", lty = "dashed")

  } else {
    p
  }

}
