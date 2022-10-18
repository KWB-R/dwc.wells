#' Plot Predictions: Points per Well
#'
#' @param predictions predictions as retrieved by \code{\link{get_predictions}}
#' @param max_simulation_years maximum years to be used as xlim, if not provided
#' (NULL) the full simulation period is used (default: NULL)
#' @param dbg print debug messages
#'
#' @return plots one page per well
#' @export
#' @importFrom kwb.utils catAndRun
#' @importFrom ggplot2 aes_string ggplot scale_color_discrete scale_x_continuous
#' scale_y_continuous geom_point theme_bw
#' @importFrom ggforce facet_wrap_paginate
plot_predictions_points_per_well <- function(predictions,
                                             max_simulation_years = NULL,
                                             dbg = TRUE) {

  if(is.null(max_simulation_years)) {
    max_simulation_years <- max(predictions$well_age_years)
  }

  well_ids <- unique(predictions$well_id)
  n_wells <- length(well_ids)

  for(i in seq_len(n_wells)) {
    kwb.utils::catAndRun(messageText = sprintf("Plotting well_id '%s' (%d/%d)",
                                               well_ids[i],
                                               i,
                                               n_wells),
                         expr = {
                           p <- ggplot2::ggplot(predictions,
                                                ggplot2::aes_string(x = "well_age_years",
                                                                    y = "Qs_rel",
                                                                    col = "type")) +
                             ggplot2::scale_color_discrete(name = "Type") +
                             ggplot2::scale_x_continuous(limits = c(0, max_simulation_years)) +
                             ggplot2::scale_y_continuous(limits = c(0, 100),
                                                         oob = scales::rescale_none) +
                             ggplot2::geom_point(alpha = 0.5) +
                             ggforce::facet_wrap_paginate(~ .data$well_id,
                                                          nrow = 1,
                                                          ncol = 1,
                                                          page = i) +
                             ggplot2::theme_bw()
                           print(p)
                         },
                         dbg = dbg)
  }

}
