remotes::install_github("ropensci-review-tools/pkgstats")

dwc.wells_stats <- pkgstats::pkgstats()

### Functions
dwc.wells_stats_functions <- dwc.wells_stats$objects %>%
  #dplyr::filter(.data$kind == "function") %>%
  dplyr::filter(stringr::str_detect(.data$file_name, pattern = "^R")) %>%
  dplyr::group_by(.data$file_name) %>%
  dplyr::summarise(loc = sum(.data$loc)) %>%
  dplyr::mutate(group = dplyr::case_when(
    stringr::str_detect(.data$file_name, "prepare|combine|helper|remove|utils|get|docu") ~ "data preparation",
    stringr::str_detect(.data$file_name, "plot") ~ "plotting",
    stringr::str_detect(.data$file_name, "chi2") ~ "data analysis",
    stringr::str_detect(.data$file_name, "classify") ~ "modelling"
    ))

dwc.wells_stats_functions_by_group <- dwc.wells_stats_functions %>%
  dplyr::filter(!is.na(.data$group)) %>%
  dplyr::group_by(.data$group) %>%
  dplyr::summarise(loc = sum(.data$loc),
                   n_files = dplyr::n()) %>%
  dplyr::arrange(dplyr::desc(.data$loc)) %>%
  dplyr::bind_cols(tibble::tibble(type = "R functions",
                                  directory = "R")) %>%
  dplyr::relocate(.data$type, .data$directory, .before = .data$group)

dwc.wells_stats_functions_by_group


### Scripts
dwc.wells_stats_scripts <- dwc.wells_stats$objects %>%
  dplyr::filter(stringr::str_detect(.data$file_name, pattern = "_drafts", negate = TRUE)) %>%
  dplyr::filter(stringr::str_detect(.data$file_name, pattern = "^inst")) %>%
  dplyr::group_by(.data$file_name) %>%
  dplyr::summarise(loc = sum(.data$loc)) %>%
  dplyr::mutate(group = dplyr::case_when(
    stringr::str_detect(.data$file_name, "prepare|combine|helper|remove|utils|get|docu") ~ "data preparation",
    stringr::str_detect(.data$file_name, "plot") ~ "plotting",
    stringr::str_detect(.data$file_name, "chi2|exp|anal") ~ "data analysis",
    stringr::str_detect(.data$file_name, "classify|ml|importance") ~ "modelling"
  ))

dwc.wells_stats_scripts_by_group <- dwc.wells_stats_scripts %>%
  dplyr::filter(!is.na(.data$group)) %>%
  dplyr::group_by(.data$group) %>%
  dplyr::summarise(loc = sum(.data$loc),
                   n_files = dplyr::n()) %>%
  dplyr::arrange(dplyr::desc(.data$loc)) %>%
  dplyr::bind_cols(tibble::tibble(type = "R scripts",
                                  directory = "inst/extdata/scripts")) %>%
  dplyr::relocate(.data$type, .data$directory, .before = .data$group)

vignettes <- tibble::tibble(
  type = "R vignettes",
  directory = "vignettes",
  group = "modelling",
  n_files = dwc.wells_stats$vignettes[1],
  loc = dwc.wells_stats$loc$nlines[dwc.wells_stats$loc$language == "Rmd"]
  )

dwc.wells_stats_by_group <- dwc.wells_stats_functions_by_group %>%
  dplyr::bind_rows(dwc.wells_stats_scripts_by_group) %>%
  dplyr::bind_rows(vignettes)

pdff <- "dwcWells_linesOfCode.pdf"
kwb.utils::preparePdf(pdfFile = pdff,
                      width.cm = 20,
                      height.cm = 15,
                      )
dwc.wells_stats_by_group %>%
  dplyr::mutate(label = sprintf("%s (directory: ./%s)", .data$type, .data$directory)) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(x = .data$group,
                                         y = .data$loc,
                                         fill = .data$group)) +
  ggplot2::scale_fill_discrete("Task") +
  ggplot2::facet_wrap(~ label, ncol = 1) +
  ggplot2::geom_col() +
  ggplot2::labs(x = "",
                y = "Lines of Code") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "right")
kwb.utils::finishAndShowPdf(pdff)


dwc.wells_stats_by_type <- dwc.wells_stats_by_group %>%
  dplyr::ungroup() %>%
  dplyr::group_by(.data$type, .data$directory) %>%
  dplyr::summarise(loc = sum(.data$loc)) %>%
  dplyr::rename(lines_of_code = .data$loc)


gg <- dwc.wells_stats_by_group %>%
  dplyr::filter(.data$directory != "vignettes") %>%
  dplyr::group_by(.data$group) %>%
  dplyr::summarise(loc = sum(.data$loc)) %>%
  ggplot2::ggplot(ggplot2::aes(x = "",
                               y = .data$loc,
                               fill = forcats::fct_reorder(.data$group,
                                                           .data$loc))) +
  ggplot2::geom_bar(position = "fill",
                    stat = "identity") +
  ggplot2::scale_fill_manual("Task", values=c("#F8766D", "#C77CFF", "#00BFC4", "#7CAE00")) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::labs(x = "",
                y = "Lines of Code (%)") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "right")

pdff <- "dwcWells_linesOfCode_byTask.pdf"
kwb.utils::preparePdf(pdfFile = pdff,
                      width.cm = 15,
                      height.cm = 10,
)
gg
kwb.utils::finishAndShowPdf(pdff)
