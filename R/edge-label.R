#' Stat Edge Label
#'
#' A `ggplot2` stat that takes the labels for series of data and
#' labels each series at its right-most point.
#' @importFrom ggplot2 layer
#' @export
stat_edgelabel <- function(data = NULL,
                          mapping = NULL,
                          position = 'identity',
                          ...,
                          inherit.aes = TRUE) {

  ggplot2::layer(data = data,
                 mapping = mapping,
                 stat = ggplot2::ggproto('StatEdgeLabel',
                                         ggplot2::Stat,
                                         compute_layer = compute_edgelabel,
                                         required_aes = c('x', 'y', 'label')),
                 geom = 'text',
                 position = position,
                 show.legend = FALSE,
                 inherit.aes = inherit.aes,
                 check.aes = FALSE,
                 check.param = FALSE,
                 params = list(...))
}

#' Compute Edge Label
#'
#' This is the computational engine for stat_edgelabel.  It takes
#' the available data a grabs the right-most values of each group,
#' where it places a label.
#' @param self A ggproto object
#' @param data The data object supplied to the ggplot object
#' @param params List of parameters supplied to the stat_edgelabel
#' function
#' @param layout Object providing infomration about the plot layout,
#' including the range of th x and y axes
#' @return A data.frame with the processed data.
#' @importFrom dplyr filter group_by arrange slice mutate ungroup
compute_edgelabel <- function (self, data, params, layout) {

  nudge_x <- if ('nudge_x' %in% names(params)) params$nudge_x else 0
  if ('panel' %in% names(params))
    data <- dplyr::filter(data, PANEL == params$panel)

  group_by(data, PANEL, group) %>%
    dplyr::filter(!is.na(y), !is.na(x)) %>%
    arrange(PANEL, group, desc(x)) %>%
    slice(1L) %>%
    mutate(x = x + nudge_x) %>%
    ungroup()
}

