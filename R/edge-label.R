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

  #browser()
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

  #browser()
  #nudge_x <- if ('nudge_x' %in% names(params)) params$nudge_x else 0
  if ('panel' %in% names(params))
    data <- dplyr::filter(data, PANEL == params$panel)

  group_by(data, PANEL, group) %>%
    dplyr::filter(!is.na(y), !is.na(x)) %>%
    arrange(PANEL, group, desc(x)) %>%
    slice(1L) %>%
    mutate(nudge_y = nudge_y_labels(label, self$nudge_y)) %>%
    mutate(label = pad_label(label, self$nudge_x)) %>%
    ungroup()
}

#' geom_edgelabel
#'
#' Adds labels to the right of the plot for each group in the data. The
#' labels appear to the right of the farthest right point of any of the
#' plotted series.
#' @param nudge_x The number of characters to move each label to the right.
#' A value of "1L" adds a single empty space to each label.  A value of 2L
#' adds two spaces.  Default is 0.
#' @param nudge_y Named character vector to use to nudge the y-values.  Nudges
#' should be expressed in 'npc' units, which capture the percentage of the
#' plot area. So setting nudge_y = 2L will move each label up 2% of the viewable
#' plot area.
#' @export
geom_edgelabel <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE, hjust = 0,
                           size = 10, inherit.aes = TRUE, nudge_y = 0,
                           nudge_x = 0, ...) {

  size <- size / .pt  # Convert to points

  layer(geom = GeomEdgeLabel,
        mapping = mapping,
        data = data,
        stat = ggplot2::ggproto('StatEdgeLabel',
                                ggplot2::Stat,
                                compute_layer = compute_edgelabel,
                                required_aes = c('x', 'y', 'label'),
                                nudge_x = nudge_x,
                                nudge_y = nudge_y),
        position = position,
        show.legend = FALSE,
        inherit.aes = inherit.aes,
        check.param = FALSE,
        params = list(na.rm = na.rm,
                      hjust = hjust,
                      size = size,
                      ...))
}

draw_edgelabel <- function(data, panel_params, coord) {

  data <- coord$transform(data, panel_params)

  heights <- unit(data$size * .pt * data$lineheight, 'pt') %>%
    grid::convertHeight('npc', valueOnly = TRUE)

  ret_data <- data %>%
    mutate(heights = heights * 1.4,
           order = seq_along(data$y)) %>%
    arrange(desc(y)) %>%
    mutate(y = change_spacing(y, heights)) %>%
    arrange(order) %>%
    mutate(x = x,
           y = y + (nudge_y / 100)) %>%
    select(-order)

  textGrob(ret_data$label,
           ret_data$x,
           ret_data$y,
           default.units = 'npc',
           hjust = ret_data$hjust,
           vjust = ret_data$vjust,
           rot = ret_data$angle,
           gp = gpar(col = alpha(ret_data$colour, ret_data$alpha),
                     fontsize = ret_data$size * .pt,
                     fontfamily = ret_data$family,
                     fontface = ret_data$fontface,
                     lineheight = ret_data$lineheight),
           check.overlap = FALSE)
}

GeomEdgeLabel <- ggplot2::ggproto("GeomEdgeLabel",
                                  ggplot2::Geom,
                                  required_aes = c("x", "y", "label"),
                                  # These default values (mostly) come from GeomText
                                  default_aes = ggplot2::aes(colour = "black",
                                                             size = 3.88,
                                                             angle = 0,
                                                             hjust = 0,
                                                             vjust = 0.5,
                                                             alpha = 1L,
                                                             family = "",
                                                             fontface = 1,
                                                             lineheight = 1.2),
                                  draw_panel = draw_edgelabel)

compute_grids <- function(i, dt) {

  dtr <- slice(dt, i)

  #browser()
  ret_grob <- grid::textGrob(label = dtr$label,
                             x = dtr$x,
                             y = dtr$y,
                             hjust = dtr$hjust,
                             vjust = dtr$vjust,
                             default.units = 'npc',
                             gp = gpar(font = dtr$fontface,
                                       fontfamily = dtr$family,
                                       lineheight = dtr$lineheight,
                                       fontsize = dtr$size * .pt,
                                       alpha = dtr$alpha),
                             vp = NULL)

  calc_grob_height(ret_grob, 'npc')
}

#' Pad Label
#'
#' Adds additional empty spaces before the labels being displayed along
#' the edge. This allows the x position of each label to remain at the
#' same point as the data to avoid the labels being completely dropped
#' by the limit set for the x-axis.
#' @param x A vector of character labels to be displayed
#' @param s Integer scalar giving the number of blank spaces to use in
#' nudging the label to the right.
#' @return A character vector with the extra blank spaces added
pad_label <- function(x, s = 1L) {
  sprintf('%s%s',
          paste0(rep(' ', as.integer(s)), collapse = ''),
          x)
}

#' Nudge Y Labels
#'
#' Creates a vector of nudges to apply to the y-axis of the label.
#' @param label Character vector of labels
#' @param nudge_y Named character vector supplied by the  user.  The name
#' of each element will correspond to the label to which the nudge is to
#' be applied.
#' @return A numeric vector of nudges
nudge_y_labels <- function(label, nudge_y) {

  #browser()
  if (length(nudge_y) == 1L & is.null(names(nudge_y)))
    return(rep(nudge_y, length(label)))

  if (!is.null(names(nudge_y))) {
    new_nudge <- nudge_y[label]
    return(ifelse(is.na(new_nudge), 0, as.numeric(new_nudge)))
  }

  stop('Invalid nudge_y supplied to geom_edgelabel.')
}

change_spacing <- function(y, h) {
  dt <- make_tiles(y, h) %>%
    adjust_bounds() %>%
    check_overlaps()

  while(max(dt$group) < length(dt$group)) {
    #browser()
    dt <- combine_tiles(dt) %>%
      adjust_bounds() %>%
      check_overlaps()
  }

  assign_newly_spaced_ys(dt) %>%
    pull(y)
}

assign_newly_spaced_ys <- function(data) {
  purrr::map_df(seq_along(data$y), .assign_newly_spaced_ys, data = data)
}

.assign_newly_spaced_ys <- function(i, data) {
  dt <- slice(data, i)

  dt$data[[1L]][[1L]] %>%
    mutate(y = dt$top - cumsum(h) + (0.5 * h)) %>%
    select(y)
}

make_tiles <- function(y, h) {
  my_data <- purrr::map(seq_along(y), ~ list(tibble::tibble(y = y[.x], h = h[.x])))

  tibble::tibble(y = y, height = h) %>%
    arrange(desc(y)) %>%
    mutate(order = seq_along(y),
           top = y + 0.5 * height,
           bottom = y - 0.5 * height,
           data = my_data)
}


check_overlaps <- function(dt) {
  mutate(dt,
         rel_pos = ifelse(dplyr::row_number() == 1L,
                          1 - top,
                          dplyr::lag(bottom, n = 1L) - top),
         no_overlap = as.logical(rel_pos > 0),
         group = 0) %>%
    # Combine tiles that overlap
    mutate(group = cumsum(no_overlap)) %>%
    select(y, height, order, top, bottom, group, data)
}

combine_tiles <- function(dt) {
  purrr::map_dfr(unique(dt$group), .combine_tiles, dt = dt)
}

# This function will be executed once for each group
# in dt.
.combine_tiles <- function(i, dt) {

  #browser()
  temp_dt <- filter(dt, group == i)

  if (dim(temp_dt)[1L] == 1L) {
    ret_dt <- temp_dt %>%
      select(y, height, order, top, bottom, data)
    return(ret_dt)
  }

  ret_dt <- temp_dt %>%
    mutate(alpha_i = y + cumsum(height) - (0.5 * height),
           alpha = mean(alpha_i)) %>%
    summarize(y = max(alpha) - (0.5 * sum(height)),
              height = sum(height),
              order = min(order),
              top = max(alpha),
              bottom = max(alpha) - sum(height)) %>%
    mutate(data = list(list(bind_rows(temp_dt$data))))
}

adjust_bounds <- function(data) {
  mutate(data,
         adjustment = dplyr::case_when(
           top > 1 ~ 1 - top,
           bottom < 0 ~ -1 * bottom,
           TRUE ~ 0)) %>%
    mutate(y = y + adjustment,
           top = top + adjustment,
           bottom = bottom + adjustment)
}

