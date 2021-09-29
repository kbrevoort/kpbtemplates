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
#' @export
compute_edgelabel <- function (self, data, params, layout) {

  #browser()
  #nudge_x <- if ('nudge_x' %in% names(params)) params$nudge_x else 0
  if ('panel' %in% names(params))
    data <- dplyr::filter(data, PANEL == params$panel)

  #browser()

  #cat('In compute_edgelabel\n\n')

  group_by(data, PANEL, group) %>%
    dplyr::filter(!is.na(y), !is.na(x)) %>%
    arrange(PANEL, group, desc(x)) %>%
    slice(1L) %>%
    ungroup() %>%
    mutate(x = max(x, na.rm = TRUE))

  #browser()
}

StatEdgeLabel <- ggplot2::ggproto('StatEdgeLabel',
                                  ggplot2::Stat,
                                  compute_layer = compute_edgelabel,
                                  required_aes = c('x', 'y', 'label'),
                                  nudge_x = 0,
                                  nudge_y = 0)

#' @importFrom ggplot2 layer ggproto
#' @export
geom_edgelabel <- function(mapping = NULL,
                           data = NULL,
                           stat = kpbtemplates::StatEdgeLabel,
                           position = "identity",
                           ...,
                           use_edge = FALSE,
                           label.padding = 0.25,
                           nudge_x = 0,
                           nudge_y = 0,
                           na.rm = FALSE,
                           inherit.aes = TRUE) {

  #cat('I am here.\n\n')

  layer(data = data,
        mapping = mapping,
        stat = stat,
        geom = kpbtemplates::GeomEdgeLabel,
        position = position,
        show.legend = FALSE,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm,
                      use_edge = use_edge,
                      label.padding = grid::unit(label.padding, 'lines'),
                      nudge_x = nudge_x,
                      nudge_y = nudge_y,
                      ...))
}

#' @importFrom grid setChildren makeContent
#' @export
makeContent.edgelabel <- function(x) {

  #cat('I am here now.\n\n')

  #browser()

  tiles <- kpbtemplates::make_tiles(obj = x) %>%
    kpbtemplates::adjust_bounds() %>%
    kpbtemplates::check_overlaps() %>%
    kpbtemplates::group_tiles() %>%
    kpbtemplates::update_y_locations()

  x$data <- select(tiles, -height)

  grobs <- lapply(seq_along(tiles$y),
                  kpbtemplates::create_edgelabel_grobs,
                  obj = x)

  grobs <- unlist(grobs, recursive = FALSE)
  class(grobs) <- "gList"

  grid::setChildren(x, grobs)
}

#' @importFrom grid textGrob gpar gList
#' @importFrom scales alpha
create_edgelabel_grobs <- function(i, obj) {

  #cat('In create_edgelabel_grobs\n\n')

  row <- slice(obj$data, i)

  line_diff <- grid::convertUnit(obj$label.padding, unitTo = 'native', valueOnly = TRUE)
  x_pos <- ifelse(obj$use_edge, 1, pmin(row$x + obj$nudge_x + line_diff, 1))

  grobs <- textGrob(label = row$label,
                    x = x_pos,
                    y = row$y,
                    rot = row$angle,
                    hjust = row$hjust,
                    vjust = row$vjust,
                    default.units = 'native',
                    check.overlap = FALSE,
                    name = sprintf('edgelabel_%02d', i),
                    gp = gpar(col = scales::alpha(row$colour,
                                                  row$alpha),
                              fontsize = row$size * .pt,
                              fontfamily = row$family,
                              fontface = row$fontface,
                              lineheight = row$lineheight),
                    vp = NULL)

  grid::gList(grobs)
}

#' @importFrom grid gTree
draw_edgelabel <- function(data,
                           panel_params,
                           coord,
                           use_edge = FALSE,
                           label.padding = 0.25,
                           nudge_x = 0,
                           nudge_y = 0) {

  if (!grid::is.unit(label.padding))
    label.padding = grid::unit(label.padding, 'lines')

  #cat('In draw edge label\n\n')

  grob <- gTree(data = coord$transform(data, panel_params),
                #name = 'geom_edgelabel',
                use_edge = use_edge,
                label.padding = label.padding,
                nudge_x = nudge_x,
                nudge_y = nudge_y,
                cl = 'edgelabel')
  grob$name <- grid::grobName(grob, 'geom_edgelabel')
  grob
}

#' @importFrom ggplot2 ggproto aes Geom draw_key_text
#' @export
GeomEdgeLabel <- ggplot2::ggproto("GeomEdgeLabel",
                                  ggplot2::Geom,
                                  required_aes = c("x", "y", "label"),
                                  default_aes = ggplot2::aes(colour = "black",
                                                             size = 3.88,
                                                             angle = 0,
                                                             alpha = NA,
                                                             family = "",
                                                             fontface = 1,
                                                             lineheight = 1.2,
                                                             hjust = 0, # 0.5,
                                                             vjust = 0.5),
                                  draw_panel = draw_edgelabel,
                                  draw_key = ggplot2::draw_key_text)


#' @importFrom purrr map_dfr
#' @importFrom dplyr arrange mutate relocate
#' @export
make_tiles <- function(obj) {
  #cat('In make_tiles.\n\n')

  purrr::map_dfr(seq_along(obj$data$y),
                 .make_tile,
                 obj = obj) %>%
    arrange(desc(y)) %>%
    mutate(order = seq_along(y),
           top = y + 0.5 * height,
           bottom = y - 0.5 * height) %>%
    relocate(y, height, order, top, bottom, data)
}

#' @importFrom grid convertHeight
#' @importFrom tibble tibble
.make_tile <- function(i, obj) {
  # The object passed here, obj, is the one gList called 'geom_edgelabel

  # The padding around each bounding box.
  add_y <- convertHeight(obj$label.padding, "native", valueOnly = TRUE)

  row <- obj$data[i, , drop = FALSE]
  tg <- create_edgelabel_grobs(i, obj = obj)

  y1 <- convertHeight(grobY(tg, "south"), "native", TRUE) - add_y + obj$nudge_y
  y2 <- convertHeight(grobY(tg, "north"), "native", TRUE) + add_y + obj$nudge_y

  row$height <- y2 - y1
  tibble::tibble(y = mean(c(y1, y2), na.rm = TRUE),  # This should equal row$y
                 height = y2 - y1,
                 data = list(row))
}

#' @export
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

#' @export
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

#' @importFrom purrr map_dfr
#' @export
combine_tiles <- function(dt) {
  purrr::map_dfr(unique(dt$group), .combine_tiles, dt = dt)
}

# This function will be executed once for each group
# in dt.
#' @importFrom dplyr filter summarize mutate
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
    mutate(data = list(bind_rows(temp_dt$data)))
}

#' @export
group_tiles <- function(orig_tiles) {
  tiles <- orig_tiles
  iter <- 0L

  while (length(tiles$group) > length(unique(tiles$group))) {
    iter <- iter + 1
    stopifnot(iter < 1000)

    tiles <- combine_tiles(tiles) %>%
      adjust_bounds() %>%
      check_overlaps()
  }

  tiles
}

#' @importFrom purrr map_df
#' @export
update_y_locations <- function(tiles) {
  purrr::map_df(seq_along(tiles$y), .update_y_locations, tiles = tiles)
}

#' @importFrom dplyr arrange mutate
.update_y_locations <- function(i, tiles) {
  #browser()
  row <- tiles[i, , drop = FALSE]

#  cat('Starting 2')
  out_dt <- row$data[[1L]] %>%
    arrange(y) %>%  # Sort for low to high
    mutate(y = row$bottom + cumsum(height) - (0.5 * height))
}
