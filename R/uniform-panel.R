stat_uniformpanel <- function(data = NULL,
                              mapping = NULL,
                              position = 'identity',
                              show.legend = FALSE,
                              ...,
                              inherit.aes = TRUE) {

  #browser()
  ggplot2::layer(data = data,
                 mapping = mapping,
                 stat = ggplot2::ggproto('StatUniformPanel',
                                         ggplot2::Stat,
                                         compute_layer = compute_uniformpanel,
                                         required_aes = c('x', 'y')),
                 geom = 'blank',
                 position = position,
                 show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 check.aes = FALSE,
                 check.param = FALSE,
                 params = list(...))
}

.compute_midpoint <- function(x) {
  sorted <- sort(unique(x))
  sorted[floor(length(sorted) / 2)]
}

compute_uniformpanel <- function (self, data, params, layout) {

  data %>%
    group_by(group, PANEL) %>%
    summarize(x = .compute_midpoint(x),
              miny = min(y, na.rm = TRUE),
              maxy = max(y, na.rm = TRUE),
              .groups = 'drop') %>%
    mutate(range = maxy - miny) %>%
    ungroup() %>%
    mutate(maxrange = max(range, na.rm = TRUE),
           expansion = (maxrange - range) / 2,
           miny = miny - expansion,
           maxy = maxy + expansion) %>%
    select(x, group, PANEL, miny, maxy) %>%
    tidyr::pivot_longer(cols = c(4:5),
                        values_to = 'y') %>%
    select(x, y, group, PANEL)
}

#' NBER Recession Date Geom
#'
#' Adds shaded areas to a time series that indicate the periods corresponding
#' to recessions as dated by the National Bureau of Economic Research.
#' @export
geom_uniformpanel <- function(mapping = NULL, data = NULL,
                              position = "identity",
                              size = 0.1, inherit.aes = TRUE, nudge_y = 0,
                              nudge_x = 0, ...) {

  layer(geom = GeomBlank,
        mapping = mapping,
        data = data,
        stat = ggplot2::ggproto('StatUniformPanel',
                                ggplot2::Stat,
                                compute_layer = compute_uniformpanel,
                                required_aes = c('x', 'y'),
                                nudge_x = nudge_x,
                                nudge_y = nudge_y),
        position = position,
        show.legend = FALSE,
        inherit.aes = inherit.aes,
        check.param = FALSE,
        params = list(size = 0,
                      color = NA,
                      ...))
}


