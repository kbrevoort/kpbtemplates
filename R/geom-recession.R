stat_recession <- function(data = NULL,
                           mapping = NULL,
                           position = 'identity',
                           show.legend = FALSE,
                           ...,
                           inherit.aes = TRUE) {

  #browser()
  ggplot2::layer(data = data,
                 mapping = mapping,
                 stat = ggplot2::ggproto('StatRecession',
                                         ggplot2::Stat,
                                         compute_layer = compute_recession,
                                         required_aes = c('x')),
                 geom = 'rect',
                 position = position,
                 show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 check.aes = FALSE,
                 check.param = FALSE,
                 params = list(...))
}

compute_recession <- function (self, data, params, layout) {

  my_path <- find.package('kpbtemplates')
  temp <- file.path(my_path, 'inst')
  if (dir.exists(temp))
    my_path <- temp

  nber_dt <- read.csv(file = file.path(my_path, 'data/nber-dates.csv')) %>%
    mutate(peak = as.Date(peak),
           trough = as.Date(trough),
           trough = ifelse(trough > as.Date('2500-01-01'),
                           Sys.Date(),
                           trough))

  max_x <- as.Date(max(data$x, na.rm = TRUE), origin = '1970-01-01')
  min_x <- as.Date(min(data$x, na.rm = TRUE), origin = '1970-01-01')

  nber_dt %>%
    dplyr::filter(peak < max_x, trough > min_x) %>%
    dplyr::mutate(xmin = as.numeric(peak),
                  xmax = as.numeric(trough),
                  ymin = -Inf,
                  ymax = Inf,
                  PANEL = data$PANEL[1L]) %>%
    dplyr::select(-peak, -trough)
}

#' NBER Recession Date Geom
#'
#' Adds shaded areas to a time series that indicate the periods corresponding
#' to recessions as dated by the National Bureau of Economic Research.
#' @export
geom_recession <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = FALSE, hjust = 0,
                           size = 10, inherit.aes = TRUE, nudge_y = 0,
                           nudge_x = 0, alpha = 0.1,
                           fill = 'gray90', ...) {

  layer(geom = GeomRect,
        mapping = mapping,
        data = data,
        stat = ggplot2::ggproto('StatRecession',
                                ggplot2::Stat,
                                compute_layer = compute_recession,
                                required_aes = c('x'),
                                nudge_x = nudge_x,
                                nudge_y = nudge_y),
        position = position,
        show.legend = FALSE,
        inherit.aes = inherit.aes,
        check.param = FALSE,
        params = list(na.rm = na.rm,
                      hjust = hjust,
                      size = size,
                      fill = fill,
                      alpha = alpha,
                      ...))
}


