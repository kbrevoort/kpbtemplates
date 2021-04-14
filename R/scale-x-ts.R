scale_x_ts <- function(freq = 'annual',
                       name = waiver(),
                       limits = NULL,
                       expand = waiver(),
                       guide = waiver(),
                       position = 'bottom') {

  freq <- match.arg(freq, c('annual', 'monthly', 'quarterly'))

  list(geom_ts_scale(),
       .scale_x_ts(freq = freq,
                   name = name,
                   limits = limits,
                   expand = expand,
                   guide = guide,
                   position = position))
}

.scale_x_ts <- function(freq,
                        name = waiver(),
                        limits = NULL,
                        expand = waiver(),
                        guide = waiver(),
                        position = 'bottom') {


  sc <- continuous_scale(aesthetics = c('x', 'xmin', 'xmax', 'xend'),  # was aesthetics
                         scale_name = 'date', # was name
                         name = name,
                         palette = identity,
                         breaks = waiver(),
                         minor_breaks = waiver(),
                         labels = waiver(),
                         guide = waiver(),
                         trans = 'date',
                         limits = limits,
                         expand = expand,
                         position = position,
                         super = ScaleContinuousQuarter)
  sc$set_freq(freq)
  sc
}

is_odd <- function(x) {
  as.integer(x) %% 2L == 1L
}

ScaleContinuousQuarter <- ggproto(`_class` = "ScaleContinuousQuarter",
                                  `_inherit` = ggplot2::ScaleContinuousDate,
                                  freq = 'annual',
                                  set_freq = function(self, x) {
                                    self$freq <- x
                                  },
                                  get_breaks = function(self, limits = self$get_limits()) {
                                    # This call should always use freq = 'annual'
                                    get_date_range(limits, use_mid = TRUE, freq = 'annual')
                                  },
                                  get_labels = function(self, breaks = self$get_breaks()) {
                                    if (is.null(breaks))
                                      return(NULL)

                                    my_years <- extract_year(breaks)
                                    ifelse(is_odd(my_years), '', as.character(my_years))
                                  }
)

draw_ts_scale <- function(data, panel_params, coord) {

  process_list <- c('annual', 'quarterly', 'monthly')
  freq <- match.arg(panel_params$x$scale$freq, process_list)

  # Create date range as a date variable
  date_range <- as.Date(panel_params$x.range,
                        origin = as.Date('1970-01-01'))

  to_process <- process_list[1:which(process_list == freq)]
  temp <- lapply(to_process,
                 compute_quarterly_axis_data,
                 dr = date_range,
                 yr = panel_params$y.range)
  df <- do.call(rbind, temp)

  coords <- coord$transform(df, panel_params)

  grid::gTree(children = grid::gList(
    make_single_polyline(filter(coords, .freq == 'annual'), 'black', 1L),
    if ('quarterly' %in% to_process) make_single_polyline(filter(coords, .freq == 'quarterly'), 'gray', 1L) else NULL,
    if ('monthly' %in% to_process) make_single_polyline(filter(coords, .freq == 'monthly'), 'gray', 1L) else NULL))
}

make_single_polyline <- function(dt, color, width) {
  if (length(dt$x) == 0)
    return(NULL)

  grid::polylineGrob(x = dt$x,
                     y = dt$alt_y,
                     id = dt$ids,
                     default.units = 'npc',
                     arrow = NULL,
                     gp = grid::gpar(col = color, lwd = width))
}

extract_year <- function(x) {
  1900 + as.POSIXlt(x)$year
}

extract_month <- function(x) {
  1 + as.POSIXlt(x)$mon
}

compute_quarterly_axis_data <- function(freq, dr, yr) {

  golden_ratio <- (1 + sqrt(5)) / 2

  freq = match.arg(freq, c('annual', 'quarterly', 'monthly'))
  if (freq == 'quarterly') {
    this_by <- '3 months'
    drop_list <- 1L
    leng = 4 / golden_ratio / 100
  } else if (freq == 'monthly') {
    this_by <- '1 month'
    drop_list <- c(1L, 4L, 7L, 10L)
    leng <- 4 / golden_ratio / golden_ratio / 100
  } else {
    this_by <- '1 year'
    drop_list <- c(2L:12L)
    leng <- 4 / 100
  }

  py <- seq(from = as.Date(sprintf('%d-01-01', extract_year(dr[1L]))),
            to = as.Date(sprintf('%d-01-01', extract_year(dr[2L]) + 1L)),
            by = this_by)
  py <- py[py >= dr[1L] &
             py <= dr[2L] &
             !extract_month(py) %in% drop_list]

  data.frame(x = rep(as.numeric(py), each = 2L),
             alt_y = rep(c(0, leng), times = length(py)),
             ids = rep(seq_along(py), each = 2L),
             PANEL = 1,
             group = -1,
             shape = 19,
             .freq = freq)
}

get_date_range <- function(x, use_mid = FALSE, freq = 'annual') {

  date_range <- as.Date(x, origin = as.Date('1970-01-01'))
  freq <- match.arg(freq, c('annual', 'quarterly', 'monthly'))
  if (freq == 'annual') {
    this_by <- '1 year'
  } else if (freq == 'quarterly') {
    this_by <- '3 months'
  } else this_by <- '1 month'

  py <- seq(from = as.Date(sprintf('%d-%02d-01',
                                   lubridate::year(date_range[1L]),
                                   if (use_mid) 7L else 1L)),
            to = as.Date(sprintf('%d-%02d-01',
                                 lubridate::year(date_range[2L]),
                                 if (use_mid) 7L else 1L)),
            by = this_by)

  py[py >= date_range[1L] & py <= date_range[2L]]
}

GeomTSScale <- ggproto("GeomTSScale",
                       Geom,
                       required_aes = c("x", "y"),
                       default_aes = aes(shape = 19, colour = "black"),
                       draw_panel = draw_ts_scale)

geom_ts_scale <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          inherit.aes = TRUE, ...) {

  layer(geom = GeomTSScale,
        mapping = mapping,
        data = data,
        stat = stat,
        position = position,
        show.legend = FALSE,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...))
}
