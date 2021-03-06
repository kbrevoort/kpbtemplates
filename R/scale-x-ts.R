#' Position scales for time series data
#'
#' Alters the appearance of the x-axis when plotting time series data.
#' Can be used with annual, quarterly, and monthly frequency data. For
#' annual data, tick marks will be drawn at the beginning and ending of
#' each year with the label for the year centered below that interval.
#' When used with quarterly data, smaller tick marks are added to show
#' end of each quarter.  The option for monthly data includes smaller
#' tick marks for the first and second months of each quarter.
#' @param freq Character scalar that determines the type of x-axis to
#' be displayed.  Options are 'annual' (the default), 'quarterly', and
#' 'monthly'.
#' @param show_years Character scaler indicating which years to display
#' on the x-axis as labels.  Options include:  'all', 'odd', and 'even'.
#' @param name Character vector specifying the name of the axis to be
#' displayed below the axis
#' @param limits Date vector with two elements giving the minimum and
#' maximum values to be shown on the x-axis.
#' @param tick_color Vector of colors for the tick marks.  Up to three
#' values can be provided.  The first is the color of the annual ticks,
#' the second is the color of the quartly ticks and the thrid is the
#' monthly tick color.  If only one color is provided, all the tick
#' marks will have the same color.  Default value is c('black', 'gray',
#' 'gray').
#' @param tick_length A scalar number determining the length of the annual
#' tick. Default value is 4L, which means the tick takes up 4 percent of
#' the lenght of the y-axis (the unit used is always 'npc').
#' @importFrom ggplot2 ggproto
#' @export
scale_x_ts <- function(freq = 'annual',
                       show_years = 'even',
                       name = waiver(),
                       limits = NULL,
                       expand = waiver(),
                       guide = waiver(),
                       tick_width = 1L,
                       tick_color = c('black', 'gray', 'gray'),
                       tick_length = 4L,
                       ...) {

  freq <- match.arg(freq, c('annual', 'monthly', 'quarterly'))
  show_years <- match.arg(show_years, c('all', 'odd', 'even'))

  if (length(tick_color) == 1L) {
    tick_color <- rep(tick_color, 3L)
  } else if (length(tick_color) == 2L) {
    tick_color <- c(tick_color, tick_color[2L])
  }

  if (length(tick_width) == 1L) {
    tick_width <- rep(tick_width, 3L)
  } else if (length(tick_width) == 2L) {
    tick_width <- c(tick_width, tick_width[2L])
  }

  if (length(tick_length) == 1L) {
    golden_ratio <- (1 + sqrt(5)) / 2
    tick_length <- c(tick_length,
                     tick_length / golden_ratio,
                     tick_length / golden_ratio / golden_ratio)
  } else if (length(tick_length) == 2L) {
    tick_length <- c(tick_length, tick_length[2L] / golden_ratio)
  }

  list(geom_ts_scale(...),
       .scale_x_ts(freq = freq,
                   show_years = show_years,
                   name = name,
                   limits = limits,
                   expand = expand,
                   guide = guide,
                   position = 'bottom',
                   tick_width = tick_width,
                   tick_colors = tick_color,
                   tick_length = tick_length))
}

.scale_x_ts <- function(freq,
                        show_years,
                        name = waiver(),
                        limits = NULL,
                        expand = waiver(),
                        guide = waiver(),
                        position = 'bottom',
                        tick_width = rep(1L, times = 3L),
                        tick_colors = c('black', 'gray', 'gray'),
                        tick_length = rep(4L, times = 3L)) {

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
  sc$set_params(colors = tick_colors,
                length = tick_length,
                freq = freq,
                show_years = show_years,
                width = tick_width)
  #sc$set_colors(tick_colors)
  #sc$set_length(tick_length)
  #sc$set_freq(freq)
  #sc$set_sy(show_years)
  #sc$set_width(tick_width)
  sc
}

is_odd <- function(x) {
  as.integer(x) %% 2L == 1L
}

ScaleContinuousQuarter <- ggplot2::ggproto(`_class` = "ScaleContinuousQuarter",
                                  `_inherit` = ggplot2::ScaleContinuousDate,
                                  freq = 'annual',
                                  show_years = 'even',
                                  tick_colors = c('black', 'gray', 'gray'),
                                  tick_length = 1L,
                                  tick_width = rep(1L, times = 3L),
                                  set_params = function(self, colors, length, show_years, freq, width) {
                                    self$tick_colors <- colors
                                    self$tick_length <- length
                                    self$show_years <- show_years
                                    self$freq <- freq
                                    self$tick_width <- width
                                  },
                                  #set_colors = function(self, x) self$tick_colors <- x,
                                  #set_length = function(self, x) self$tick_length <- x,
                                  #set_sy = function(self, x) self$show_years <- x,
                                  #set_freq = function(self, x) self$freq <- x,
                                  get_breaks = function(self, limits = self$get_limits()) {
                                    end_points <- as.Date(limits, origin = '1970-01-01')
                                    # This call should always use freq = 'annual'
                                    breaks <- get_date_range(limits, use_mid = TRUE, freq = 'annual')
                                    breaks <- breaks[range2date(breaks) >= end_points[1L]]
                                    breaks <- breaks[as.numeric(range2date(breaks)) + 365L < limits[2L]]
                                    breaks
                                  },
                                  get_labels = function(self, breaks = self$get_breaks()) {
                                    if (is.null(breaks))
                                      return(NULL)

                                    my_years <- extract_year(breaks)
                                    if (self$show_years == 'odd') {
                                      ret_val <- ifelse(is_odd(my_years), as.character(my_years), '')
                                    } else if (self$show_years == 'even') {
                                      ret_val <- ifelse(is_odd(my_years), '', as.character(my_years))
                                    } else ret_val <- my_years
                                    ret_val
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
                 yr = panel_params$y.range,
                 leng = panel_params$x$scale$tick_length)
  df <- do.call(rbind, temp)

  coords <- coord$transform(df, panel_params)

  grid::gTree(children = grid::gList(
    make_single_polyline(subset(coords, .freq == 'annual'),
                         color = panel_params$x$scale$tick_colors[1L],
                         length = panel_params$x$scale$tick_length[1L],
                         width = panel_params$x$scale$tick_width[1L]),
    if ('quarterly' %in% to_process) make_single_polyline(subset(coords, .freq == 'quarterly'),
                                                          color = panel_params$x$scale$tick_colors[2L],
                                                          length = panel_params$x$scale$tick_length[2L],
                                                          width = panel_params$x$scale$tick_width[2L]) else NULL,
    if ('monthly' %in% to_process) make_single_polyline(subset(coords, .freq == 'monthly'),
                                                        color = panel_params$x$scale$tick_colors[3L],
                                                        length = panel_params$x$scale$tick_length[3L],
                                                        width = panel_params$x$scale$tick_width[3L]) else NULL))
}

make_single_polyline <- function(dt, color, length, width = 1L) {
  if (length(dt$x) == 0)
    return(NULL)

  grid::polylineGrob(x = dt$x,
                     y = dt$alt_y, #dt$alt_y * length,
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

compute_quarterly_axis_data <- function(freq, dr, yr, leng) {

  golden_ratio <- (1 + sqrt(5)) / 2

  freq = match.arg(freq, c('annual', 'quarterly', 'monthly'))
  if (freq == 'quarterly') {
    drop_list <- 1L
    leng = leng[2L] / 100#0.01 / golden_ratio
  } else if (freq == 'monthly') {
    drop_list <- c(1L, 4L, 7L, 10L)
    leng <- leng[3L] / 100 #0.01 / golden_ratio / golden_ratio
  } else {
    drop_list <- c(2L:12L)
    leng <- leng[1L] / 100 #0.01
  }

  py <- seq(from = as.Date(sprintf('%d-01-01', extract_year(dr[1L]))),
            to = as.Date(sprintf('%d-01-01', extract_year(dr[2L]) + 1L)),
            by = get_by_string(freq))
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

  py <- seq(from = range2date(date_range[1L], use_mid),
            to = range2date(date_range[2L], use_mid),
            by = get_by_string(freq))

  py[py >= date_range[1L] & py <= date_range[2L]]
}

range2date <- function(r, mid = FALSE) {
  as.Date(sprintf('%d-%02d-01',
                  extract_year(r),
                  if (mid) 7L else 1L))
}

get_by_string <- function(x) {
  switch(x,
         annual = '1 year',
         quarterly = '3 months',
         monthly = '1 month')
}

GeomTSScale <- ggplot2::ggproto("GeomTSScale",
                                ggplot2::Geom,
                                required_aes = c("x", "y"),
                                default_aes = ggplot2::aes(colour = "black"),
                                draw_panel = draw_ts_scale)

geom_ts_scale <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          #tick_color = c('black', 'gray', 'gray'),
                          #tick_length = 4L,
                          inherit.aes = TRUE, ...) {

  layer(geom = GeomTSScale,
        mapping = mapping,
        data = data,
        stat = stat,
        position = position,
        show.legend = FALSE,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm,
                      #tick_color = tick_color,
                      #tick_length = tick_length,
                      ...))
}

#' @export
ctrq <- function(x) {
  temp <- as.POSIXlt(x)
  temp$mday <- ifelse(temp$mon + 1L == 2L, 14L, 15L)
  temp$mon <- (floor(temp$mon / 3) * 3) + 1L
  as.Date(temp)
}

#' @export
ctrm <- function(x) {
  temp <- as.POSIXlt(x)
  temp$mday <- ifelse(temp$mon + 1L == 2L, 14L, 15L)
  as.Date(temp)
}
