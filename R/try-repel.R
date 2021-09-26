geom_text_repel2 <- function(mapping = NULL,
                             data = NULL,
                             stat = "identity",
                             position = "identity",
                             parse = FALSE,
                             ...,
                             box.padding = 0.25,
                             point.padding = 1e-6,
                             min.segment.length = 0.5,
                             arrow = NULL,
                             force = 1,
                             force_pull = 1,
                             max.time = 0.5,
                             max.iter = 10000,
                             max.overlaps = getOption("ggrepel.max.overlaps",
                                                      default = 10),
                             nudge_x = 0,
                             nudge_y = 0,
                             xlim = c(NA, NA),
                             ylim = c(NA, NA),
                             na.rm = FALSE,
                             show.legend = NA,
                             direction = c("both","y","x"),
                             seed = NA,
                             verbose = FALSE,
                             inherit.aes = TRUE) {

  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }
    position <- position_nudge_repel(nudge_x, nudge_y)
  }

  layer(data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomTextRepel,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(parse = parse,
                      na.rm = na.rm,
                      box.padding = to_unit(box.padding),
                      point.padding = to_unit(point.padding),
                      min.segment.length = to_unit(min.segment.length),
                      arrow = arrow,
                      force = force,
                      force_pull = force_pull,
                      max.time = max.time,
                      max.iter = max.iter,
                      max.overlaps = max.overlaps,
                      nudge_x = nudge_x,
                      nudge_y = nudge_y,
                      xlim = xlim,
                      ylim = ylim,
                      direction = match.arg(direction),
                      seed = seed,
                      verbose = verbose,
                      ...))
}


#' grid::makeContent function for the grobTree of textRepelGrob objects
#' @param x A grid grobTree.
#' @export
#' @noRd
makeContent.textrepeltree2 <- function(x) {

  # The padding around each point.
  if (is.na(x$point.padding)) {
    x$point.padding = unit(0, "lines")
  }

  # Do not create text labels for empty strings.
  valid_strings <- which(not_empty(x$lab))
  invalid_strings <- which(!not_empty(x$lab))
  ix <- c(valid_strings, invalid_strings)
  x$data <- x$data[ix,]
  x$lab <- x$lab[ix]

  # Create a dataframe with x1 y1 x2 y2
  cat('Creating new boxes::\n\n')
  boxes <- lapply(seq_along(valid_strings), create_boxes, obj = x)

  # Make the repulsion reproducible if desired.
  if (is.null(x$seed) || !is.na(x$seed)) {
    set.seed(x$seed)
  }

  # The points are represented by circles.
  x$data$point.size[is.na(x$data$point.size)] <- 0

  # Beware the magic numbers. I do not understand them.
  # I just accept them as necessary to get the code to work.
  p_width <- convertWidth(unit(1, "npc"), "inch", TRUE)
  p_height <- convertHeight(unit(1, "npc"), "inch", TRUE)
  p_ratio <- (p_width / p_height)
  if (p_ratio > 1) {
    p_ratio <- p_ratio ^ (1 / (1.15 * p_ratio))
  }
  point_size <- p_ratio * convertWidth(
    to_unit(x$data$point.size), "native", valueOnly = TRUE
  ) / 13
  point_padding <- p_ratio * convertWidth(
    to_unit(x$point.padding), "native", valueOnly = TRUE
  ) / 13

  # Repel overlapping bounding boxes away from each other.
  repel <- ggrepel:::repel_boxes2(
    data_points     = as.matrix(x$data[,c("x","y")]),
    point_size      = point_size,
    point_padding_x = point_padding,
    point_padding_y = point_padding,
    boxes           = do.call(rbind, boxes),
    xlim            = range(x$limits$x),
    ylim            = range(x$limits$y),
    hjust           = rep.int(0, times = length(x$data$hjust)),  #x$data$hjust %||% 0.5,
    vjust           = x$data$vjust %||% 0.5,
    force_push      = x$force * 1e-6,
    force_pull      = x$force_pull * 1e-2,
    max_time        = x$max.time,
    max_iter        = ifelse(is.infinite(x$max.iter), 1e9, x$max.iter),
    max_overlaps    = x$max.overlaps,
    direction       = x$direction,
    verbose         = x$verbose
  )

  # THIS SHOULD BE REMOVED
  repel$x <- max(repel$x, na.rm = TRUE)

  if (any(repel$too_many_overlaps)) {
    warn(
      sprintf(
        "ggrepel: %s unlabeled data points (too many overlaps). Consider increasing max.overlaps",
        sum(repel$too_many_overlaps)
      )
    )
  }

  if (all(repel$too_many_overlaps)) {
    grobs <- list()
    class(grobs) <- "gList"
    return(setChildren(x, grobs))
  }

  grobs <- lapply(seq_along(valid_strings),
                  create_text_repel_grobs,
                  repel = repel,
                  obj = x,
                  ps = point_size)

  grobs <- unlist(grobs, recursive = FALSE)
  class(grobs) <- "gList"

  # # Put segment grobs before text grobs.
  # grob_names <- sapply(grobs, "[[", "name")
  # grobs <- grobs[order(!grepl("^segment", grob_names))]

  setChildren(x, grobs)
}


as_unit <- function(x, default.units = 'npc') {
  if (!grid::is.unit(x)) x <- grid::unit(x, default.units)
  x
}

# copied from ggplot2
compute_just <- function(just, x) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]

  unname(c(left = 0, center = 0.5, right = 1,
           bottom = 0, middle = 0.5, top = 1)[just])
}

# copied from ggplot2
just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}

#' Return a unit version of the argument.
#'
#' @param x Number or unit object.
#' @return unit(x, "lines") if number or the unchanged argument if it's already
#'  a unit object.
#' @noRd
to_unit <- function(x) {
  # don't change arg if already unit
  if (is.unit(x)) {
    return(x)
  }

  # NA used to exclude points from repulsion calculations
  if (length(x) == 1 && is.na(x)) {
    return(NA)
  }

  unit(x, "lines")
}

#is.unit <- grid::is.unit
#grobName <- grid::grobName

#' Return a boolean vector of non-empty items.
#'
#' @param xs Vector with a mix of "expression" items, "character" items,
#'  and items from other classes.
#' @return Boolean vector indicating which items are not empty.
#' @noRd
not_empty <- function(xs) {
  sapply(seq_along(xs), function(i) {
    if (is.expression(xs[i])) {
      return(length(nchar(xs[i])) > 0)
    } else {
      return(xs[i] != "")
    }
  })
}

`%||%` <- rlang::`%||%`

create_boxes <- function(i, obj) {

  # The padding around each bounding box.
  box_padding_x <- convertWidth(obj$box.padding, "native", valueOnly = TRUE)
  box_padding_y <- convertHeight(obj$box.padding, "native", valueOnly = TRUE)

  row <- obj$data[i, , drop = FALSE]
  tg <- textGrob(label = obj$lab[i],
                 x = row$x,
                 y = row$y,
                 default.units = "native",
                 rot = row$angle,
                 hjust = 0, #row$hjust,
                 vjust = row$vjust,
                 gp = gpar(fontsize   = row$size * .pt,
                           fontfamily = row$family,
                           fontface   = row$fontface,
                           lineheight = row$lineheight))

  x1 <- convertWidth(grobX(tg, "west"), "native", TRUE)
  x2 <- convertWidth(grobX(tg, "east"), "native", TRUE)
  y1 <- convertHeight(grobY(tg, "south"), "native", TRUE)
  y2 <- convertHeight(grobY(tg, "north"), "native", TRUE)

  x2 <- 0.95 + (x2 - x1)
  x1 <- 0.95
  cat(sprintf('x1 = %f, x2 = %f\n', x1, x2))

  c(
    "x1" = x1 - box_padding_x + row$nudge_x,
    "y1" = y1 - box_padding_y + row$nudge_y,
    "x2" = x2 + box_padding_x + row$nudge_x,
    "y2" = y2 + box_padding_y + row$nudge_y
  )
}

create_text_repel_grobs <- function(i, repel, obj, ps) {
  if (repel$too_many_overlaps[i])
    return(NULL)

  row <- obj$data[i, , drop = FALSE]
  ps <- ps[i]

  grobs <- textGrob(label = obj$lab[i],
                    x = as_unit(repel$x[i]),
                    y = as_unit(repel$y[i]),
                    rot = row$angle,
                    hjust = 0,
                    vjust = row$vjust,
                    default.units = 'npc',
                    check.overlap = FALSE,
                    name = sprintf('textrepelgrob%s', i),
                    gp = gpar(col = scales::alpha(row$colour, row$alpha),
                              fontsize = row$size * .pt,
                              fontfamily = row$family,
                              fontface = row$fontface,
                              lineheight = row$lineheight),
                    vp = NULL)

  gList(grobs)
}

draw_panel_function <- function(data,
                                panel_scales,
                                coord,
                                parse = FALSE,
                                na.rm = FALSE,
                                box.padding = 0.25,
                                point.padding = 1e-6,
                                min.segment.length = 0.5,
                                arrow = NULL,
                                force = 1,
                                force_pull = 1,
                                max.time = 0.5,
                                max.iter = 10000,
                                max.overlaps = 10,
                                nudge_x = 0,
                                nudge_y = 0,
                                xlim = c(NA, NA),
                                ylim = c(NA, NA),
                                direction = "both",
                                seed = NA,
                                verbose = FALSE) {

  #browser()
  cat('In panel drawing function\n')

  lab <- data$label
  if (parse) lab <- parse_safe(as.character(lab))
  if (!length(which(not_empty(lab)))) return()

  # As a test without disrupting anything, rename columns to match old names
  for (this_dim in c("x", "y")) {
    this_orig <- sprintf("%s_orig", this_dim)
    this_nudge <- sprintf("nudge_%s", this_dim)
    data[[this_nudge]] <- data[[this_dim]]
    if (this_orig %in% colnames(data)) {
      data[[this_dim]] <- data[[this_orig]]
      data[[this_orig]] <- NULL
    }
  }

  # Transform the nudges to the panel scales.
  nudges <- data.frame(x = data$nudge_x, y = data$nudge_y)
  nudges <- coord$transform(nudges, panel_scales)

  # Transform the raw data to the panel scales.
  data <- coord$transform(data, panel_scales)

  # The nudge is relative to the data.
  data$nudge_x <- nudges$x - data$x
  data$nudge_y <- nudges$y - data$y

  # Transform limits to panel scales.
  limits <- data.frame(x = xlim, y = ylim)
  limits <- coord$transform(limits, panel_scales)

  # Allow Inf.
  if (length(limits$x) == length(xlim)) {
    limits$x[is.infinite(xlim)] <- xlim[is.infinite(xlim)]
  }
  if (length(limits$y) == length(ylim)) {
    limits$y[is.infinite(ylim)] <- ylim[is.infinite(ylim)]
  }

  # Fill NAs with defaults.
  limits$x[is.na(limits$x)] <- c(0, 1)[is.na(limits$x)]
  limits$y[is.na(limits$y)] <- c(0, 1)[is.na(limits$y)]

  # Convert hjust and vjust to numeric if character
  if (is.character(data$vjust)) {
    data$vjust <- compute_just(data$vjust, data$y)
  }
  if (is.character(data$hjust)) {
    data$hjust <- compute_just(data$hjust, data$x)
  }

  # This is a line I added
  data$hjust <- rep.int(0, times = length(data$hjust))

  cat('About to exit panel drawing function\n\n')
  #browser()
  grob <- gTree(limits = limits,
                data = data,
                lab = lab,
                box.padding = to_unit(box.padding),
                point.padding = to_unit(point.padding),
                min.segment.length = to_unit(min.segment.length),
                arrow = arrow,
                force = force,
                force_pull = force_pull,
                max.time = max.time,
                max.iter = max.iter,
                max.overlaps = max.overlaps,
                direction = direction,
                seed = seed,
                verbose = verbose,
                cl = "textrepeltree2")
  grob$name <- grid::grobName(grob, 'geom_text_repel2')
  grob
}

#' GeomTextRepel
#' @rdname ggrepel-ggproto
#' @format NULL
#' @usage NULL
#' @seealso \link[ggplot2]{GeomText} from the ggplot2 package.
#' @export
GeomTextRepel <- ggproto("GeomTextRepel",
                         ggplot2::Geom,
                         required_aes = c("x", "y", "label"),
                         default_aes = aes(colour = "black",
                                           size = 3.88,
                                           angle = 0,
                                           alpha = NA,
                                           family = "",
                                           fontface = 1,
                                           lineheight = 1.2,
                                           hjust = 0, # 0.5,
                                           vjust = 0.5,
                                           point.size = 1,
                                           bg.colour = NA,
                                           bg.r = 0.1),
                         draw_panel = draw_panel_function,
                         draw_key = draw_key_text)

