#' Presentation Theme
#'
#' This is a ggplot2 theme that I have prepared to format graphs in my
#' preferred style for presentations, which uses larger fonts than would
#' be appropriate in a printed graph.
#' @importFrom ggplot2 theme theme_minimal margin element_text element_line element_blank
#' @export
theme_presentation <- function(legend.position = 'none') {

  t <- theme(
    plot.margin = margin(0, 0, 0, 0),
    plot.title.position = 'plot',
    plot.title = element_text(size = 18L,
                              lineheight = 1,
                              hjust = 0,
                              vjust = 0.5,
                              family = 'sans',
                              face = 'bold',
                              margin = margin(b = 10, l = 0, t = 10, r = 0)),
    plot.subtitle = element_text(size = 14L,
                                 lineheight = 1,
                                 hjust = 0,
                                 vjust = 0.5,
                                 family = 'sans',
                                 face = 'plain',
                                 margin = margin(b = 10, l = 0, t = 0, r = 0)),
    axis.title.y = element_text(family = 'sans',
                                face = 'italic',
                                angle = 90,
                                size = 12L,
                                hjust = 0.5,
                                vjust = 0.5,
                                margin = margin(b = 5, l = 0, t = 0, r = 0)),
    axis.title.x = element_text(family = 'sans',
                                face = 'italic',
                                angle = 0,
                                size = 12L,
                                hjust = 0.5,
                                vjust = 0.5,
                                margin = margin(b = 10, l = 0, t = 10, r = 0)),
    axis.text.y = element_text(family = 'sans',
                               size = 12L,
                               hjust = 1,
                               vjust = 0.5,
                               face = 'plain'),
    axis.text.x = element_text(family = 'sans',
                               size = 12L,
                               hjust = 0.5,
                               vjust = 0.5,
                               face = 'plain'),
    axis.line.x = element_line(color = 'black',
                               size = 1L,
                               linetype = 'solid'),
    axis.ticks = element_blank(),
    plot.tag.position = c(1, 1),
    plot.tag = element_text(family = 'serif',
                            face = 'bold',
                            size = 14L,
                            lineheight = 2,
                            hjust = 1,
                            vjust = 0.5,
                            margin = margin(b = 5, t = 5, r = 0, l = 0)),
    plot.caption.position = 'plot',
    plot.caption = element_text(family = 'serif',
                                face = 'plain',
                                size = 11L,
                                lineheight = 1.3,
                                hjust = 0,
                                vjust = 0.5,
                                margin = margin(b = 10, l = 0, t = 10, r = 0)),
    legend.position = legend.position
  )

  theme_minimal() %+replace% t
}

#' @export
theme_timeseries <- function(legend.position = 'none') {

  t <- theme(
    # Switch the major and minor grid lines so that the heavier weight lines
    # correspond to the start and end of each year
    panel.grid.major.x = element_line(color = 'grey92',
                                      size = rel(0.5)),
    panel.grid.minor.x = element_line(color = 'grey92',
                                      size = rel(1L)),
    panel.grid.minor.y = element_blank()
  )

  theme_presentation(legend.position = legend.position) %+replace% t
}
