#' Params to Graphical Parameters
#'
#' Converts the elements of a elemnt_text specification from ggplot2 into the
#' graphical parameter vector that is needed for grid graphics.
#' @param p Parameter vector from a ggplot object, as specified in the plot's
#' theme.
#' @param bs The base size of the fonts in the ggplot plot. This is necessary
#' for cases where the font size in the parameter vector is expressed as a
#' relative size.
#' @importFrom grid gpar
params2gp <- function(p, bs) {
  grid::gpar(col = p$colour,
             fill = 'white',
             alpha = 1,
             lty = 'solid',
             lwd = 1,
             lex = 1,
             lineend = 'round',
             linejoin = 'round',
             linemitre = 10,
             fontsize = if (class(p$size) == 'rel') bs * as.numeric(p$size) else p$size,
             cex = 1,
             fontfamily = p$family,
             fontface = p$face,
             lineheight = p$lineheight)
}

#' Build Label Grob
#'
#' Extracts the necessary information from a ggplot object to convert one of the
#' labels (title, subtitle, caption, or tag) into a textbox_grob.
#' @param plot A ggplot graph
#' @param label_type Character scalar giving the label to convert to a grob.
#' @param w The width of the plot expressed as a grid unit. Default is `unit(6.5, 'in')`.
#' @return A textbox_grob from the gridtext package.
#' @importFrom ggplot2 is.ggplot
#' @importFrom glue glue
#' @importFrom gridtext textbox_grob
build_label_grob <- function(label_type, plot, w = grid::unit(6.5, 'in')) {
  if (!is.ggplot(plot))
    stop('build_label_grob only works with ggplot objects.')

  if (!label_type %in% names(plot$labels))
    return(invisible(NULL))

  if (label_type == 'y')
    return(build_y_grob(plot, w))

  params <- plot$theme[[glue::glue("plot.{label_type}")]]
  out_grob <- gridtext::textbox_grob(text = plot$labels[[label_type]],
                                     halign = params$hjust,
                                     hjust = params$hjust,
                                     vjust = params$vjust,
                                     width = w,
                                     padding = if(is.null(params$margin)) margin(0, 0, 0, 0) else params$margin,
                                     gp = params2gp(params, plot$theme$text$size))
}

#' @importFrom gridtext textbox_grob
build_y_grob <- function(plot, w) {
  y_text <- plot$labels$y
  if (length(plot$scales$scales) > 0) {   # Are there scales
    for (i in 1:length(plot$scales$scales)) {  # If so, loop through them
      if ('y' %in% plot$scales$scales[[i]]$aesthetics)  # Is there a y-scale
        if (class(plot$scales$scales[[i]]$name) == 'character')  # Does it have an assigned name
          y_text <- plot$scales$scales[[i]]$name
    }
  }

  if (y_text == '')
    return(NULL)

  params <- plot$theme$axis.title.y
  grob_y <- gridtext::textbox_grob(y_text,
                                   halign = 0,
                                   hjust = params$hjust,
                                   vjust = params$vjust,
                                   width = grid::unit(6.5, 'in'),
                                   padding = if (is.null(params$margin)) margin(0, 0, 0, 0) else params$margin,
                                   gp = params2gp(params,
                                                  plot$theme$text$size))
}

#' @importFrom ggplot2 theme element_blank
clean_plot_object <- function(plot, gbl) {
  for (i in names(gbl))
    if (i %in% names(plot$labels))
      plot$labels[i] <- NULL

  plot + theme(axis.title.y = element_blank())
}

#' @importFrom grid grobHeight convertHeight
calculate_grob_heights <- function(grob) {
  if (is.ggplot(grob))
    return(grid::unit(1, 'null'))

  grid::grobHeight(grob) %>%
    grid::convertHeight('in', valueOnly = TRUE)
}


#' @importFrom gridExtra arrangeGrob
format_graph <- function(plot, w) {

  all_labels <- c('tag', 'title', 'subtitle', 'y', 'graph', 'caption')
  grob_list <- lapply(all_labels,
                      build_label_grob,
                      plot = plot,
                      w = w)
  names(grob_list) <- all_labels

  plot <- clean_plot_object(plot, grob_list)
  grob_list$graph <- plot

  # Remove NULL elements
  null_index <- vapply(grob_list, is.null, TRUE)
  grob_list <- grob_list[!null_index]

  grob_table <- gridExtra::arrangeGrob(grobs = grob_list,
                                       heights = grid::unit(vapply(grob_list, calculate_grob_heights, 4.3),
                                                      ifelse(names(grob_list) == 'graph', 'null', 'in')),
                                       widths = w,
                                       ncol = 1L)
  grob_table
}

#' Save KPB Graph
#'
#' Converts a ggplot2 graphic into an output format that files the style
#' I like to use.
#' @param filename Character vector giving the output file path
#' @param plot Ggplot graph to save.  Default's to the last plot.
#' @param device A graphical device to use to print to.  Default is
#' NULL in which case the device will be assumed based on the extension
#' given the output file.
#' @param width Numerical scalar giving the width of the output. Default is
#' 6.5.
#' @param units Character scalar giving the units in which the width is
#' provided.  Default equals 'in'.
#' @importFrom grid grid.newpage grid.draw
#' @export
save_kpb_graph <- function(filename, plot = last_plot(), device = NULL, width = 6.5, units = 'in') {

  out_graph <- format_graph(plot, w = grid::unit(width, units))

  # Open a graphics device
  if (!is.null(device)) {
    device(filename)
  } else {
    dev_type <- tolower(tools::file_ext(filename))
    if (dev_type == 'png') {
      png(filename = filename)
    } else stop('Device unsupported in save_kpb_graph.')
  }

  on.exit(dev.off())

  grid::grid.newpage()
  grid::grid.draw(out_graph)
}

print_graph <- function(plot = last_plot(), width = 6.5, units = 'in') {
  out_graph <- format_graph(plot, w = grid::unit(width, units))

  grid::grid.newpage()
  grid::grid.draw(out_graph)

  invisible(NULL)
}

