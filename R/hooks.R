#' @export
caption_hook <- function(x, options) {
  resize1 <- resize2 <- ''
  if (is.null(options$resize.command)) {
    if (!is.null(options$resize.width) || !is.null(options$reside.height)) {
      resize1 = sprintf('\\resizebox{%s}{%s}{',
                        options$resize.width %n% '!',
                        options$resize.height %n% '!')
      resize2 = '} '
    }
  } else {
    # users can specify a custom "resize" command (we can use an arbitrary
    # command, e.g., framebox)
    resize1 = paste0('\\', rc, '{')
    resize2 = '} '
  }

  a <- options$fig.align
  fig.cur <- options$fig.cur %n% 1L
  fig.num <- options$fig.num %n% 1L
  animate <- options$fig.show == 'animate'
  fig.ncol <- options$fig.ncol %n% fig.num
  if (is.null(fig.sep <- options$fig.sep)) {
    fig.sep <- character(fig.num)
    if (fig.ncol < fig.num) fig.sep[seq(fig.ncol, fig.num - 1L, fig.ncol)] <- '\\newline'
  }
  sep.cur <- NULL

  usesub = length(subcap <- options$fig.subcap) && fig.num > 1
  # multiple plots: begin at 1, end at fig.num
  ai = options$fig.show != 'hold'

  # TRUE if this picture is standalone or first in set
  plot1 = ai || fig.cur <= 1L
  # TRUE if this picture is standalone or last in set
  plot2 = ai || fig.cur == fig.num

  # open align code if this picture is standalone/first in set
  align1 <- if (plot1)
    switch(a,
           left = '\n\n',
           center = '\n\n{\\centering ',
           right = '\n\n\\hfill{}',
           '\n')
  # close align code if this picture is standalone/last in set
  align2 <- if (plot2)
    switch(a,
           left = '\\hfill{}\n\n',
           center = '\n\n}\n\n',
           right = '\n\n',
           '')

  # figure environment: caption, short caption, label
  cap <- options$fig.cap
  scap <- options$fig.scap
  fig1 <- fig2 <- ''
  cap1 <- ''
  mcap <- fig.num > 1L && options$fig.show == 'asis' && !length(subcap)
  # initialize subfloat strings
  sub1 <- sub2 <- ''

  # Wrap in figure environment only if user specifies a caption
  if (length(cap) && !is.na(cap)) {
    # If pic is standalone/first in set: open figure environment
    if (plot1) {
      pos <- options$fig.pos
      if (pos != '' && !grepl('^[[{]', pos))
        pos <- sprintf('[%s]', pos)
      fig1 <- sprintf('\\begin{%s}%s',
                      options$fig.env, pos)
    }

    cap1 <- create_caption(options)
    if (cap1 != '')
      cap1 <- sprintf('%s%s\n%s',
                      cap1,
                      knitr:::create_label(paste0(options$fig.lp, options$label),
                                           if (mcap) c('-', fig.cur),
                                           latex = TRUE),
                      create_note(options))

    # Add subfloat code if needed
    if (usesub) {
      sub1 <- sprintf('\\subfloat[%s%s]{',
                      subcap,
                      knitr:::create_label(lab,
                                           '-',
                                           fig.cur,
                                           latex = TRUE))
      sub2 <- '}'
      sep.cur <- fig.sep[fig.cur]
      if (is.na(sep.cur)) sep.cur <- NULL
    }


    # If pic is standalone/last in set:
    # * place caption with label
    # * close figure environment
    if (plot2)
      fig2 <- sprintf('\\end{%s}\n', options$fig.env)

  } else if (pandoc_to(c('latex', 'beamer'))) {
    # use alignment environments for R Markdown latex output (\centering won't work)
    align.env <- switch(a,
                        left = 'flushleft',
                        center = 'center',
                        right = 'flushright')
    align1 <- if (plot1) if (a == 'default') '\n' else sprintf('\n\n\\begin{%s}', align.env)
    align2 <- if (plot2) if (a == 'default') '' else sprintf('\\end{%s}\n\n', align.env)
  }

  ow <- options$out.width
  # maxwidth does not work with animations
  if (animate && identical(ow, '\\maxwidth')) ow <- NULL
  if (is.numeric(ow)) ow <- paste0(ow, 'px')
  size <- paste(c(sprintf('width=%s', ow),
                  sprintf('height=%s', options$out.height),
                  options$out.extra),
                collapse = ',')

  res <- sprintf('\\includegraphics%s{%s} ',
                 if (nzchar(size)) sprintf('[%s]', size) else size,
                 if (getOption('knitr.include_graphics.ext', FALSE)) x else xfun::sans_ext(x))
  lnk <- options$fig.link
  stmt <- if (!is.null(lnk) && !is.na(lnk))
    res <- sprintf('\\href{%s}{%s}', lnk, res)

  out_val <- paste0(fig1,
                    align1,
                    cap1, # includes caption, label, and note
                    sub1,
                    resize1,
                    res,
                    resize2,
                    sub2,
                    sep.cur,
                    align2,
                    fig2)

  out_val
}

`%n%` <- function(x, y) {
  if (is.null(x)) y else x
}

create_caption <- function(options) {

  cap <- options$fig.cap
  scap <- options$fig.scap
  note <- options$fig.note

  if (is.null(cap) || cap == '')
    return('')

  if (is.null(scap) && !grepl('[{].*?[:.;].*?[}]', cap))
    scap <- strsplit(cap, '[:.;]( |\\\\|$)')[[1L]][1L]

  out_cap <- sprintf('\\caption%s{%s}\n',
                     if (is.null(scap) || is.na(scap)) '' else sprintf('[%s]', scap),
                     cap)

  out_cap
}

create_note <- function(options) {
  note <- options$fig.note

  a <- options$fig.align
  old_align <- switch(a,
                      left = '\\RaggedRight',
                      center = '\\centering',
                      right = '\\RaggedLeft',
                      '')

  if (is.null(note) || note == '' || is.na(note))
    return('')

  out_note <- sprintf('\\fnote{%s}%s\n\\medskip',
                      note,
                      old_align)
}

