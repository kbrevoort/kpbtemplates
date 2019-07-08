#' Working Paper
#'
#' This function sets the default for my working paper format.
#' @export
working_paper <- function(...) {
  tex_template <- system.file("rmarkdown/templates/kpbworkingpaper/resources/brevoortwp2.tex",
                              package="kpbtemplates")
  ret_val <- bookdown::pdf_document2(...,
                                     template = tex_template,
                                     toc = FALSE,
                                     citation_package = 'biblatex',
                                     latex_engine = 'pdflatex'
  )

  ret_val$inherits <- 'pdf_book'

  ret_val
}
