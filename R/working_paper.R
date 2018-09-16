#' Working Paper
#'
#' This function sets the default for my working paper format.
#' @export
working_paper <- function(...) {
  tex_template <- system.file("rmarkdown/templates/kpbworkingpaper/resources/brevoortwp.tex",
                              package="kpbtemplates")
  ret_val <- bookdown::pdf_document2(...,
                                     template = tex_template,
                                     toc = FALSE,
                                     citation_package = 'biblatex',
                                     latex_engine = 'pdflatex'
  )

  ret_val$inherits <- 'pdf_book'
<<<<<<< HEAD

  ret_val
=======
>>>>>>> ee0aa8c796423ae67985a2a0d6f56a5104e18833
}
