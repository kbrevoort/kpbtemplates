#' Working Paper
#'
#' This function sets the default for my working paper format.
#' @export
working_paper <- function(...) {
  template <- system.file("rmarkdown/templates/kpbworkingpaper/resources/brevoortwp.tex",
                          package="kpbtemplates")
  bookdown::pdf_document2(...,
                          template = template
  )
}
