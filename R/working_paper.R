#' Working Paper
#'
#' This function sets the default for my working paper format.
#' @importFrom bookdown pdf_document2
#' @importFrom zotero2r create_bibliography
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

  # Check if zotero2r is to be used.  If so, create the bibliography file.
  yaml_front_matter <- get_from_parent_frame('yaml_front_matter')
  if ('zotero2r' %in% tolower(names(yaml_front_matter))) {
    if (!is.logical(yaml_front_matter$zotero2r) | yaml_front_matter$zotero2r != FALSE) {
      rmd_file <- get_from_parent_frame('original_input')
      zotero2r::create_bibliography(rmd_file)
    }
  }
  #ret_val$pre_processor <- test_function(metadata, input, output, clean, verbose)

  ret_val
}

#' Get from Parent Frame
#'
#' Searches parent frames to find a specified variable.
#' @param x Scalar character vector giving the name of the variable to be searched
#' for.
#' @importFrom rlang env_names env_get
get_from_parent_frame <- function(x) {
  all_frames <- sys.frames()
  for (i in all_frames) {
    if (x %in% rlang::env_names(i)) {
      return(rlang::env_get(env = i, nm = x))
    }
  }

  stop(paste0('Could not locate ', x))
}

test_function <- function(metadata, input, output, clean, verbose) {
  browser()
}
