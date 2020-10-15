#' Working Paper
#'
#' This function sets the default for my working paper format.
#' @importFrom bookdown pdf_document2
#' @importFrom zotero2r create_bibliography
#' @export
working_paper <- function(..., latex_engine = 'pdflatex') {
  #browser()
  tex_template <- system.file("rmarkdown/templates/kpbworkingpaper/resources/brevoortwp2.tex",
                              package="kpbtemplates")

  yaml_front_matter <- get_from_parent_frame('yaml_front_matter')
  if (is.null(yaml_front_matter))
    yaml_front_matter <- get_from_parent_frame('front_matter')
  if (is.null(yaml_front_matter))
    stop('Could not located YAML front matter')

  yaml_names <- tolower(names(yaml_front_matter))

  # Select a citation package depending on the YAML settings
  if ('csl' %in% yaml_names) {
    cite_package = 'default'   # Uses CSL through Pandoc -- skips biblatex
  } else if (!any(c('bibliography', 'zotero2r') %in% yaml_names)) {
    cite_package = 'default'
  } else cite_package <- 'biblatex' # The default is to use biblatex

  # Check for latex_engine in yaml_names
  if ('latex_engine' %in% yaml_names)
    latex_engine == yaml_front_matter[['latex_engine']]

  ret_val <- bookdown::pdf_document2(...,
                                     template = tex_template,
                                     toc = FALSE,
                                     citation_package = cite_package,
                                     latex_engine = latex_engine
  )

  #ret_val$pre_processor <- my_wp_preprocessor
  ret_val$post_processor <- my_wp_postprocessor
  #browser()

  #ret_val$post_processor <- function(metadata, input, output, clean, verbose) browser()

  ret_val$inherits <- 'pdf_book'

  # Check if zotero2r is to be used.  If so, create the bibliography file.
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
#' @return The value of the variable, if found. If not found returns NULL.
#' @importFrom rlang env_names env_get
get_from_parent_frame <- function(x) {
  all_frames <- sys.frames()
  for (i in all_frames) {
    if (x %in% rlang::env_names(i)) {
      return(rlang::env_get(env = i, nm = x))
    }
  }

  #stop(paste0('Could not locate ', x))
  NULL
}

test_function <- function(metadata, input, output, clean, verbose) {
  browser()
}

find_csl_files <- function() {
  package_path <- find.package('kpbtemplates')

  local_dir <- 'inst/rmarkdown/templates/kpbWorkingPaper/resources/'
  my_csl <- 'canadian-journal-of-economics.csl'

  if(!file.exists(file.path(package_path,
                            local_dir,
                            my_csl)))
    local_dir <- substring(local_dir, 6L)

  my_path <- file.path(package_path, local_dir, my_csl)
  if (!file.exists(my_path))
    stop('Could not find CSL file.')

  my_path
}

my_wp_postprocessor <- function(metadata, input, output, clean, verbose) {
  #browser()
  # This changes the TeX file
  temp <- readr::read_file(output)
  temp <- sub('\\cslBibliographyStartLine', '\\begin{cslbibliography}', temp)
  temp <- sub('\\cslBibliographyEndLine', '\\end{cslbibliography}', temp)
  temp <- readr::write_file(temp, output)

  # Now call the default post_processor function
  f <- bookdown::pdf_document2()$post_processor
  f(metadata, input, output, clean, verbose)

}
