#' Working Paper
#'
#' This function sets the default for my working paper format.
#' @importFrom bookdown pdf_document2
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
    latex_engine <- yaml_front_matter[['latex_engine']]

  ret_val <- bookdown::pdf_document2(...,
                                     template = tex_template,
                                     toc = FALSE,
                                     citation_package = cite_package,
                                     latex_engine = latex_engine
  )

  #ret_val$pre_processor <- my_wp_preprocessor
  ret_val$post_processor <- my_wp_postprocessor
  ret_val$pre_processor <- my_wp_preprocessor
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

#' Use Default CSL
#'
#' A convenience function that allows users to use a default CSL file
#' that is included with the package. The default is brevoort.csl, which
#' is a custom CSL specification that I created.
#' @return A character scalar giving the full path to brevoort.csl
#' @export
use_default_csl <- function() {
  package_path <- find.package('kpbtemplates')

  local_dir <- 'inst/rmarkdown/templates/kpbWorkingPaper/resources/'
  my_csl <- 'brevoort.csl'

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
  # Make necessary changes to the intermediate markdown file before processing
  readr::read_file(output) %>%
    replace_duplicate_punctuation() %>%
    add_csl_environment() %>%
    readr::write_file(output)

  # Now call the default post_processor function
  f <- bookdown::pdf_document2()$post_processor
  f(metadata, input, output, clean, verbose)

}

my_wp_preprocessor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
  in_file <- readr::read_file(input_file) %>%
    strsplit('\n') %>%
    unlist()

  # Find the end point of the YAML header
  yaml_range <- which(in_file == '---')
  if(length(yaml_range) < 2L) {
    warning('Unable to find YAML header during preprocessing.')
    return(NULL)
  }

  for(i in yaml_range[1L]:yaml_range[2L]) {
    if (grepl('^csl:[[:space:]\"\']*default[[:space:]\"\']*$', in_file[i])) {
      in_file[i] <- sprintf("csl: \"%s\"", use_default_csl())
    } else if (grepl('^date:[[:space:]]*today[[:space:]]*$', in_file[i])) {
      in_file[i] <- sprintf('date: \"%s\"', format(Sys.Date(), '%B %e, %Y'))
    }
  }

  paste(in_file, collapse = '\n') %>%
    readr::write_file(in_file, path = input_file)

  NULL
}

#' Replace Duplicate Punctuation
#'
#' In some cases, CSL bibliographies may allow redundant punctuation.  For example,
#' when an item's title ends in a question mark, this will frequently result in
#' the title ending in a "?." which looks ugly.  This function searchs the bibliography
#' for such duplicates and, when found, removes the second punctuation.
#' @param x Character scalar that contains all the text in the intermediate markdown file
#' @return Character scalar with the duplicate punctuation removed
#' @importFrom stringr str_locate_all str_sub
#' @importFrom purrr map_dfr
#' @importFrom dplyr filter "%>%" distinct arrange
replace_duplicate_punctuation <- function(x) {
  all_dup_positions <- stringr::str_locate_all(x, '[!?][,.]')

  all_starts <- stringr::str_locate_all(x, 'cslBibliographyStartLine')[[1]][,2L] %>%
    unname()
  all_ends <- stringr::str_locate_all(x, 'cslBibliographyEndLine')[[1]][, 1L] %>%
    unname()

  if (length(all_starts) != length(all_ends)) {
    warning('There is an inconsistent number of bibliography start and end points.  Skipping replacement of duplicate punctuation.')
    return(x)
  }

  all_doubles <- stringr::str_locate_all(x, '[!?][,.]')[[1L]] %>%
    as.data.frame()
  n <- dim(all_doubles)[1]
  if (n == 0) return(x)    # If there are no duplicates, make no changes

  doubles_in_bib <- purrr::map_dfr(c(1:length(all_starts)),
                                   ~ dplyr::filter(all_doubles,
                                                   start >= all_starts[.x],
                                                   end <= all_ends[.x])) %>%
    distinct() %>%
    arrange(desc(end))

  n <- dim(doubles_in_bib)[1]
  if (n == 0) return(x)

  for (i in 1:n)
    stringr::str_sub(x, doubles_in_bib$end, doubles_in_bib$end) <- ''

  x
}

#' Add CSL Environment
#'
#' Replaces the temporary place holders for the CSL environment with Latex-formatted
#' begin and end designations.
#' @param x Character scalar with the intermediate markdown file
#' @return Character scalar with the intermediate markdown file with changes made.
#' @importFrom dplyr "%>%"
add_csl_environment <- function(x) {
  x %>%
    sub('\\cslBibliographyStartLine', '\\begin{cslbibliography}', .) %>%
    sub('\\cslBibliographyEndLine', '\\end{cslbibliography}', .)
}
