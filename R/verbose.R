
verbose_global <- new.env(parent = emptyenv())


#' Set/Get the verbosity level
#'
#' If no arguments are given, all current verbosity level are returned.
#' If a name is given, the verbosity level for that name is returned.
#' If no name is given and key = value pairs are provided
#' the verbosity levels for these keys will be set using the given values.
#'
#' @param name name for which verbosity level should be returned
#' @param ... key = value pairs to be set
#' @export
verbose <- function(name = NULL, ...) {
  args <- list(...)
  if (!is.null(name)) {
    get_verbosity(name)
  } else if (is.null(name) && length(args) == 0) {
    get_verbosity()
  } else {
    set_verbosity(...)
  }
}


set_verbosity <- function(...) {
  args <- list(...)
  for (key in names(args)) {
    verbose_global[[key]] <- args[[key]]
  }
}


get_verbosity <- function(name = NULL) {
  if (is.null(name)) {
    res <- list()
    for (key in ls(verbose_global)) {
      res[[key]] <- verbose_global[[key]]
    }
    res
  } else {
    verbose_global[[name]]
  }
}


#' Clear all verbosity settings
#' @export
clear_verbose <- function() {
  rm(list = ls(verbose_global), envir = verbose_global)
}

