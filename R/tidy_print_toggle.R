#' Enable or disable tidy print for SummarizedExperiment objects
#'
#' These functions control whether SummarizedExperiment objects use the
#' custom tidy print format or the standard Bioconductor print format.
#' By default, standard print is used.
#'
#' @param permanent Logical (default \code{FALSE}). If \code{TRUE}, saves the
#'   setting to a local cache file so it persists across R sessions. If \code{FALSE},
#'   the setting only affects the current R session.
#'
#' @details
#' The tidy print format provides a compact tibble-like display that combines
#' assay data with sample and feature metadata. The standard print format is
#' the default Bioconductor display.
#'
#' When tidy print is enabled, SummarizedExperiment objects will display using
#' the custom tibble abstraction format. When disabled (default), they will use
#' the standard SummarizedExperiment print method from the Bioconductor package.
#'
#' The setting is stored in the global R options as `tidyprint.use_tidy_print`.
#' When \code{permanent = TRUE}, the setting is also saved to a cache file in
#' the user's R configuration directory, which takes precedence over the option.
#' The cache file location is determined by \code{tools::R_user_dir("tidyprint", "config")}.
#'
#' @return
#' \itemize{
#'   \item `tidy_print_on()`: Returns `TRUE` invisibly after enabling tidy print
#'   \item `tidy_print_off()`: Returns `FALSE` invisibly after disabling tidy print
#'   \item `tidy_print_enabled()`: Returns a logical indicating whether tidy print is currently enabled
#' }
#'
#' @seealso \link{print.SummarizedExperiment}
#'
#' @examples
#' \dontrun{
#'   # Check current status
#'   tidy_print_enabled()
#'
#'   # Enable tidy print (session only)
#'   tidy_print_on()
#'
#'   # Enable tidy print permanently (saved to cache)
#'   tidy_print_on(permanent = TRUE)
#'
#'   # Disable tidy print (use standard print)
#'   tidy_print_off()
#'
#'   # Disable tidy print permanently
#'   tidy_print_off(permanent = TRUE)
#' }
#'
#' @name tidy_print_toggle
NULL

# Internal helper functions for cache management

#' @keywords internal
.get_cache_path <- function() {
  cache_dir <- tools::R_user_dir("tidyprint", "config")
  file.path(cache_dir, "tidy_print_enabled.rds")
}

#' @keywords internal
.read_cache <- function() {
  cache_file <- .get_cache_path()
  if (file.exists(cache_file)) {
    readRDS(cache_file)
  } else {
    NULL
  }
}

#' @keywords internal
.write_cache <- function(value) {
  cache_file <- .get_cache_path()
  cache_dir <- dirname(cache_file)
  
  # Create directory if it doesn't exist
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  tryCatch({
    saveRDS(value, cache_file)
    invisible(TRUE)
  }, error = function(e) {
    warning("Failed to write cache file: ", e$message)
    invisible(FALSE)
  })
}

#' @rdname tidy_print_toggle
#' @export
tidy_print_on <- function(permanent = FALSE) {
  options(tidyprint.use_tidy_print = TRUE)
  if (permanent) .write_cache(TRUE)
  invisible(TRUE)
}

#' @rdname tidy_print_toggle
#' @export
tidy_print_off <- function(permanent = FALSE) {
  options(tidyprint.use_tidy_print = FALSE)
  if (permanent) .write_cache(FALSE)
  invisible(FALSE)
}

#' @rdname tidy_print_toggle
#' @export
tidy_print_enabled <- function() {
  # Option takes precedence over cache
  option_value <- getOption("tidyprint.use_tidy_print")
  cache_value <- .read_cache()
  
  # If option is explicitly set, use it
  if (!is.null(option_value)) {
    # Warn if cache exists and disagrees
    if (!is.null(cache_value) && cache_value != option_value) {
      message(
        "tidyprint: R option 'tidyprint.use_tidy_print' (", option_value, 
        ") overrides cache value (", cache_value, "). ",
        "Use tidy_print_", if(option_value) "on" else "off", "(permanent = TRUE) to update cache."
      )
    }
    option_value
    
  } 
  # If cache exists and option is not set, use cache    
  else if (!is.null(cache_value)) cache_value
  # If neither option nor cache is set, return FALSE
  else FALSE
}
