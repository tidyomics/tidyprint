#' Print a styled tidyprint message
#'
#' Prints a message to the console with a consistent tidyomics style.
#' A package-specific prefix is automatically added based on the calling
#' package name, followed by the message text. Different message types
#' are displayed using appropriate styles from the \pkg{cli} package.
#'
#' Frequency control uses the tidyverse messaging API
#' (`cli::cli_inform()` / `rlang::inform()`). Use `frequency = "once"`
#' to show a message once per R session.
#'
#' @param message A character string containing the message to display.
#' @param type The type of message to display. One of
#'   \code{"info"}, \code{"success"}, \code{"warning"}, or \code{"danger"}.
#'   Defaults to \code{"info"}.
#' @param frequency How often the message should be displayed. One of
#'   \code{"always"} (default), \code{"regularly"} (once every 8 hours),
#'   or \code{"once"} (once per R session). Passed to
#'   \code{\link[rlang:abort]{rlang::inform()}}.
#' @param frequency_id A unique identifier used to recognise the same
#'   message when \code{frequency} is not \code{"always"}. Required in
#'   that case; see \code{\link[rlang:abort]{rlang::inform()}}.
#'
#' @return Invisibly returns \code{NULL}. Called for side effects (printing
#'   a styled message to the console).
#'
#' @export
#' @examples
#' tidy_message("Loading data...", type = "info")
#' tidy_message("Data loaded successfully!", type = "success")
#' tidy_message("Shown once per session.", type = "warning",
#'              frequency = "once", frequency_id = "example-once")
tidy_message <- function(message,
                         type = c("info", "success", "warning", "danger"),
                         frequency = c("always", "regularly", "once"),
                         frequency_id = NULL) {
  type <- match.arg(type, choices = c("info", "success", "warning", "danger"))
  frequency <- match.arg(frequency, choices = c("always", "regularly", "once"))

  if (frequency != "always" && is.null(frequency_id)) {
    stop("`frequency_id` must be supplied when `frequency` is not \"always\".",
         call. = FALSE)
  }

  calling_package <- utils::packageName(parent.frame())
  prefix_package <- ifelse(is.null(calling_package), "Console", calling_package)
  prefix <- paste0(prefix_package, " says")

  bullet <- switch(
    type,
    info = "i",
    success = "v",
    warning = "!",
    danger = "x"
  )

  formatted <- stats::setNames("{prefix}: {message}", bullet)

  cli::cli_inform(
    formatted,
    .frequency = frequency,
    .frequency_id = frequency_id
  )
}

#' Demonstrate tidy_message usage
#'
#' Runs through examples of the [`tidy_message()`] function for all
#' supported message types (`"info"`, `"success"`, `"warning"`, `"danger"`).
#' Intended for demonstration and testing only.
#'
#' @return Invisibly returns `NULL`. Called for side effects (messages
#'   printed to the console).
#' @export
#' @examples
#' demo_tidy_message()
demo_tidy_message <- function() {
  tidy_message("This is an informational message send within tidyprint package.")
  tidy_message("Operation completed successfully!", type = "success")
  tidy_message("Potential issue detected.", type = "warning")
  tidy_message("Operation failed.", type = "danger")
}
