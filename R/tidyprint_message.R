#' Print a styled tidyprint message
#'
#' Prints a message to the console with a consistent tidyomics style.
#' A package-specific prefix is automatically added based on the calling
#' package name, followed by the message text. Different message types
#' are displayed using appropriate styles from the \pkg{cli} package.
#'
#' @param message A character string containing the message to display.
#' @param type The type of message to display. One of
#'   \code{"info"}, \code{"success"}, \code{"warning"}, or \code{"danger"}.
#'   Defaults to \code{"info"}.
#'
#' @return Invisibly returns \code{NULL}. Called for side effects (printing
#'   a styled message to the console).
#'
#' @export
#' @examples
#' tidy_message("Loading data...", type = "info")
#' tidy_message("Data loaded successfully!", type = "success")
tidy_message <- function(message, type = c("info", "success", "warning", "danger")) {
  type <- match.arg(type, choices = c("info", "success", "warning", "danger"))

  calling_package <- utils::packageName(parent.frame())
  prefix_package <- ifelse(is.null(calling_package), "Console", calling_package)

  # prefix <- switch(
  #   type,
  #   info = paste0(prefix_package, " says"),
  #   success = paste0(prefix_package, " success"),
  #   warning = paste0(prefix_package, " warning"),
  #   danger = paste0(prefix_package, " error")
  # )

  prefix = paste0(prefix_package, " says")

  style_fun <- switch(
    type,
    info = cli::cli_alert_info,
    success = cli::cli_alert_success,
    warning = cli::cli_alert_warning,
    danger = cli::cli_alert_danger
  )

  style_fun("{prefix}: {message}")
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
