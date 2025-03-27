#' Print styled tidyprint messages
#'
#' @param message A character string containing the message to display.
#' @param type The type of message to display ("info", "success", "warning", "danger"). Defaults to "info".
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

#' Test tidy_message function
#' @export
#' @examples
#' demo_tidy_message()
demo_tidy_message <- function() {
  tidy_message("This is an informational message send within tidyprint package.")
  tidy_message("Operation completed successfully!", type = "success")
  tidy_message("Potential issue detected.", type = "warning")
  tidy_message("Operation failed.", type = "danger")
}
