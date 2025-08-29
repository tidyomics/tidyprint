NBSP <- "\U00A0"

pillar___format_comment <- function (x, width, strip.spaces = TRUE)
{
  if (length(x) == 0L) {
    return(character())
  }
  map_chr(x, pillar___wrap, prefix="# ",
          width=min(width, cli::console_width()), strip.spaces = strip.spaces)
}

#' @importFrom fansi strwrap_ctl
pillar___strwrap2 <- function (x, width, indent, strip.spaces = TRUE)
{
  fansi::strwrap2_ctl(x, width=max(width, 0), indent=indent,
                     exdent=indent + 2, strip.spaces = strip.spaces)
}


pillar___wrap <- function (..., indent=0, prefix="", width, strip.spaces = TRUE)
{

  x <- paste0(..., collapse="")
  wrapped <- pillar___strwrap2(x, width, indent, strip.spaces = strip.spaces)
  wrapped <- paste0(prefix, wrapped)
  wrapped <- gsub(NBSP, " ", wrapped)
  paste0(wrapped, collapse="\n")
}
