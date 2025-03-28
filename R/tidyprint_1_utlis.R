
#' @importFrom pillar pillar_component
#' @importFrom pillar new_pillar_shaft
#' @importFrom pillar ctl_new_rowid_pillar
#' @importFrom pillar new_pillar
#' @export
ctl_new_rowid_pillar.SE_print_abstraction <- function(controller, x, width, ...) {
  # message('attrx =', x %>% attributes())
  # message('attrc = ', controller %>% attributes() %>% names())
  #
  # print(controller %>% attributes())
  out <- NextMethod()

  n =  controller |> attr('n_print')

  total_rows = controller |> attr('total_rows')

  # message('n=', n)
  # message('total_rows=', total_rows)

  # Generate row IDs: First n/2 rows, then a separator, then last n/2 rows
  if (total_rows > n) {
    rowid <- c(
      seq_len(ceiling(n / 2)), # Top half
      NA,               # Separator row (will be replaced with `---`)
      (total_rows - floor(n / 2) + 1):total_rows # Bottom half
    )
  } else {
    rowid <- seq_len(total_rows)
  }

  # Convert row IDs to characters and replace NA with separator
  rowid <- as.character(rowid)
  rowid[is.na(rowid)] <- ""

  # Determine the maximum width required
  width <- max(nchar(rowid))

  # Create the custom pillar
  new_pillar(
    list(
      title = out$title,
      type = out$type,
      data = pillar_component(
        new_pillar_shaft(list(row_ids = rowid),
                         width = width,
                         class = "pillar_rif_shaft"
        )
      )
    ),
    width = width
  )

}


#' @importFrom pillar pillar ctl_new_pillar
#' @export
ctl_new_pillar.SE_print_abstraction <- function(controller, x, width, ..., title = NULL) {

  if (inherits(x, "|")) {
    p <- pillar(x, title = "|", ...)
    class(p$title[[1]]) <- "blank_pillar_title"
    class(p$type[[1]]) <- "blank_pillar_type"
    attr(p$type, "width") <- 1L
    attr(p$type, "min_width") <- 1L
    attr(p$type[[1]], "width") <- 1L
    attr(p$type[[1]], "min_width") <- 1L
    p
  } else {
    NextMethod()
  }

}



#' @importFrom rlang names2
#' @importFrom pillar align
#' @importFrom pillar get_extent
#' @importFrom pillar style_subtle
#' @importFrom pillar tbl_format_header
#' @importFrom cli col_br_black
#' @importFrom tibble as_tibble
#' @export
tbl_format_header.SE_print_abstraction <- function(x, setup, ...) {

  number_of_features <- x |> attr("number_of_features")
  number_of_samples <- x |> attr("number_of_samples")
  named_header <- x |> attr("named_header")
  assay_names <- x |> attr("assay_names")


  if (all(names2(named_header) == "")) {
    header <- named_header
  } else {
    header <-
      paste0(
        align(paste0(names2(named_header), ":"), space=NBSP),
        " ",
        named_header
      ) %>%
      # Add further info single-cell
      append( cli::col_br_black( sprintf(
        " Features=%s | Samples=%s | Assays=%s",
        number_of_features,
        number_of_samples,
        assay_names %>% paste(collapse=", ")
      )), after = 1)
  }
  style_subtle(pillar___format_comment(header, width=setup$width))
}

# type_sum.sep <- function(x, ...) {
#   '|'
# }
#
# registerS3method("type_sum", "sep", type_sum.sep)
