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
#' @importFrom stringr str_replace_all
#' @importFrom purrr map2_chr
#' @export
tbl_format_header.SE_print_abstraction <- function(x, setup, ...) {
  number_of_features <- x |> attr("number_of_features")
  number_of_samples <- x |> attr("number_of_samples")
  named_header <- x |> attr("named_header")
  assay_names <- x |> attr("assay_names")
  separator_row <- x |> attr("separator_row")
  covariate_names <- x |> attr("covariate_names")
  
  number_of_total_rows = (x |> attr("number_of_features")) * (x |> attr("number_of_samples"))
  
  printed_colnames <- x |> attr("printed_colnames")

  # Identify covariate columns: those from colData
  # Assume covariate columns are after .sample, .feature, .count, and assay columns
  # We'll use heuristics: find the first and last covariate column positions
  
  # .feature and .samples SHOULD BE A GLOBAL VARIABLE CREATED ONES
  # SO IT CAN BE CHANGED ACROSS THE PACKAGE
  # THIS BREAKS IF I HAVE ROWDATA
  covariate_candidates <- setdiff(printed_colnames, c(".sample", ".feature", "|", assay_names))
  # Remove gene/rowData columns if possible (e.g., chromosome, gene_feature, ...)
  # For now, just use all columns after .count and before gene_feature as covariates
  all_printed_covariates = 
  first_covariate <- which(printed_colnames %in% covariate_candidates)[1]
  last_covariate <- which(printed_colnames %in% covariate_candidates) |> tail(1)
  last_covariate <- if (length(last_covariate) > 0) max(last_covariate) else NA

  # Only add header if there are covariate columns
  covariate_header <- NULL
  if (!is.na(first_covariate) && !is.na(last_covariate) && last_covariate >= first_covariate) {
    # Build a header row with blanks except for the covariate span
    header_row <-
      map2_chr(separator_row, names(separator_row), ~ if_else(.y %in% covariate_names, " ", .x))  |>
      str_replace_all("-", " ")
    
    
    span_length <- last_covariate - first_covariate + 1
    # Adapt label length
    label <- paste0("-- COVARIATES ", paste(rep("-", max(0, span_length * 3 - 13)), collapse=""), "--")
    # Abbreviate if too long
    if (nchar(label) > span_length * 8) label <- "-- COVAR --"
    
    difference_spacing = separator_row[names(separator_row) %in% covariate_names & names(separator_row) %in% printed_colnames] |> map_int(nchar) |> sum() - nchar(label)

    header_row[first_covariate] <- paste0("| ", label)
    header_row[last_covariate] <- paste0(header_row[last_covariate], "|")
    
    # Spacer
    if(last_covariate > (first_covariate+1)) header_row[first_covariate +1] = rep("-", difference_spacing) |> paste(collapse = "")
    
    header_row = paste(rep(" ", number_of_total_rows |> nchar() -2), collapse = "") |> c(header_row)
    covariate_header <- paste(header_row, collapse=" ")
    covariate_header <- cli::col_br_blue(covariate_header)
  }

  # Compose the main header as before
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
      #append(
      paste0( cli::col_br_black( sprintf(
        "Features=%s | Samples=%s | Assays=%s",
        number_of_features,
        number_of_samples,
        assay_names %>% paste(collapse=", ")
      )))
      # , after = 1)
  }
  # Add covariate header if present
  if (!is.null(covariate_header)) {
    header <- c(header, covariate_header)
  }
 
  style_subtle(pillar___format_comment(header, width=setup$width, strip.spaces = FALSE))
}

# type_sum.sep <- function(x, ...) {
#   '|'
# }
#
# registerS3method("type_sum", "sep", type_sum.sep)
