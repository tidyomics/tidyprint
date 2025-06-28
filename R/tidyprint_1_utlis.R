#' @importFrom pillar pillar_component
#' @importFrom pillar new_pillar_shaft
#' @importFrom pillar ctl_new_rowid_pillar
#' @importFrom pillar new_pillar
#' @importFrom rlang names2
#' @importFrom pillar align
#' @importFrom pillar get_extent
#' @importFrom pillar style_subtle
#' @importFrom pillar tbl_format_header
#' @importFrom cli col_br_black
#' @importFrom tibble as_tibble
#' @importFrom stringr str_replace_all
#' @importFrom purrr map2_chr
#' @importFrom purrr map_int
#' @importFrom dplyr if_else
#' @importFrom pillar pillar___format_comment
#' @importFrom pillar NBSP
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

<<<<<<< HEAD

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
=======
#' Format covariate header by distributing label across covariate columns
#' @param separator_row The separator row with column widths
#' @param printed_colnames The printed column names
#' @param covariate_names The names of covariate columns
#' @param number_of_total_rows The total number of rows for spacing
#' @param label The label to distribute (default: "COVARIATES")
#' @return Formatted header string
#' @export
format_covariate_header <- function(separator_row, printed_colnames, covariate_names, number_of_total_rows, label = "COVARIATES") {
  header_row <-
    map2_chr(separator_row, names(separator_row), ~ if_else(.y %in% covariate_names, .x, .x |> str_replace_all("-", " ")))

  covariate_indices <- which(printed_colnames %in% covariate_names)
  covariate_widths <- separator_row[printed_colnames[covariate_indices]] |> purrr::map_int(nchar)
  total_covariate_width <- sum(covariate_widths)

  # Build a string of dashes for all covariate columns
  dash_string <- paste(rep("-", total_covariate_width), collapse = "")

  # Overlay the label onto the dash string, centered
  label_start <- floor((total_covariate_width - nchar(label)) / 2) + 1
  label_end <- label_start + nchar(label) - 1
  chars <- strsplit(dash_string, "")[[1]]
  label_chars <- strsplit(label, "")[[1]]
  if (label_start > 0 && label_end <= total_covariate_width) {
    chars[label_start:label_end] <- label_chars
  } else {
    # If label is too long, truncate
    chars[1:length(label_chars)] <- label_chars
  }
  overlayed <- paste(chars, collapse = "")

  # Split overlayed string back into covariate column widths
  split_labels <- character(length(covariate_widths))
  pos <- 1
  for (i in seq_along(covariate_widths)) {
    split_labels[i] <- substr(overlayed, pos, pos + covariate_widths[i] - 1)
    pos <- pos + covariate_widths[i]
  }

  # Place split_labels into the header_row at covariate_indices
  for (i in seq_along(covariate_indices)) {
    header_row[covariate_indices[i]] <- split_labels[i]
  }

  # Add row ID spacing at the beginning
  header_row <- c(paste(rep(" ", number_of_total_rows |> nchar() - 2), collapse = ""), header_row)
  paste(header_row, collapse = " ")
}

>>>>>>> 4b7ac46 (more scalable COVRIATE header)
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
  first_covariate <- which(printed_colnames %in% covariate_candidates)[1]
  last_covariate <- which(printed_colnames %in% covariate_candidates) |> tail(1)
  last_covariate <- if (length(last_covariate) > 0) max(last_covariate) else NA

  # Only add header if there are covariate columns
  covariate_header <- NULL
  if (!is.na(first_covariate) && !is.na(last_covariate) && last_covariate >= first_covariate) {
    covariate_header <- format_covariate_header(
      separator_row = separator_row,
      printed_colnames = printed_colnames,
      covariate_names = covariate_names,
      number_of_total_rows = number_of_total_rows,
      label = "COVARIATES"
    )
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
