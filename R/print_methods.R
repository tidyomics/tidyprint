#' Print method for SummarizedExperiment with tidyprint styles
#'
#' Provides a compact tibble-like display for a \link[SummarizedExperiment]{SummarizedExperiment}
#' object that preserves assay values and key covariates with a separator band when
#' the table is truncated.
#'
#' @param x A \code{SummarizedExperiment} object to print.
#' @param n Integer (default \code{10}). Approximate number of rows to show
#'   in the display. When the total cells shown are fewer than \code{n}, the
#'   full table is printed and the separator row is suppressed.
#' @param ... Additional arguments passed to internal printers (currently unused).
#'
#' @details
#' This method constructs a tibble abstraction for SummarizedExperiment data with
#' columns: \code{.feature}, \code{.sample}, assay columns, a vertical separator \code{"|"},
#' followed by selected \code{colData} and \code{rowData} fields. When the output
#' is truncated, an adaptive dash-only separator row is inserted after the first
#' half block of rows. Additional indication of \code{colData} is provided as well.
#'
#' By default, this method uses the standard SummarizedExperiment print format.
#' To enable the tidy print format, use \code{\link{tidy_print_on}}. To check
#' the current status, use \code{\link{tidy_print_enabled}}.
#'
#' @return \code{x} is returned \emph{invisibly} after printing.
#'
#' @seealso \link[SummarizedExperiment]{SummarizedExperiment}, \link[tibble]{as_tibble}
#'
#' @examples
#' \dontrun{
#'   library(tidyprint)
#'   print(se_airway)                         # compact tibble display
#'   print(se_airway, n = 20)                # with custom row count
#' }
#'
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#' @importFrom methods setMethod


#' @importFrom vctrs new_data_frame vec_rep vec_rep_each
#' @importFrom SummarizedExperiment assayNames assays rowData assays<- rowRanges
#' @importFrom stats setNames
#' @importFrom S4Vectors coolcat
#' @importFrom purrr map_chr keep
#' @importFrom stringr str_replace
#' @importFrom magrittr `%>%`
#' @importFrom dplyr if_else mutate across all_of
#' @export
print.SummarizedExperiment <- function(x, n = 10, ...) {
  print_tibble_abstraction(x, n = n, ...)
  invisible(x)
}

# Describe how an object is laid out as a tibble abstraction
#
# Dispatch follows the class hierarchy, so a method extends the layout of its
# parent class through `NextMethod()` rather than restating it. The layout names
# the column identifier, the colData columns to show next to it and the ones to
# show inside the COVARIATES band, and the fields the header reports, in the
# order they are reported. Fields are data, so a class adds what it has to say
# about itself, cells or spatial coordinates say, without the renderer knowing
# about it.
#
# @noRd
tidyprint_layout <- function(x, ...) {
  UseMethod("tidyprint_layout")
}

#' The layout of a SummarizedExperiment: samples are the columns of the object,
#' and every colData column is a covariate of theirs
#'
#' @noRd
#' @export
tidyprint_layout.SummarizedExperiment <- function(x, ...) {
  list(
    column_name = ".sample",
    covariate_columns = colnames(colData(x)),
    header = list(
      Features = nrow(x),
      Samples = ncol(x),
      Assays = assays(x) %>% names %>% paste(collapse = ", ")
    )
  )
}

# Build and print the tibble abstraction of a rectangular assay object
#
# Columns are laid out as `.feature`, the column identifier, then assays, then
# the colData columns of `layout$covariate_columns` inside the COVARIATES band,
# then the colData columns of `layout$cell_columns`, which vary within a sample
# and so stay out of the band, then rowData. Anything the layout leaves out, such
# as the cells of a SingleCellExperiment, is absent from the display and from the
# header.
#
# @noRd
print_tibble_abstraction <- function(x, n = 10, layout = tidyprint_layout(x), ...) {
  column_name <- layout$column_name
  cell_columns <- layout$cell_columns
  covariate_columns <- layout$covariate_columns

  onr <- nr <- nrow(x) %>% as.double()
  onc <- nc <- ncol(x) %>% as.double()
  
  if ( onc > 0 && onr > 0 && n / onc >= onr ) {
    n <- onc*onr
    separator_row_flag = FALSE
  }else{
    separator_row_flag = TRUE
  }

  top_n <- ceiling(n / 2)
  bot_n <- floor(n / 2)
  
  if (bot_n == 0) separator_row_flag = FALSE
  
  row_slice <- if (nr < 2 * n) {
    seq_len(nr)
  } else {
    c(seq_len(n), (nr - n + 1):nr)
  }

  
  col_slice <- if (nc < 2 * n) {
    seq_len(nc)
  } else {
    c(seq_len(n), (nc - n + 1):nc)
  }

  x_ <- x[row_slice, col_slice]
  nr <- nrow(x_)
  nc <- ncol(x_)
  .features <- rownames(x_) %||% seq_len(onr)[row_slice]
  .samples  <- colnames(x_) %||% seq_len(onc)[col_slice]

  assays_ <- purrr::map(assays(x_), as_vec)
  row_    <- purrr::map(rowData(x_), vec_rep, times = nc) |> purrr::map(maybe_phantom)
  col_    <- purrr::map(colData(x_), vec_rep_each, times = nr) |> purrr::map(maybe_phantom)

  nn <- nc * nr
  out <- c(
    setNames(
      list(
        vctrs::vec_rep(.features, times = nc),
        vctrs::vec_rep_each(.samples, times = nr)
      ),
      c(".feature", column_name)
    ),
    list(`|` = sep_(nn)),
    assays_,
    list(`|` = sep_(nn)),
    col_[covariate_columns],
    list(`|` = sep_(nn)),
    # Only a SingleCellExperiment has columns varying within a sample, and they
    # get a section of their own so that the band covers the covariates alone
    if (length(cell_columns)) c(col_[cell_columns], list(`|` = sep_(nn))),
    row_
  )
  attr(out, "row.names") <- c(NA_integer_, -nn)
  class(out) <- c("SE_abstraction", "tbl_df", "tbl", "data.frame")

  sub_seq <- if (nn < 2 * top_n) {
    seq_len(nn)
  } else if (bot_n == 0){
    seq_len(top_n)
  } else {
    c(seq_len(top_n), (nn - bot_n + 1):nn)
  }
    
  out_sub <- out[sub_seq, ]
  
  # Compute the max character width for each column
  separator_row <- vapply(out_sub %>% colnames(), function(col) {
    max_width <- max(nchar(as.character(col)), na.rm = TRUE)  # Get max width in the column
    paste(rep("-", max_width), collapse = "")  # Generate a separator of the same length
  }, character(1))

  if (separator_row_flag){
    
    # Modify the entire tibble to include a separator row across all columns
    ## temporalily convert factor cols to char
    fct_col = map(out_sub, is.factor) %>% keep(~{.x == TRUE}) %>% names
    if (length(fct_col)) out_sub[, fct_col] = out_sub[, fct_col] %>% mutate(across(all_of(fct_col), as.character))
    
    
    out_sub <- suppressWarnings(rbind(
      out_sub[seq_len(top_n),],
      as.list(separator_row),      # Adaptive separator row
      out_sub[(top_n+1):nrow(out_sub), ]
    ))
    ## reverse to factor cols
    if (length(fct_col)) out_sub[, fct_col] = out_sub[, fct_col] %>% mutate(across(all_of(fct_col), as.factor))
  }

  out_sub = out_sub %>%
    vctrs::new_data_frame(class=c('SE_print_abstraction', "tbl_df", "tbl", "data.frame")) %>%
    add_attr(n, 'n') %>%
    add_attr(onc*onr, 'total_rows') %>%
    add_attr(layout$header, "header_fields") %>%
    add_attr(separator_row, "separator_row") |>
    add_attr(covariate_columns, "covariate_names") |>
    add_attr(
      '' %>%
        setNames(sprintf("A %s abstraction", class(x)[1])),
      "named_header"
    )

  out_sub %>% print(n = ifelse(separator_row_flag, n+1, n), ...)
  invisible(NULL)
}

# Assign the new print function to SummarizedExperiment objects
setMethod(
  f = "show",
  signature = "SummarizedExperiment",
  definition = function(object) {
    # Check if tidy print is enabled
    if (!tidy_print_enabled()) {
      # Use standard SummarizedExperiment show method
      # Get the original method from SummarizedExperiment namespace and call it
      methods::getMethod("show", "SummarizedExperiment",
                        where = asNamespace("SummarizedExperiment"))(object)
    } else {
      # Dispatch, so that a subclass with its own print method is honoured
      print(object)
    }
  }
)

#' Print method for SingleCellExperiment with tidyprint styles
#'
#' Provides a compact tibble-like display for a
#' \link[SingleCellExperiment]{SingleCellExperiment} object, telling cell-wise
#' from sample-wise \code{colData} apart.
#'
#' @param x A \code{SingleCellExperiment} object to print.
#' @param n Integer (default \code{10}). Approximate number of rows to show
#'   in the display, as in \code{\link{print.SummarizedExperiment}}.
#' @param ... Additional arguments passed to internal printers (currently unused).
#'
#' @details
#' The display follows \code{\link{print.SummarizedExperiment}}, the columns of
#' the object being cells rather than samples, so the identifier is \code{.cell}
#' and the header reports the number of cells.
#'
#' The samples are the groups of cells named by
#' \code{metadata(x)$tidyomics_sample_id}, which holds the name of the
#' \code{colData} column holding the sample identifier. When it is declared, that
#' column and the covariates constant within it are shown under the
#' \code{COVARIATES} band, the remaining \code{colData} columns are shown next to
#' \code{.cell}, and the header reports the number of samples. When it is absent,
#' the samples are left unreported and \code{colData} is shown as it is for a
#' \code{SummarizedExperiment}.
#'
#' As for \code{SummarizedExperiment}, this format is opt in through
#' \code{\link{tidy_print_on}}.
#'
#' @return \code{x} is returned \emph{invisibly} after printing.
#'
#' @seealso \link[SingleCellExperiment]{SingleCellExperiment},
#'   \link{print.SummarizedExperiment}
#'
#' @examples
#' \dontrun{
#'   library(tidyprint)
#'   tidy_print_on()
#'   sce                                      # compact tibble display
#'   print(sce, n = 20)                       # with custom row count
#' }
#'
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importFrom SummarizedExperiment colData
#' @importFrom methods setMethod
#' @export
print.SingleCellExperiment <- function(x, n = 10, ...) {
  # The display itself is the SummarizedExperiment one, extended by
  # tidyprint_layout.SingleCellExperiment()
  NextMethod()
}

#' Extend the SummarizedExperiment layout with the cell notion: the columns of
#' the object are cells, and the samples are the groups of cells declared by
#' `metadata(x)$tidyomics_sample_id`
#'
#' @noRd
#' @export
tidyprint_layout.SingleCellExperiment <- function(x, ...) {
  layout <- NextMethod()

  layout$column_name <- ".cell"

  sample_id <- metadata(x)$tidyomics_sample_id
  if (!is.null(sample_id) && sample_id %in% colnames(colData(x))) {
    split <- split_col_data(colData(x), sample_id)
    layout$cell_columns <- split$cell
    layout$covariate_columns <- split$sample
  } else {
    sample_id <- NULL
  }

  # Cells are reported next to the features, and the samples inherited from the
  # SummarizedExperiment layout, which counted the columns of the object, become
  # the groups of cells. Assigning NULL drops the field, leaving the samples
  # unreported when no sample identifier is declared
  layout$header <- append(
    layout$header,
    list(Cells = ncol(x)),
    after = which(names(layout$header) == "Features")
  )
  layout$header$Samples <-
    if (!is.null(sample_id)) {
      attr(group_codes(colData(x)[[sample_id]]), "n")
    }

  layout
}

# Assign the new print function to SingleCellExperiment objects
setMethod(
  f = "show",
  signature = "SingleCellExperiment",
  definition = function(object) {
    if (!tidy_print_enabled()) {
      methods::getMethod("show", "SingleCellExperiment",
                         where = asNamespace("SingleCellExperiment"))(object)
    } else {
      # Dispatch, so that a subclass with its own print method is honoured
      print(object)
    }
  }
)
