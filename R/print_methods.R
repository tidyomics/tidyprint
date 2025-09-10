#' Print method for SummarizedExperiment with tidyprint styles
#'
#' Provides alternative console displays for a \link[SummarizedExperiment]{SummarizedExperiment}
#' object. The default shows the standard Bioconductor summary. The
#' `"tidyprint_1"` design prints a compact tibble-like abstraction that
#' preserves assay values and key covariates with a separator band when
#' the table is truncated.
#'
#' @param x A \code{SummarizedExperiment} object to print.
#' @param design Either a character string or integer selecting the print style.
#'   Character choices are:
#'   \itemize{
#'     \item \code{"SummarizedExperiment"} — standard Bioconductor summary.
#'     \item \code{"tidyprint_1"} — tidyprint tibble abstraction (top/bottom slices,
#'       assays | colData | rowData blocks, with an adaptive separator row).
#'   }
#'   Numeric shortcuts are mapped as:
#'   \itemize{
#'     \item \code{1} \eqn{\to} \code{"SummarizedExperiment"}
#'     \item \code{2} \eqn{\to} \code{"tidyprint_1"}
#'   }
#' @param n_print Integer (default \code{10}). Approximate number of rows to show
#'   in the \code{"tidyprint_1"} display. When the total cells shown are fewer
#'   than \code{n_print}, the full table is printed and the separator row is
#'   suppressed.
#' @param ... Additional arguments passed to internal printers (currently unused).
#'
#' @details
#' The \code{"tidyprint_1"} design constructs a tibble abstraction for SummarizedExperiment
#' data with columns:
#' \code{.feature}, \code{.sample}, assay columns, a vertical separator \code{"|"},
#' followed by selected \code{colData} and \code{rowData} fields. When the output
#' is truncated, an adaptive dash-only separator row is inserted after the first
#' half block of rows. Additional indication of \code{colData} is provided as well.
#'
#' @return \code{x} is returned \emph{invisibly} after printing.
#'
#' @seealso \link[SummarizedExperiment]{SummarizedExperiment}, \link[tibble]{as_tibble}
#'
#' @examples
#' \dontrun{
#'   library(tidyprint)
#'   print(se_airway)                         # default 
#'   print(se_airway, design = "tidyprint_1") # tidyprint abstraction
#'   print(se_airway, design = 2)             # numeric alias for "tidyprint_1"
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
#' @importFrom dplyr if_else mutate across
#' @export
print.SummarizedExperiment <- function(x, design = 2, n_print = 10, ...) {

  # Match the user-supplied design argument to one of the valid choices:
  if (is.numeric(design)) {
    # Allowed numeric -> corresponding design
    design_map <- c("SummarizedExperiment", "tidyprint_1", "tidySummarizedExperiment", "plyxp")
    
    # Validate numeric input
    if (!design %in% seq_len(4)) {
      stop("Invalid numeric design argument. Choose 1 or 2.")
    }
    design <- design_map[design]
  }

  design <- match.arg(design, c("SummarizedExperiment", "tidyprint_1", "tidySummarizedExperiment", "plyxp"))
  

  if (!inherits(x, "SummarizedExperiment")) {
    stop("The object provided is not a SummarizedExperiment.")
  }

  # --- 1) SUMMARIZED EXPERIMENT (DEFAULT) ---
  if (design == "SummarizedExperiment") {

    print_summary_summarized_experiment <- function(object) {
      cat("class:", class(object), "\n")
      cat("dim:", dim(object), "\n")

      expt <- names(metadata(object))
      if (is.null(expt)) expt <- character(length(metadata(object)))
      coolcat("metadata(%d): %s\n", expt)

      nms <- assayNames(object)
      if (is.null(nms)) nms <- character(length(assays(object, withDimnames = FALSE)))
      coolcat("assays(%d): %s\n", nms)

      rownames <- rownames(object)
      if (!is.null(rownames)) coolcat("rownames(%d): %s\n", rownames)
      else cat("rownames: NULL\n")

      coolcat("rowData names(%d): %s\n", names(rowData(object, use.names = FALSE)))

      colnames <- colnames(object)
      if (!is.null(colnames)) coolcat("colnames(%d): %s\n", colnames)
      else cat("colnames: NULL\n")

      coolcat("colData names(%d): %s\n", names(colData(object)))
    }

    print_summary_summarized_experiment(x)

    # --- 2) TIDY SUMMARIZED EXPERIMENT PRINTING ---
  } else if (design == "tidySummarizedExperiment") {

    print_tidy_summarized_experiment <- function(x) {
      # Fix NOTEs
      . <- NULL

      # Stop if any column or row names are duplicated
      if (check_if_any_dimnames_duplicated(x, dim = "cols")) {
        stop("tidySummarizedExperiment says: some column names are duplicated")
      }
      if (check_if_any_dimnames_duplicated(x, dim = "rows")) {
        stop("tidySummarizedExperiment says: some row names are duplicated")
      }

      # Stop if column names of assays do not overlap
      if (check_if_assays_are_NOT_overlapped(x, dim = "cols")) {
        stop("tidySummarizedExperiment says: the assays in your SummarizedExperiment have column names,
but they do not completely overlap.")
      }
      if (check_if_assays_are_NOT_overlapped(x, dim = "rows")) {
        stop("tidySummarizedExperiment says: the assays in your SummarizedExperiment have row names,
but they do not completely overlap.")
      }

      # Reorder assays before printing
      x <- order_assays_internally_to_be_consistent(x)

      tmp_x <- x
      if (nrow(tmp_x) > 30) {
        tmp_x <- tmp_x[seq_len(min(50, nrow(x))), min(1, ncol(x)), drop = FALSE]
      } else if (ncol(tmp_x) == 0) {
        tmp_x <- tmp_x
      } else {
        tmp_x <- tmp_x[, seq_len(min(20, ncol(x))), drop = FALSE]
      }
      my_tibble <- tmp_x %>% as_tibble()

      my_tibble |>
        vctrs::new_data_frame(class=c("tidySummarizedExperiment", "tbl")) %>%
        add_attr(nrow(x),  "number_of_features") %>%
        add_attr(ncol(x),  "number_of_samples") %>%
        add_attr(assays(x) %>% names, "assay_names") %>%
        add_attr(
          sprintf(
            "%s %s %s",
            x %>% dim %>% {(.)[1] * (.)[2]} %>%
              format(format="f", big.mark=",", digits=1),
            cli::symbol$times,
            ncol(my_tibble)
          ) %>%
            setNames("A SummarizedExperiment-tibble abstraction"),
          "named_header"
        ) %>%
        print()
      invisible(x)
    }

    print_tidy_summarized_experiment(x)

    # --- 3) plyxp-STYLE PRINTING ---
  } else if (design == "plyxp"){

    print_plyxp_summarized_experiment <- function(x, n = n_print , ...) {
      top_n <- ceiling(n / 2)
      bot_n <- floor(n / 2)
      onr <- nr <- nrow(x)
      row_slice <- if (nr < 2 * n) {
        seq_len(nr)
      } else {
        c(seq_len(n), (nr - n + 1):nr)
      }

      onc <- nc <- ncol(x)
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
        list(
          .features = vctrs::vec_rep(.features, times = nc),
          .samples  = vctrs::vec_rep_each(.samples, times = nr)
        ),
        list(`|` = sep_(nn)),
        assays_,
        list(`|` = sep_(nn)),
        row_,
        list(`|` = sep_(nn)),
        col_
      )
      attr(out, "row.names") <- c(NA_integer_, -nn)
      class(out) <- c("SE_abstraction", "tbl_df", "tbl", "data.frame")

      sub_seq <- if (nn < 2 * top_n) {
        seq_len(nn)
      } else {
        c(seq_len(top_n), (nn - bot_n + 1):nn)
      }
      out_sub <- out[sub_seq, ]

      if (nrow(out_sub) == nn) {
        attr(out_sub, "plyxp:::has_break_at") <- 0L
      } else {
        attr(out_sub, "plyxp:::has_break_at") <- max(top_n)
      }

      attr(out_sub, "plyxp:::data") <- x
      print(out_sub, n = n, ...)
      invisible(x)
    }

    print_plyxp_summarized_experiment(x, ...)
    invisible(x)

  # tidyprint_1: SE_print_abstraction
  }  else if (design == "tidyprint_1"){

    print_tidyprint_1 <- function(x, n = n_print , ...){
      
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
        list(
          .feature = vctrs::vec_rep(.features, times = nc),
          .sample  = vctrs::vec_rep_each(.samples, times = nr)
        ),
        list(`|` = sep_(nn)),
        assays_,
        list(`|` = sep_(nn)),
        col_,
        list(`|` = sep_(nn)),
        row_
      )
      attr(out, "row.names") <- c(NA_integer_, -nn)
      class(out) <- c("SE_abstraction", "tbl_df", "tbl", "data.frame")

      sub_seq <- if (nn < 2 * top_n) {
        seq_len(nn)
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

      # attr(out_sub, "n") <- n
      # attr(out_sub, "total_rows") <- x %>% dim %>% {(.)[1] * (.)[2]}

      # class(out_sub) <- c("SE_print_abstraction", "tbl_df", "tbl", "data.frame")

      out_sub = out_sub %>%
        vctrs::new_data_frame(class=c('SE_print_abstraction', "tbl_df", "tbl", "data.frame")) %>%
        add_attr(n, 'n_print') %>%
        add_attr(onc*onr, 'total_rows') %>%
        add_attr(nrow(x),  "number_of_features") %>%
        add_attr(ncol(x),  "number_of_samples") %>%
        add_attr(assays(x) %>% names, "assay_names") %>%
        add_attr(separator_row, "separator_row") |>
        add_attr(names(col_), "covariate_names") |>

        # add_attr(
        #   # Get the actual column names that will be printed on screen
        #   # This uses tibble's internal method to determine visible columns
        #   pillar::tbl_format_setup(out_sub, width = getOption("width", 80) + 4)$body[1] |> as.character(),
        #   "printed_colnames"
        # ) %>%
        add_attr(
          '' %>%
            setNames("A SummarizedExperiment-tibble abstraction"),
          "named_header"
        )

      # print(attributes(out_sub))

      out_sub %>% print()
      invisible(x)
    }


    print_tidyprint_1(x, ...)
    invisible(x)

  } else {
    stop("Invalid design argument.")
  }
}

# Assign the new print function to SummarizedExperiment objects
setMethod(
  f = "show",
  signature = "SummarizedExperiment",
  definition = function(object) {
    print.SummarizedExperiment(object)
  }
)
