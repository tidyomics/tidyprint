#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#' @importFrom methods setMethod


# This file is a replacement of the unexported functions in the tibble
# package, in order to specify "tibble abstraction in the header"

#' @importFrom rlang names2
#' @importFrom pillar align
#' @importFrom pillar get_extent
#' @importFrom pillar style_subtle
#' @importFrom pillar tbl_format_header
#' @importFrom cli col_br_black
#' @importFrom tibble as_tibble
#' @export
tbl_format_header.tidySummarizedExperiment <- function(x, setup, ...) {

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


#' @importFrom vctrs new_data_frame vec_rep vec_rep_each
#' @importFrom SummarizedExperiment assayNames assay
#' @importFrom stats setNames
#' @export
print.SummarizedExperiment <- function(x, design = 1, n_print = 10, ...) {

  # Match the user-supplied design argument to one of the valid choices:
  if (is.numeric(design)) {
    # Allowed numeric -> corresponding design
    design_map <- c("SummarizedExperiment", "tidySummarizedExperiment", "plyrxp")

    # Validate numeric input
    if (!design %in% 1:3) {
      stop("Invalid numeric design argument. Choose 1, 2, or 3.")
    }
    design <- design_map[design]
  }

  design <- match.arg(design, c("SummarizedExperiment", "tidySummarizedExperiment", "plyrxp"))

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

      my_tibble <- x |>
        # If more than 30 genes, select first sample
        when(
          nrow(.) > 30 ~.[1:min(50, nrow(x)), min(1, ncol(x)), drop=FALSE] ,
          ncol(.) == 0 ~ .,
          ~ .[, 1:min(20, ncol(x)), drop=FALSE]
        ) %>%
        as_tibble()
      # browser()
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

    # --- 3) PLYRXP-STYLE PRINTING ---
  } else if (design == "plyrxp"){

    print_plyrxp_summarized_experiment <- function(x, n = n_print , ...) {
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
        list(sep_(nn)),
        assays_,
        list(sep_(nn)),
        row_,
        list(sep_(nn)),
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

    print_plyrxp_summarized_experiment(x, ...)
    invisible(x)
  }  else {
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
