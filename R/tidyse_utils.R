#' @importFrom tidyr nest
#' @importFrom dplyr select
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr full_join
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

check_if_assays_are_NOT_overlapped <- function(se, dim = "cols") {

  stopifnot(dim %in% c("rows", "cols"))
  if (dim == "rows") {
    dimnames_function <- rownames
    length_function <- nrow
  } else {
    dimnames_function <- colnames
    length_function <- ncol
  }
  is_identical_for_reduce <- function(x,y) if (identical(x,y)) x else FALSE

  # If I have any assay at all
  assays(se) |> length() |> gt(0) &&

    # If I have at least one assay with dimnames
    Filter(
      Negate(is.null),
      assays(se, withDimnames = FALSE) |>
        as.list() |>
        map(dimnames_function)
    ) |>
    length() |>
    gt(0) &&

    # If I have lack of consistency
    # This will be TRUE also if some assays have dimnames and other don't
    # For each assay, sort the dimnames, then check that they are all the
    # same. Can't check for unique length, since some names may be repeated
    # If they're not all the same, the reduce() step will return FALSE;
    # otherwise, returns the (shared) dimnames
    assays(se, withDimnames = FALSE) |>
    as.list() |>
    map(dimnames_function) |>
    map(sort) |>
    reduce(is_identical_for_reduce) |>
    is.logical()

}

order_assays_internally_to_be_consistent <- function(se) {
  se <- check_se_dimnames(se)
  se |>
    assays(withDimnames = FALSE) =
    map2(
      assays(se, withDimnames = FALSE) %>% as.list(),
      names(assays(se)),
      ~ {
        if (!is.null(rownames(se)) && !is.null(rownames(.x)) &&
            all(rownames(se) %in% rownames(.x))) {
          .x = .x[rownames(se), , drop = FALSE]
        }
        if (!is.null(colnames(se)) && !is.null(colnames(.x)) &&
            all(colnames(se) %in% colnames(.x))) {
          .x = .x[, colnames(se), drop = FALSE]
        }
        .x

      })

  se
}

check_se_dimnames <- function(se) {

  # Stop if any column or row names are duplicated
  if (check_if_any_dimnames_duplicated(se, dim = "cols")) {
    stop("tidySummarizedExperiment says: some column names are duplicated")
  }
  if (check_if_any_dimnames_duplicated(se, dim = "rows")) {
    stop("tidySummarizedExperiment says: some row names are duplicated")
  }

  # Stop if column names of assays do not overlap, or if some assays have
  # column names and others don't
  if (check_if_assays_are_NOT_overlapped(se, dim = "cols")) {
    warning(
      "tidySummarizedExperiment says: at least one of the assays in your SummarizedExperiment have column names, but they don't completely overlap between assays. It is strongly recommended to make the assays consistent, to avoid erroneous matching of samples."
    )
  }
  # Same for row names
  if (check_if_assays_are_NOT_overlapped(se, dim = "rows")) {
    warning(
      "tidySummarizedExperiment says: at least one of the assays in your SummarizedExperiment have row names, but they don't completely overlap between assays. It is strongly recommended to make the assays consistent, to avoid erroneous matching of features."
    )
  }

  # If the assays have dimnames but the SE does not, throw a warning and set
  # the dimnames of the SE to those of the first assay with dimnames.
  # (At this point we know that all assays have the same dimnames (could be
  # NULL), but they could be in different order)
  if (is.null(colnames(se)) &&
      length(assays(se)) > 0) {
    cn <- vapply(assays(se, withDimnames = FALSE), function(x) !is.null(colnames(x)), FALSE)
    if (any(cn)) {
      idx <- which(cn)[1]
      warning(
        "tidySummarizedExperiment says: the assays in your SummarizedExperiment have column names, but the SummarizedExperiment does not. Setting colnames(se) to column names of first assay with column names (assay ", idx, ")."
      )
      colnames(se) <- colnames(assays(se, withDimnames = FALSE)[[idx]])
    }
  }
  if (is.null(rownames(se)) &&
      length(assays(se)) > 0) {
    rn <- vapply(assays(se, withDimnames = FALSE), function(x) !is.null(rownames(x)), FALSE)
    if (any(rn)) {
      idx <- which(rn)[1]
      warning(
        "tidySummarizedExperiment says: the assays in your SummarizedExperiment have row names, but the SummarizedExperiment does not. Setting rownames(se) to row names of first assay with row names (assay ", idx, ")."
      )
      rownames(se) <- rownames(assays(se, withDimnames = FALSE)[[idx]])
    }
  }

  # If the assays as well as the SE have dimnames, but they don't overlap
  # (they may be in different order), throw an error.
  if (!is.null(colnames(se)) &&
      length(assays(se)) > 0 &&
      !is.null(colnames(assays(se, withDimnames = FALSE)[[1]])) &&
      !all(colnames(assays(se, withDimnames = FALSE)[[1]]) %in% colnames(se))) {
    warning(
      "tidySummarizedExperiment says: the assays in your SummarizedExperiment have column names, but they don't agree with the column names of the SummarizedExperiment object itself. It is strongly recommended to make the assays consistent, to avoid erroneous matching of samples."
    )

  }

  if (is.null(rownames(se)) &&
      length(assays(se)) > 0) {
    rn <- vapply(assays(se, withDimnames = FALSE), function(x) !is.null(rownames(x)), FALSE)
    if (any(rn)) {
      idx <- which(rn)[1]
      warning(
        "tidySummarizedExperiment says: the assays in your SummarizedExperiment have row names, but the SummarizedExperiment does not. Setting rownames(se) to row names of first assay with row names (assay ", idx, ")."
      )
      rownames(se) <- rownames(assays(se, withDimnames = FALSE)[[idx]])
    }
  }

  # If the assays as well as the SE have dimnames, but they don't overlap
  # (they may be in different order), throw an error.
  if (!is.null(colnames(se)) &&
      length(assays(se)) > 0 &&
      !is.null(colnames(assays(se, withDimnames = FALSE)[[1]])) &&
      !all(colnames(assays(se, withDimnames = FALSE)[[1]]) %in% colnames(se))) {
    warning(
      "tidySummarizedExperiment says: the assays in your SummarizedExperiment have column names, but they don't agree with the column names of the SummarizedExperiment object itself. It is strongly recommended to make the assays consistent, to avoid erroneous matching of samples."
    )
  }
  if (!is.null(rownames(se)) &&
      length(assays(se)) > 0 &&
      !is.null(rownames(assays(se, withDimnames = FALSE)[[1]])) &&
      !all(rownames(assays(se, withDimnames = FALSE)[[1]]) %in% rownames(se))) {
    warning(
      "tidySummarizedExperiment says: the assays in your SummarizedExperiment have row names, but they don't agree with the row names of the SummarizedExperiment object itself. It is strongly recommended to make the assays consistent, to avoid erroneous matching of features."
    )
  }

  se
}

check_if_any_dimnames_duplicated <- function(se, dim = "cols") {
  stopifnot(dim %in% c("rows", "cols"))
  if (dim == "rows") {
    dimnames_function <- rownames
    nbr_unique_dimnames_function <- function(x) length(unique(rownames(x)))
    length_function <- nrow
  } else {
    dimnames_function <- colnames
    nbr_unique_dimnames_function <- function(x) length(unique(colnames(x)))
    length_function <- ncol
  }

  # Check assays
  # If I have any assay at all
  assays_check <- assays(se) |> length() |> gt(0) &&

    # If I have at least one assay with dimnames
    Filter(
      Negate(is.null),
      assays(se, withDimnames = FALSE) |>
        as.list() |>
        map(dimnames_function)
    ) |>
    length() |>
    gt(0) &&

    # If any named assay have fewer unique names than expected
    assays(se, withDimnames = FALSE) |>
    as.list() |>
    map(dimnames_function) |>
    Filter(Negate(is.null), x = _) |>
    map(unique) |>
    map(length) |>
    reduce(min) |>
    magrittr::equals(length_function(se)) |>
    not()

  # Check SE object
  se_check <- !is.null(dimnames_function(se)) &&
    nbr_unique_dimnames_function(se) != length_function(se)

  # Return TRUE if either of the two checks return TRUE
  assays_check || se_check
}

# Greater than
gt <- function(a, b) {
  a > b
}

# Negation
not <- function(is) {
  !is
}

add_attr <- function(var, attribute, name) {
  attr(var, name) <- attribute
  var
}

eliminate_GRanges_metadata_columns_also_present_in_Rowdata <- function(.my_data, se) {
  .my_data %>%
    select(-one_of(colnames(rowData(se)))) %>%
    
    # In case there is not metadata column
    suppressWarnings() 
}

get_special_datasets <- function(se) {

  rr =  se %>%
    rowRanges()

  rr %>%
    when(

      # If no ranges
      as.data.frame(.) %>%
        nrow() %>%
        magrittr::equals(0) ~ tibble(),

      # If it is a range list (multiple rows per feature)
      is(., "CompressedGRangesList") ~ {

        # If GRanges does not have row names
        if (is.null(rr@partitioning@NAMES)) {
          rr@partitioning@NAMES <- as.character(1:nrow(se))
        }

        tibble::as_tibble(rr) %>%
          eliminate_GRanges_metadata_columns_also_present_in_Rowdata(se) %>%
          nest(GRangesList = -group_name) %>%
          dplyr::rename(!!f_(se)$symbol := group_name)

      },

      # If standard GRanges (one feature per line)
      ~ {

        # If GRanges does not have row names
        if (is.null(rr@ranges@NAMES)) {
          rr@ranges@NAMES <- as.character(1:nrow(se))
        }

        tibble::as_tibble(rr) %>%
          eliminate_GRanges_metadata_columns_also_present_in_Rowdata(se) %>%
          mutate(!!f_(se)$symbol := rr@ranges@NAMES)
      }

    ) %>%
    list()
}

change_reserved_column_names <- function(col_data, .data) {

  # Fix  NOTEs
  . = NULL

  col_data %>%

    setNames(
      colnames(.) %>%
        sapply(function(x) if (x == f_(.data)$name) sprintf("%s.x", f_(.data)$name) else x) %>%
        sapply(function(x) if (x == s_(.data)$name) sprintf("%s.x", s_(.data)$name) else x) %>%
        str_replace("^coordinate$", "coordinate.x")
    )

}

get_special_column_name_symbol <- function(name) {
  list(name = name, symbol = as.symbol(name))
}

feature__ <- get_special_column_name_symbol(".feature")
sample__ <- get_special_column_name_symbol(".sample")

f_ <- function(x) {
  # Check if old deprecated columns are used
  if ("feature__" %in% names(metadata(x))) feature__ = metadata(x)$feature__
  return(feature__)
}

#' @importFrom S4Vectors metadata
s_ <- function(x) {
  if ("sample__" %in% names(metadata(x))) sample__ = metadata(x)$sample__
  return(sample__)
}

get_count_datasets <- function(se) {
  # Check that dimnames are consistent
  se <- check_se_dimnames(se)

  # Join assays
  list_assays =
    map2(
      assays(se, withDimnames = FALSE) %>% as.list(),
      names(assays(se)),
      ~ {

        # If the counts are in a sparse matrix convert to a matrix
        # This might happen because the user loaded tidySummarizedExperiment and is
        # print a SingleCellExperiment
        if (is(.x, "dgCMatrix") | is(.x, "DelayedArray")) {
          .x <- as.matrix(.x)
        }

        # Rearrange if assays has colnames and rownames
        if (!is.null(rownames(se)) && !is.null(rownames(.x)) &&
            all(rownames(se) %in% rownames(.x))) {
          .x = .x[rownames(se), , drop = FALSE]
        }
        if (!is.null(colnames(se)) && !is.null(colnames(.x)) &&
            all(colnames(se) %in% colnames(.x))) {
          .x = .x[, colnames(se), drop = FALSE]
        }

        # If I don't have assay colnames and rownames add them
        if (!is.null(rownames(se)) && is.null(rownames(.x))) rownames(.x) = rownames(se)
        if (!is.null(colnames(se)) && is.null(colnames(.x))) colnames(.x) = colnames(se)

        .x =
          .x %>%
          # matrix() %>%
          # as.data.frame() %>%

          # In case I have a sparse matrix
          as.matrix() |>
          tibble::as_tibble(rownames = f_(se)$name, .name_repair = "minimal") %>%

          # If the matrix does not have sample names, fix column names
          when(colnames(.x) %>% is.null() & is.null(colnames(se)) ~ setNames(., c(
            f_(se)$name,  seq_len(ncol(.x))
          )),
          ~ (.)
          )

        # Avoid dug if SE if completely empty, no rows, no columns
        if(.x |> select(-!!f_(se)$symbol) |> ncol() == 0) return(.x)

        .x |>
          pivot_longer(names_to = s_(se)$name, values_to = .y, cols = -!!f_(se)$symbol, cols_vary = "slowest")

        #%>%
        #  rename(!!.y := count)
      }) %>%

    # Add dummy sample or feature if we have empty assay.
    # This is needed for a correct visualisation of the tibble form
    map(~when(
      f_(se)$name %in% colnames(.x) %>% not ~ mutate(.x, !!f_(se)$symbol := as.character(NA)),
      s_(se)$name %in% colnames(.x) %>% not ~ mutate(.x, !!s_(se)$symbol := as.character(NA)),
      ~ .x
    ))

  # If assays is non empty
  if(list_assays |> length() > 0)
    list_assays |>
    reduce(full_join, by = c(f_(se)$name, s_(se)$name))

  # If assays is empty
  else {

    # If I don't have row column names
    if(se |> rownames() |> is.null()) rn = nrow(se) |> seq_len() |> as.character()
    else rn = rownames(se)
    if(se |> colnames() |> is.null()) cn = ncol(se) |> seq_len() |> as.character()
    else cn = colnames(se)

    expand.grid(  rn, cn  ) |>
      setNames(c(f_(se)$name, s_(se)$name)) |>
      tibble::as_tibble()
  }


}
