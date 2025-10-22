#' Convert SummarizedExperiment to tibble
#'
#' Converts a \link[SummarizedExperiment]{SummarizedExperiment} object to a tibble
#' format, combining assay data with sample and feature metadata in a long format
#' suitable for tidyverse workflows.
#'
#' @param x A \code{SummarizedExperiment} object to convert.
#' @param ... Additional arguments passed to internal conversion functions.
#' @param .name_repair Treatment of problematic column names. See
#'   \code{\link[tibble]{as_tibble}} for details.
#' @param rownames Either \code{NULL} or a string giving the name of a column
#'   to use as rownames. See \code{\link[tibble]{as_tibble}} for details.
#'
#' @return A tibble containing the assay data combined with sample and feature
#'   metadata. The structure includes:
#'   \itemize{
#'     \item Feature identifiers (from \code{rownames} or \code{rowData})
#'     \item Sample identifiers (from \code{colnames} or \code{colData})
#'     \item Assay values (one column per assay)
#'     \item Sample metadata (from \code{colData})
#'     \item Feature metadata (from \code{rowData})
#'   }
#'
#' @details
#' This method provides a bridge between Bioconductor's SummarizedExperiment
#' objects and tidyverse data manipulation workflows. The conversion creates
#' a long-format tibble where each row represents a feature-sample combination,
#' making it suitable for filtering, grouping, and other tidyverse operations.
#'
#' @seealso \link[SummarizedExperiment]{SummarizedExperiment}, \link[tibble]{as_tibble}
#'
#' @examples
#' \dontrun{
#'   library(tidyprint)
#'   data(se_airway)
#'   as_tibble(se_airway)
#' }
#'
#' @importFrom purrr reduce
#' @importFrom purrr map map2
#' @importFrom tidyr spread
#' @importFrom tibble enframe
#' @importFrom SummarizedExperiment colData
#' @importFrom pkgconfig get_config
#' @importFrom rlang enquo
#' @importFrom dplyr left_join
#' @export
as_tibble.SummarizedExperiment <- function(x, ...,
                                           .name_repair=c("check_unique", "unique", "universal", "minimal"),
                                           rownames=pkgconfig::get_config("tibble::rownames", NULL)) {

  .as_tibble_optimised(x = x, ...,
                       .name_repair=.name_repair, rownames=rownames)

}

.as_tibble_optimised <- function(x, skip_GRanges=FALSE, .subset=NULL,
                                 .name_repair=c("check_unique", "unique", "universal", "minimal"),
                                 rownames=pkgconfig::get_config("tibble::rownames", NULL)) {

  .subset <- rlang::enquo(.subset)

  sample_info <-
    colData(x) %>%

    # If reserved column names are present add .x
    change_reserved_column_names(x) %>%

    # Convert to tibble
    tibble::as_tibble(rownames=s_(x)$name) %>%
    setNames(c(s_(x)$name, colnames(colData(x))))

  range_info <-
    {
      if (isTRUE(skip_GRanges)) {
        list(tibble())
      } else {
        get_special_datasets(x)
      }
    } %>%
    reduce(left_join, by = "coordinate")

  gene_info <-
    rowData(x) %>%

    # If reserved column names are present add .x
    change_reserved_column_names(x)%>%

    # Convert to tibble
    tibble::as_tibble(rownames=f_(x)$name) %>%
    setNames(c(f_(x)$name, colnames(rowData(x))))

  count_info <- get_count_datasets(x)

  # Return
  if ( rlang::quo_is_null(.subset))

    # If I want to return all columns
    count_info %>%
    full_join(sample_info, by=s_(x)$name) %>%
    full_join(gene_info, by=f_(x)$name) %>%
    {
      if (nrow(range_info) > 0) {
        suppressMessages(left_join(., range_info))
      } else {
        .
      }
    }

  # This function outputs a tibble after subsetting the columns
  else subset_tibble_output(x, count_info, sample_info,
                            gene_info, range_info, !!.subset)
}
