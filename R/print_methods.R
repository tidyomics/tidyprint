#' Print method for SummarizedExperiment with tidyprint styles
#'
#' Provides a compact tibble-like display for a \link[SummarizedExperiment]{SummarizedExperiment}
#' object that preserves assay values and key covariates with a separator band when
#' the table is truncated.
#'
#' @param x A \code{SummarizedExperiment} object to print.
#' @param n_print Integer (default \code{10}). Approximate number of rows to show
#'   in the display. When the total cells shown are fewer than \code{n_print}, the
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
#' @return \code{x} is returned \emph{invisibly} after printing.
#'
#' @seealso \link[SummarizedExperiment]{SummarizedExperiment}, \link[tibble]{as_tibble}
#'
#' @examples
#' \dontrun{
#'   library(tidyprint)
#'   print(se_airway)                         # compact tibble display
#'   print(se_airway, n_print = 20)           # with custom row count
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
print.SummarizedExperiment <- function(x, n_print = 10, ...) {

  # SE_print_abstraction
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
      
      out_sub %>% print(n = ifelse(separator_row_flag, n+1, n), ...)
      invisible(x)
    }


  print_tidyprint_1(x, ...)
  invisible(x)
}

# Assign the new print function to SummarizedExperiment objects
setMethod(
  f = "show",
  signature = "SummarizedExperiment",
  definition = function(object) {
    print.SummarizedExperiment(object)
  }
)
