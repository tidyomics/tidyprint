
#' @importFrom purrr reduce
#' @importFrom purrr map map2
#' @importFrom tidyr spread
#' @importFrom tibble enframe
#' @importFrom SummarizedExperiment colData
#' @importFrom pkgconfig get_config
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

  .subset <- enquo(.subset)

  sample_info <-
    colData(x) %>%

    # If reserved column names are present add .x
    change_reserved_column_names(x) %>%

    # Convert to tibble
    tibble::as_tibble(rownames=s_(x)$name) %>%
    setNames(c(s_(x)$name, colnames(colData(x))))

  range_info <-
    skip_GRanges %>%
    when(
      (.) ~ tibble() %>% list,
      ~  get_special_datasets(x)
    ) %>%
    reduce(left_join, by="coordinate")

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
    when(nrow(range_info) > 0 ~
           (.) %>% left_join(range_info) %>% suppressMessages(),
         ~ (.))

  # This function outputs a tibble after subsetting the columns
  else subset_tibble_output(x, count_info, sample_info,
                            gene_info, range_info, !!.subset)
}
