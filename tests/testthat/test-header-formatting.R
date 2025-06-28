library(testthat)
library(purrr)
library(stringr)
library(dplyr)
library(tidyprint)

context("format_covariate_header")

separator_row <- c(
  ".feature" = "               ",
  ".sample" = "          ",
  "|" = "              ",
  "counts" = "          ",
  "|" = "              ",
  "dex" = "-------",
  "celltype" = "--------",
  "geo_id" = "----------",
  "sample_id" = "----------",
  "sample_id2" = "----------",
  "|" = "              ",
  "gene_name" = "               "
)

printed_colnames <- c(
  ".feature", ".sample", "|", "counts", "|", 
  "dex", "celltype", "geo_id", "sample_id", "sample_id2",
  "|", "gene_name"
)

covariate_names <- c("dex", "celltype", "geo_id", "sample_id", "sample_id2")
number_of_total_rows <- 1000

test_that("format_covariate_header overlays label as a continuous string", {
  result <- tidyprint:::format_covariate_header(
    separator_row = separator_row,
    printed_colnames = printed_colnames,
    covariate_names = covariate_names,
    number_of_total_rows = number_of_total_rows,
    label = "COVARIATES"
  )
  expect_snapshot_output(cat(result, "\n"))

  result2 <- tidyprint:::format_covariate_header(
    separator_row = separator_row,
    printed_colnames = printed_colnames,
    covariate_names = covariate_names,
    number_of_total_rows = number_of_total_rows,
    label = "COVAR"
  )
  expect_snapshot_output(cat(result2, "\n"))

  result3 <- tidyprint:::format_covariate_header(
    separator_row = separator_row,
    printed_colnames = printed_colnames,
    covariate_names = covariate_names,
    number_of_total_rows = number_of_total_rows,
    label = "COVARIATES_LONG"
  )
  expect_snapshot_output(cat(result3, "\n"))
}) 