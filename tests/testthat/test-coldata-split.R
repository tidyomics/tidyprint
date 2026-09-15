library(testthat)
library(tidyprint)

test_that("columns constant within the sample identifier describe the samples", {
  col_data <- S4Vectors::DataFrame(
    barcode = paste0("cell", 1:6),
    cluster = rep(c("c1", "c2"), 3),
    sample_id = rep(c("S1", "S2", "S3"), each = 2),
    treatment = rep(c("trt", "untrt"), c(4, 2)),
    project = "GSE1"
  )

  split <- tidyprint:::split_col_data(col_data, "sample_id")

  expect_equal(split$sample, c("sample_id", "treatment", "project"))
  expect_equal(split$cell, c("barcode", "cluster"))
})

test_that("a coarser factor is sample-wise, a crossed one is not", {
  col_data <- data.frame(
    sample_id = rep(sprintf("S%s", 1:4), each = 2),
    batch = rep(c("b1", "b2"), each = 4),
    cluster = rep(c("c1", "c2"), 4)
  )

  split <- tidyprint:::split_col_data(col_data, "sample_id")

  expect_equal(split$sample, c("sample_id", "batch"))
  expect_equal(split$cell, "cluster")
})

test_that("the split works on colData holding an Rle", {
  col_data <- S4Vectors::DataFrame(
    sample_id = rep(c("S1", "S2"), each = 2),
    tissue = S4Vectors::Rle("lung", 4)
  )

  split <- tidyprint:::split_col_data(col_data, "sample_id")

  expect_equal(split$sample, c("sample_id", "tissue"))
  expect_equal(split$cell, character())
})
