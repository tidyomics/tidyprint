library(testthat)
library(tidyprint)
library(SummarizedExperiment)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(magrittr)
example(SummarizedExperiment)

# test for default design
test_that("Default SummarizedExperiment print works", {
  expect_output(print(se0), "class: SummarizedExperiment")
})

test_that("Default SummarizedExperiment print works", {
  expect_output(print(se0, design = 1), "class: SummarizedExperiment")
})

test_that("Default SummarizedExperiment print works", {
  expect_output(print(se0, design = "SummarizedExperiment"), "class: SummarizedExperiment")
})



# test for design 2
test_that("tidySummarizedExperiment print works", {
  expect_output(print(se0, design = "tidySummarizedExperiment"), "A SummarizedExperiment-tibble abstraction:")
})

test_that("tidySummarizedExperiment print works", {
  expect_output(print(se0, design = 2), "A SummarizedExperiment-tibble abstraction:")
})


# test for design 3
test_that("plyxp print works", {
  expect_output(print(se0, design = "plyxp"), "A tibble:")
})

test_that("plyxp print works", {
  expect_output(print(se0, design = 3), "A tibble:")
})


# test for error
test_that("Invalid design throws an error", {
  expect_error(print(se0, design = "invalidDesign"), "should be one of")
})
