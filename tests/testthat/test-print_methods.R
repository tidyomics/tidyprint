library(testthat)
library(dplyr)
library(tidyr)

library(tidyprint)

# Example SummarizedExperiment data from the airway package

data(se_airway)

# test for default print
test_that("Default print works", {
  expect_output(print(se_airway), "A SummarizedExperiment-tibble abstraction:")
})

test_that("Print works with n", {
  expect_output(print(se_airway, n = 5), "A SummarizedExperiment-tibble abstraction:")
})

# test for message
test_that("tidy_message works correctly", {
  expect_message(tidy_message("Test info message"), "says: Test info message")
})

# test for pasilla (regression test for subsetting error)
# Previously triggered with 7-sample data:
# "Can't subset elements past the end. Locations 136, 137, 138, 139, and 140 don't exist. There are only 7 elements."
test_that("pasilla prints without subsetting error", {
  tidy_print_on()
  expect_no_error(tidySummarizedExperiment::pasilla |> show())
})
