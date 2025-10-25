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
