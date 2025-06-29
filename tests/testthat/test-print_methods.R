library(testthat)
library(dplyr)
library(tidyr)

library(tidyprint)

# Example SummarizedExperiment data from the airway package

data(se_airway)

# test for default design
test_that("Default SummarizedExperiment print works", {
  expect_output(print(se_airway), "A SummarizedExperiment-tibble abstraction:")
})

test_that("Default SummarizedExperiment print works", {
  expect_output(print(se_airway, design = 1), "class: SummarizedExperiment")
})

test_that("Default SummarizedExperiment print works", {
  expect_output(print(se_airway, design = "SummarizedExperiment"), "class: SummarizedExperiment")
})



# test for design 2
test_that("tidySummarizedExperiment print works", {
  expect_output(print(se_airway, design = "tidySummarizedExperiment"), "A SummarizedExperiment-tibble abstraction:")
})

test_that("tidySummarizedExperiment print works", {
  expect_output(print(se_airway, design = 2), "A SummarizedExperiment-tibble abstraction:")
})


# test for design 3
test_that("plyxp print works", {
  expect_output(print(se_airway, design = "plyxp"), "A tibble:")
})

test_that("plyxp print works", {
  expect_output(print(se_airway, design = 3), "A tibble:")
})

# test for design 4
test_that("tidyprint_1 print works", {
  expect_output(print(se_airway, design = "tidyprint_1"), "A SummarizedExperiment-tibble abstraction:")
})

test_that("tidyprint_1 print works", {
  expect_output(print(se_airway, design = 4), "A SummarizedExperiment-tibble abstraction:")
})

# test for error
test_that("Invalid design throws an error", {
  expect_error(print(se_airway, design = "invalidDesign"), "should be one of")
})

# test for message
test_that("tidy_message works correctly", {
  expect_message(tidy_message("Test info message"), "says: Test info message")
})
