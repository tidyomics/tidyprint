library(testthat)
library(tidyprint)

sce_example <- function(col_data, sample_id = NULL) {
  counts <- matrix(
    seq_len(5 * nrow(col_data)),
    nrow = 5,
    dimnames = list(paste0("gene", 1:5), paste0("cell", seq_len(nrow(col_data))))
  )
  SingleCellExperiment::SingleCellExperiment(
    assays = list(counts = counts),
    colData = col_data,
    metadata = if (is.null(sample_id)) list() else list(tidyomics_sample_id = sample_id)
  )
}

sce <- sce_example(
  S4Vectors::DataFrame(
    n_count = c(11, 22, 33, 44, 55, 66),
    cluster = rep(c("c1", "c2"), 3),
    sample_id = rep(c("S1", "S2", "S3"), each = 2),
    treatment = rep(c("untrt", "trt"), c(2, 4))
  ),
  sample_id = "sample_id"
)

test_that("the SummarizedExperiment layout knows nothing about cells", {
  data(se_airway)
  layout <- tidyprint:::tidyprint_layout(se_airway)

  expect_equal(layout$column_name, ".sample")
  expect_null(layout$cell_columns)
  expect_equal(names(layout$header), c("Features", "Samples", "Assays"))
  expect_equal(layout$header$Samples, ncol(se_airway))
})

test_that("the SingleCellExperiment layout extends it with cells", {
  layout <- tidyprint:::tidyprint_layout(sce)

  expect_equal(layout$column_name, ".cell")
  expect_equal(layout$cell_columns, c("n_count", "cluster"))
  expect_equal(layout$covariate_columns, c("sample_id", "treatment"))

  # cells are reported next to the features, and samples now count their groups
  expect_equal(names(layout$header), c("Features", "Cells", "Samples", "Assays"))
  expect_equal(layout$header$Cells, 6L)
  expect_equal(layout$header$Samples, 3L)
})

test_that("without a declared sample id, colData is laid out as for a SummarizedExperiment", {
  undeclared <- sce_example(S4Vectors::DataFrame(
    cluster = rep(c("c1", "c2"), 2),
    treatment = rep(c("trt", "untrt"), each = 2)
  ))

  layout <- tidyprint:::tidyprint_layout(undeclared)

  expect_equal(layout$column_name, ".cell")
  expect_null(layout$cell_columns)
  expect_equal(layout$covariate_columns, c("cluster", "treatment"))
  expect_equal(names(layout$header), c("Features", "Cells", "Assays"))
})

test_that("a sample id naming no colData column is ignored", {
  mistyped <- sce_example(
    S4Vectors::DataFrame(sample = rep(c("S1", "S2"), each = 2)),
    sample_id = "sample_id"
  )

  layout <- tidyprint:::tidyprint_layout(mistyped)

  expect_null(layout$header$Samples)
  expect_equal(layout$covariate_columns, "sample")
})

test_that("the header reports cells and samples", {
  tidy_print_on()
  expect_output(
    print(sce),
    "A SingleCellExperiment abstraction: Features=5 \\| Cells=6 \\| Samples=3"
  )
})

test_that("only sample-wise columns sit in the covariate band", {
  tidy_print_on()
  printed <- capture.output(print(sce))
  columns <- printed[grepl(".feature", printed, fixed = TRUE)]
  blocks <- strsplit(columns, "|", fixed = TRUE)[[1]]

  # assays follow the identifier as they do for a SummarizedExperiment, the band
  # holds the sample-wise columns, and the cell-wise ones come after it
  expect_match(blocks[1], "^\\s*\\.feature\\s+\\.cell\\s*$")
  expect_match(blocks[2], "counts")
  expect_match(blocks[3], "sample_id\\s+treatment")
  expect_match(blocks[4], "n_count\\s+cluster")
  expect_match(printed[grepl("COVARIATES", printed)], "COVARIATES")
})

test_that("samples are left out when no sample id is declared", {
  tidy_print_on()
  undeclared <- sce_example(S4Vectors::DataFrame(
    dex = rep(c("trt", "untrt"), each = 2),
    line = rep(c("A", "B"), 2)
  ))

  printed <- capture.output(print(undeclared))
  expect_match(printed[1], "Features=5 \\| Cells=4 \\| Assays=counts")
  expect_false(any(grepl("Samples=", printed)))
})

test_that("opting out restores the SingleCellExperiment show method", {
  tidy_print_off()
  expect_output(show(sce), "class: SingleCellExperiment")
  tidy_print_on()
})
