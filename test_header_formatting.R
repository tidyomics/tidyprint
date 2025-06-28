# Test script for header formatting function
# Load required packages
library(purrr)
library(stringr)
library(dplyr)

# Source the function directly
source("R/tidyprint_1_utlis.R")

# Mock data based on user's example
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

# Test the function
cat("=== Testing format_covariate_header function ===\n\n")

cat("Input data:\n")
cat("separator_row names:", names(separator_row), "\n")
cat("printed_colnames:", printed_colnames, "\n")
cat("covariate_names:", covariate_names, "\n")
cat("number_of_total_rows:", number_of_total_rows, "\n\n")

# Get covariate indices
covariate_indices <- which(printed_colnames %in% covariate_names)
cat("Covariate indices:", covariate_indices, "\n")

# Get covariate widths
covariate_widths <- separator_row[printed_colnames[covariate_indices]] |> purrr::map_int(nchar)
cat("Covariate widths:", covariate_widths, "\n")
cat("Total covariate width:", sum(covariate_widths), "\n\n")

# Test the function
result <- format_covariate_header(
  separator_row = separator_row,
  printed_colnames = printed_colnames,
  covariate_names = covariate_names,
  number_of_total_rows = number_of_total_rows,
  label = "COVARIATES"
)

cat("=== Result ===\n")
cat("Formatted header:\n")
cat(result, "\n\n")

# Let's also test with a shorter label
cat("=== Testing with shorter label 'COVAR' ===\n")
result2 <- format_covariate_header(
  separator_row = separator_row,
  printed_colnames = printed_colnames,
  covariate_names = covariate_names,
  number_of_total_rows = number_of_total_rows,
  label = "COVAR"
)

cat("Formatted header with 'COVAR':\n")
cat(result2, "\n\n")

# Let's also test with a longer label
cat("=== Testing with longer label 'COVARIATES_LONG' ===\n")
result3 <- format_covariate_header(
  separator_row = separator_row,
  printed_colnames = printed_colnames,
  covariate_names = covariate_names,
  number_of_total_rows = number_of_total_rows,
  label = "COVARIATES_LONG"
)

cat("Formatted header with 'COVARIATES_LONG':\n")
cat(result3, "\n\n")

# Debug: Let's see what happens step by step
cat("=== Debug: Step by step analysis ===\n")

# Build header row
header_row <- purrr::map2_chr(separator_row, names(separator_row), 
                              ~ if(.y %in% covariate_names) .x else stringr::str_replace_all(.x, "-", " "))

cat("Initial header_row:\n")
for(i in seq_along(header_row)) {
  cat(sprintf("%2d: '%s' (width: %d)\n", i, header_row[i], nchar(header_row[i])))
}

# Calculate character distribution
label <- "COVARIATES"
label_chars <- strsplit(label, "")[[1]]
total_label_chars <- length(label_chars)
total_covariate_width <- sum(covariate_widths)

cat("\nLabel analysis:\n")
cat("Label:", label, "\n")
cat("Label characters:", paste(label_chars, collapse=", "), "\n")
cat("Total label chars:", total_label_chars, "\n")

# Calculate distribution
char_distribution <- numeric(length(covariate_widths))
remaining_chars <- total_label_chars

for (i in seq_along(covariate_widths)) {
  if (remaining_chars > 0) {
    if (i == length(covariate_widths)) {
      char_distribution[i] <- remaining_chars
    } else {
      prop <- covariate_widths[i] / total_covariate_width
      chars_for_col <- max(1, round(prop * total_label_chars))
      chars_for_col <- min(chars_for_col, remaining_chars)
      char_distribution[i] <- chars_for_col
      remaining_chars <- remaining_chars - chars_for_col
    }
  }
}

cat("\nCharacter distribution:\n")
for(i in seq_along(covariate_indices)) {
  col_idx <- covariate_indices[i]
  col_name <- printed_colnames[col_idx]
  chars_to_use <- char_distribution[i]
  cat(sprintf("Column %d (%s): %d characters\n", col_idx, col_name, chars_to_use))
} 