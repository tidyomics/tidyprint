# Split colData around a declared sample identifier
#
# Columns constant within the sample identifier describe the samples, the others
# describe the cells. Cost is one hashing pass per column over the cells.
#
# @param col_data `colData(x)`, or any table-like object of covariates.
# @param sample_id Name of the column holding the sample identifier.
# @return A list with the column names split into `sample`, the identifier and
#   the covariates constant within it, and `cell`.
# @noRd
split_col_data <- function(col_data, sample_id) {
  columns <- as.list(col_data)
  sample_codes <- group_codes(columns[[sample_id]])

  others <- setdiff(names(columns), sample_id)
  is_sample_wise <- vapply(
    others,
    function(other) {
      determines(sample_codes, attr(sample_codes, "n"), group_codes(columns[[other]]))
    },
    logical(1)
  )

  list(
    sample = c(sample_id, others[is_sample_wise]),
    cell = others[!is_sample_wise]
  )
}

# Integer group codes of a covariate, carrying the number of groups as attribute
# "n". Values that vctrs cannot hash directly, such as Rle, go through character.
group_codes <- function(x) {
  if (!vctrs::vec_is(x)) x <- as.character(x)
  vctrs::vec_group_id(x)
}

# Is `codes` constant within each group of `group`? Writing the codes into a
# vector indexed by group keeps one value per group; the dependency holds when
# every element agrees with the one kept for its group.
determines <- function(group, number_of_groups, codes) {
  kept <- integer(number_of_groups)
  kept[group] <- codes
  all(kept[group] == codes)
}
