
as_vec <- function(x) {
  # tidySummarizedExperiment can expose assays as data.frame columns;
  # coerce to matrix first so as.vector() returns cell-wise values.
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }
  x <- as.vector(x)
  # unintuitively, a matrix of lists does NOT
  # become a vector after `as.vector()`,
  # must remove dim
  dim(x) <- NULL
  x
}

maybe_phantom <- function(x) {
  if (isS4(x)) return(vec_phantom(x))
  x
}

vec_phantom <- function(x) {
  vctrs::new_vctr(
    seq_len(length.out = length(x)),
    phantomData = x,
    class = "vec_phantom"
  )
}

sep_ <- function(n) {
  x <- vctrs::vec_rep("|", times = n)
  class(x) <- "|"
  x
}
