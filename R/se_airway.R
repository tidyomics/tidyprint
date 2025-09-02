#' Example SummarizedExperiment dataset
#'
#' Scaled RNA-seq counts for airway samples packaged as a
#' \link[SummarizedExperiment]{SummarizedExperiment}.
#'
#' @docType data
#' @name se_airway
#' @usage data(se_airway)
#' @format A SummarizedExperiment object with:
#' \describe{
#'   \item{assays}{A matrix of scaled counts}
#'   \item{colData}{Sample metadata (cell type, treatment, etc.)}
#' }
#' @keywords datasets
#' @source airway package
"se_airway"
