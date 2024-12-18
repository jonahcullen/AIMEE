#' @name tissues
#' @title tissues: tissues dataset
#'
#' @description This dataset contains information about various tissue samples, including their system, tissue type, sample information, breed, and post_counts.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{tissue_sample}{Name of the tissue and sample (character).}
#'   \item{system}{Tissue system associated with the tissue (factor).}
#'   \item{tissue}{Specific tissue from which the sample was taken (factor).}
#'   \item{abvs}{Abbreviation for the tissue type.}
#'   \item{sample}{Sample identifier.}
#'   \item{breed}{Breed of the horse from which the tissue sample was obtained (factor).}
#'   \item{sex}{Sex of the horse (factor).}
#'   \item{post_counts}{Numeric value indicating post-processed counts associated with the sample.}
#'   \item{source}{Source of the sample (factor).}
#'   \item{new_lab}{Label for the tissue used in analysis (factor).}
#' }
#' @source Data was prepared using the script provided in `data-raw/se_coldata_final.tsv`.
#' @examples
#' # Example of loading and inspecting the tissues dataset
#' data(tissues)
#' head(tissues)
"tissues"

