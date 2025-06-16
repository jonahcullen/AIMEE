#' mirgenedb_map: Canonical miRNAs from MirGeneDB v3.0
#'
#' This dataset contains a mapping of miRNA identifiers to their curated canonical
#' miRNA annotations as defined by MirGeneDB v3.0. It is used in AIMEE to enable
#' filtering and comparison of mature miRNA sequences against a trusted reference set.
#'
#' @format A data frame with 259 rows and 4 columns:
#' \describe{
#'   \item{id}{Unique identifier for each miRNA (character).}
#'   \item{Read_rna}{miRNA sequence (character).}
#'   \item{mirgenedb_id}{Canonical miRNA ID from MirGeneDB v3.0 (character).}
#'   \item{names}{Collapsed miRNA names corresponding to each `id` (character).}
#' }
#'
#' @source Derived from the `data-raw/mirgenedb_map.tsv` file.
#'
#' @examples
#' data(mirgenedb_map)
#' head(mirgenedb_map)
"mirgenedb_map"
