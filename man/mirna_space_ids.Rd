% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mirna_space_ids.R
\docType{data}
\name{mirna_space_ids}
\alias{mirna_space_ids}
\title{mirna_space_ids: miRNA identifiers with miRNA-space tags}
\format{
A data frame with 2 columns:
\describe{
  \item{id}{Unique identifier for each miRNA (character).}
  \item{filter_type}{The type of miRNA-space (character). It can be one of the following:
  \itemize{
    \item \code{Exclusive repeat}: Indicates that the miRNA is exclusive and within a repeat region.
    \item \code{Exclusive}: Indicates that the miRNA is exclusive.
    \item \code{Ambiguous repeat}: Indicates that the miRNA is ambiguous and within a repeat region.
    \item \code{Ambiguous}: Indicates that the miRNA is ambiguous.
  }}
}
}
\source{
Data derived from `data-raw/isomir_rpm.isomirmap_exp_filt.tsv` file.
}
\usage{
mirna_space_ids
}
\description{
This dataset contains miRNA ids and associated miRNA-space tags.
The data has been processed to categorize miRNA ids with regards to miRNA-space.
}
\examples{
data(mirna_space_ids)
head(mirna_space_ids)
}
\keyword{datasets}
