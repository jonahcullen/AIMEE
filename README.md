
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AIMEE

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/jonahcullen/AIMEE/workflows/R-CMD-check/badge.svg)](https://github.com/jonahcullen/AIMEE/actions)
<!-- badges: end -->

AIMEE (Animal IsomiR and MiRNA Expression Explorer) hosts miRNA and
isomiR expression data across 461 equine small RNA-seq samples, spanning
61 tissue types. The samples were sourced from 12 American Quarter
horses, 4 FAANG Thoroughbreds, and 158 samples from public data
repositories. All samples were processed using the pipeline FARmiR
(<https://github.com/jonahcullen/FARmiR>).

## Installation

You can install the development version of AIMEE like so:

``` r
# install devtools if you have not already
install.packages("devtools")

# install package from github
devtools::install_github("jonahcullen/AIMEE")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(AIMEE)
run_app()
## basic example code
```

## Code of Conduct

Please note that the AIMEE project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
