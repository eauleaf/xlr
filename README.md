xlr
================

<br>

> A set of tools to help data scientists work interactively between
> **R** and **spreadsheets**.

<hr>
<!-- badges: start -->

[![](https://img.shields.io/badge/devel%20version-0.0.0.9037-blue.svg)](https://github.com/eauleaf/xlr)
[![](https://img.shields.io/github/last-commit/eauleaf/xlr.svg)](https://github.com/eauleaf/xlr/commits/main)
<!-- badges: end -->

<br>

## Setup

``` r
# install.packages("remotes")
remotes::install_github('eauleaf/xlr', build_vignettes = TRUE)
library(xlr)
```

## Example

``` r
# Send R data to workbook
iris |> split(f = iris$Species) |> xl()
```

## Testing

Because several `xlr` functions require interactive use, reporting
issues would be helpful.

## Contribute

`xlr` has taken hundreds of hours to write, test, and re-write. If you
find the package helpful, please consider paying it forward by donating
to Ukraine: <https://u24.gov.ua/>
