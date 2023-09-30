
![](vignettes/www/xlr.svg)

# xlr

<!-- badges: start -->
<!-- badges: end -->

`xlr` is a set of tools to help data scientists work interactively
between R and spreadsheets.

## Installation

You can install the development version of xlr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("eauleaf/xlr")
```

## Example

Kick a series of datasets out of R into spreadsheets of a workbook by
passing them to function `xl()`.

``` r
library(xlr)
iris |> split(f = iris$Species) |> xl()
```

For an overview of the other `xlr` functions, click on “Get started”
above.

## Testing

Because several ‘xlr’ functions require interactive use to test,
downloading the package from github and reporting any issues you
encounter would be very helpful.
