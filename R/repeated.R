#' Flag all values in a vector that occur more than once.
#'
#' Similar to [duplicated()], but marks all repeated values as TRUE.
#'
#' Removes NULL, and treats NaN, Inf, and NA like any other value.
#'   `repeated(c(NULL, NULL, NULL, NaN, Inf, NA, NA))` returns
#'   `c(FALSE, FALSE, TRUE, TRUE)`
#'
#' @details
#' Datasets often contain repeated data where there should be unique values.
#'  [repeated()] is a convenience function that is helpful for reviewing
#'  repeated observations in a dataframe. It's an alias for
#'  `x %in% x[duplicated(x)]`.
#'
#' Filtering a dataframe '.data' by the repeated values in column 'x',
#'   as in `dplyr::filter(.data, xlr::repeated(x))`, performs the same task as
#'   grouping by a variable and selecting counts greater than 1, as in
#'   `dplyr::group_by(.data, x)  |> dplyr::filter(dplyr::n()>1)`, but is
#'   quicker and easier to remember.
#'
#' Ideally [repeated()] would be written in c++ and included in [dplyr].
#'
#' https://stackoverflow.com/questions/28244123/find-duplicated-elements-with-dplyr
#'
#'
#' @param x a vector
#'
#' @return a logical vector the same length as the input
#' @export
#'
#' @examples
#' repeated(c(1, 2, 3, 4, 5, 5, 5))
#' c(NA, NA, '', NULL, NULL, NULL) |> repeated()
#'
#' # Find all 'artists' in dataset 'billboard' who had more than one hit:
#' tidyr::billboard |> dplyr::filter(repeated(artist))
#'
#' # Compare repeated() output to duplicated() output:
#' mtcars |> dplyr::filter(repeated(wt))
#' mtcars |> dplyr::filter(duplicated(wt))
#'
#' # To filter on several variables use `&` and `|`, as in:
#' mtcars |> dplyr::filter(repeated(qsec), repeated(wt))
#' mtcars |> dplyr::filter(repeated(qsec) | repeated(wt))
#'
repeated <- function(x) {

  x <- checkmate::qassert(x, c("v",0))
  x %in% x[duplicated(x)]

}

