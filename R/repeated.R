#' Flag all values in a vector that occur more than once.
#'
#' @description Useful for reviewing datasets with where there should be unique
#' values. It is similar to [duplicated()], but marks all repeated values as
#' TRUE rather than just the second observation and thereafter.
#'
#' Treats NaN, Inf, and NA like any other value. `repeated(c(NULL, NULL, NULL,
#' NaN, Inf, NA, NA))` returns `c(FALSE, FALSE, TRUE, TRUE)`
#'
#' @details A convenience function to use with dplyr::filter() to make it easy
#' to identify and review all repeated observations in a dataframe. The function
#' is an alias for `x %in% x[duplicated(x)]`.
#'
#' Filtering a dataframe by the repeated values in column 'x', as in
#' `dplyr::filter(.data, repeated(x))`, performs the same task as grouping by a
#' variable and selecting counts greater than 1, as in `dplyr::group_by(.data,
#' x)  |> dplyr::filter(dplyr::n()>1)`, but is quicker and easier to remember.
#'
#' (Ideally [repeated()] would be written in c++ and included in [dplyr].)
#'
#' @param x a vector
#' @param incomparables a vector of values that cannot be compared. FALSE is a
#'   special value, meaning that all values can be compared, and
#'   may be the only value accepted for methods other than the default; values
#'   are coerced internally to be the same type as x.
#' @param ... passed to [duplicated()]
#'
#' @return a logical vector the same length as the input
#' @export
#'
#' @examples
#' repeated(c(1, 2, 3, 4, 5, 5, 5))
#' repeated(c('A','A','B','C',5,5), incomparables = c('A'))
#' c(NA, NA, '', NULL, NULL, NULL) |> repeated()
#'
#' # Find all cars where 'qsec' is the same as another car's:
#' mtcars |> dplyr::filter(repeated(qsec))
#'
#' # For clarity, compare repeated() output to duplicated() output:
#' mtcars |> dplyr::filter(repeated(wt))
#' mtcars |> dplyr::filter(duplicated(wt))
#'
#' # To use dplyr::filter() on several variables, put a `,` for 'and',
#' #  and use `|` for an 'or' statement, as in:
#' mtcars |> dplyr::filter(repeated(qsec), repeated(wt))
#' mtcars |> dplyr::filter(repeated(qsec) | repeated(wt))
#'
repeated <- function(x, incomparables = FALSE, ...) {

  checkmate::qassert(x, c("v",0))
  checkmate::qassert(incomparables, c("v",0))
  x %in% x[duplicated(x, incomparables = incomparables, ...)]

}
