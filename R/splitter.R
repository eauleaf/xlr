#' Splits, with auto-naming, a dataframe into a list of dataframes
#'
#' Similar to [dplyr::group_split()] or [base::split()], but:
#'
#' * takes only dataframes
#' * auto-names the list of output dataframes
#' * splits based on column names, column numbers, or any selection features from [dplyr::select()]
#' * if user specifies no columns to split by, `splitter()` will split on groupings from [dplyr::group_by()]
#' * does not create dataframe groups, but preserves dataframe groups in outputs
#'
#'
#' @param .data a dataframe
#' @param ... specified columns to split by
#' @param .sep character by which to
#'
#' @return named list of dataframes
#' @export
#'
#' @examples
#' # splits by column names
#' mtcars |> splitter(cyl)
#' mtcars |> splitter('cyl')
#' mtcars |> splitter(gear, cyl)
#'
#' # or a column number
#' mtcars |> splitter(2)
#' mtcars |> splitter(2, 10)
#'
#' # splits by `dplyr` grouped variables if no split info is passed as an argument
#' mtcars |> dplyr::group_by(cyl) |> splitter()
#'
#' # if there are already groups, but a split argument is passed in,
#' #  splitter ignores the `dplyr` groupings as a split variable and
#' #  preserves the `dplyr` groups in the result
#' mtcars |> dplyr::group_by(cyl) |> splitter(gear)
#'
splitter <- function(.data, ..., .sep = '|'){

  checkmate::assert_data_frame(.data)
  checkmate::assert_string(.sep)

  make_groups <- ...length()!=0
  has_groups <- !identical(dplyr::groups(.data),list())


  if( has_groups && make_groups ){
    prior_groupings <- .data |> dplyr::groups()
    .data <- .data |> dplyr::ungroup()
  } else if (!make_groups && !has_groups) {
    cli::cli_abort('No column names provided to split on?')
  }

  groups <- dplyr::select(.data, ...) |>
    purrr::reduce(\(a, b) paste0(a, sep = .sep, b)) |>
    suppressMessages()

  out <- split(.data, f = groups)

  if( has_groups && make_groups ){
    out <- out |> purrr::map(\(.) dplyr::group_by(., !!!prior_groupings))
  }

  return(out)

}

