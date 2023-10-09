#' Create a savepath from user expression or user specs
#'
#' To be used in a function that takes dots
#'
#' @param ... an expression (not used if .path is specified)
#' @param .path a full save path or file name string with out without suffix
#' @param .file_suffix a file type, e.g. '.xlsx' or '.pdf'
#' @param .tmp_prefix if producing a temp file, prefix before date and time name components, e.g. 'xlr'
#'
#' @return the complete path string
#' @export
#'
#' @examples
#' expr_savepath(.path = 'hi')
#' iris |> expr_savepath()
#' iris |> expr_savepath(.path = 'hi')
#' expr_savepath(iris, .file_suffix = '.pdf', .tmp_prefix = 'boing-')
#' # .path, if specified, overrides prefix
#' expr_savepath(.path = 'hi', .file_suffix = '.pdf', .tmp_prefix = 'boing-')
#' iris |> expr_savepath(.path = 'somewhere/hi/bye', .file_suffix = '.pdf')
#'
expr_savepath <- function(..., .path = NULL, .file_suffix = '.xlsx', .tmp_prefix = 'xlr-'){

  checkmate::assert_string(.path, null.ok = TRUE, na.ok = FALSE)
  checkmate::assert_string(.file_suffix, null.ok = TRUE, na.ok = FALSE)
  checkmate::assert_string(.tmp_prefix, null.ok = TRUE, na.ok = FALSE)

  if (is.null(.path)){
    wb_name_tmp <- rlang::quos( ... ) |> rlang::exprs_auto_name() |> names() |> purrr::pluck(1)
    wb_name_tmp <- paste0(.tmp_prefix, wb_name_tmp) |> fs::path_sanitize(replacement = "#") |>
      stringr::str_replace_all('\\$','#') |> # 'gio open' incorrectly handles filenames with a '$' char. E.g. '$' |> xl() fails in Ubuntu (so removing '$' from filenames)
      stringr::str_squish() |>
      stringr::str_sub(start = 1, end = 20)
    wb_name_tmp <- paste0(
      wb_name_tmp, '_',
      stringr::str_replace(format(Sys.time(), "%Y%m%d_%H%M_%OS3"),'\\.',''), .file_suffix)
    .path <- file.path(tempdir(), wb_name_tmp)
  } else if (!stringr::str_detect(.path, glue::glue('(?i)\\{.file_suffix}$'))) {
    .path <- paste0(here::here(.path),.file_suffix)
  } else {
    .path <- here::here(.path)
  }

  return(.path)

}


#' Report out save location
#'
#' @param path a complete path to cat
#' @param desc optional to describe type of path
#'
#' @return the input path
#'
#' @examples here::here() |> xlr:::.locn_reporter( desc = 'Workbook ')
.locn_reporter <- function(path, desc = ''){

    cli::cat_line()
    cli::cat_line(glue::glue(desc, 'location:'))
    cli::cat_line(glue::glue("    '{path}'"))
    cli::cat_line()

    return(path)

}


