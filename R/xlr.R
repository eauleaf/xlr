#' View an R dataframe or dataframes quickly in a Libreoffice or Excel workbook.
#' TODO: background-color: #00A500;
#'
#' Provides a means of reviewing datasets through the user's default spreasheet
#' program. Produces output similar to [tibble::view()], but in a spreadsheet,
#' which allows for larger datasets and a list of dataframes.
#'
#' Auto-corrects forbidden sheetnames
#' Flattens embedded lists with preserved name structure
#'
#'
#' @param ... A dataframe, list of dataframes, or inputs coerceable to one
#'   or more dataframes.
#' @param .path Optional path to save a copy of the output spreadsheet. Specify
#'   a minimal file name s*-+
#'   3.6920uch as 'expenses' or a .path such as
#'   'my-data-stash/workbook.xlsx'. [.path] uses [here::here()]. If .path is not
#'   specified, workbook is removed on or before closure. If .path is specified
#'   but the folder structure does not exist, [xlr()] will attempt to
#'   construct it.
#' @param .open If FALSE, workbook will not open after being written.
#' @param .quiet TRUE or FALSE (default) denoting whether you want messaging
#' @param .sheet_titles TRUE (default), FALSE, or a character vector/list of the
#'   same count as spreadsheets to be written
#' @param .fieldname_spec NULL (default) or a purrr-style function to apply
#'   across all fieldnames; e.g. `~janitor::clean_names(., case = 'title'))` to
#'   titlecase fieldnames, 'toupper' for all caps, or `janitor::clean_names` for
#'   snakecase
#' @param .tabname_spec a list that allows the user to define tab labeling
#'   arguments: sep = ".", pad = ".", name_spec = "{inner}"
#' @param .workbook_spec a list of arguments to pass to
#'   [openxlsx::buildWorkbook()], e.g. list(asTable = TRUE, orientation =
#'   'landscape', zoom = 70)
#' @param .return defines the type of output to R, such as the dataframes, the
#'   workbook-object, or the save path. Default is NULL.
#'
#' @seealso [openxlsx::buildWorkbook()]
#'
#' @return
#' The side-effect, a workbook of spreadsheets, is the primary output of this function.
#' The direct-return output is TRUE or FALSE (default) denoting whether the
#' xlsx workbook file wrote out properly. However, if the parameter `.return` is
#' specified by the user, the function will instead return the list of inputs as
#' tibbles, the `openxlsx` workbook-object, or the save path. All direct outputs
#' are returned invisibly.
#'
#' @export
#'
#' @examples \dontrun{
#' xlr(mtcars)
#' xlr(NA)
#' xlr(a = NA)
#' xlr(character(0))
#' xlr(';')
#' xlr(TRUE)
#' list(c(list(1:5), list(5:1)), letters) |> xlr()
#' xlr(c(1:5))
#' c(1:5) |> xlr()
#' xlr(1)
#' xlr(list(a = 1,5))
#' list(a = 1,b=5, 1:10, letters, as.matrix(warpbreaks[1:10,]) ) |> xlr()
#' c(1,1:5, list(1:10)) |> xlr()
#' a <- tibble::tibble(iris); xlr(a)
#' rlang::set_names(as.list(letters), LETTERS) |> xlr()
#' a_dataframe <- tibble::tibble(iris); xlr(a_dataframe)
#' xlr()
#' xlr('')
#' xlr(NULL)
#' xlr(a = NULL)
#' xlr(iris, dplyr::starwars, mtcars, cars)
#'
xlr <- function(...
               ,.path = NULL
               ,.open = TRUE
               ,.quiet = TRUE
               ,.sheet_titles = TRUE
               ,.fieldname_spec = NULL
               ,.tabname_spec = list(sep = ".", pad = ".", name_spec = "{inner}") # arguments passed to scrub tabnames, see [scrub_tabnames] for options; # collapse description for list-embedded tabnames grouped, passed to purrr::list_flatten @importParam
               ,.workbook_spec = list(asTable = TRUE, orientation = 'landscape', zoom = 70) # @seealso [opensxlsx::buildWorkbook] args as a named list
               ,.return = list(NULL, 'savepath', 'tibbles', 'workbook')
){


# do some input checks ---------------------------------------------------------
  checkmate::assert_string(.path, null.ok = TRUE, na.ok = FALSE)
  checkmate::assert_flag(.open, na.ok = FALSE, null.ok = FALSE)
  checkmate::assert_flag(.quiet, na.ok = FALSE, null.ok = FALSE)
  # parameter '.sheet_titles' done way below
  checkmate::assert(
    checkmate::check_formula(.fieldname_spec, null.ok = TRUE),
    checkmate::check_function(.fieldname_spec, null.ok = TRUE)
  )
  checkmate::assert(
    checkmate::check_list(.tabname_spec, any.missing = FALSE, len = 3),
    checkmate::check_names(.tabname_spec, must.include = c('sep', 'pad', 'name_spec'))
  )
  checkmate::assert_list(.workbook_spec, null.ok = FALSE)



# put ...dots in tibbles and a list  -------------------------------------------
  df_list <- list_iron( ... , name_spec = .tabname_spec[['name_spec']], .f = entibble) |>
    purrr::discard( \(.x) identical(.x, entibble()) )
  df_list <- df_list[names(df_list) != '']


# more input checks ------------------------------------------------------------
  if( length(df_list)==0 ){
    cli::cli_alert_danger('Insufficient data provided.')
    return(invisible(NULL))
  }


# construct savename path ------------------------------------------------------
  mk_tempfile <- is.null(.path)

  if (mk_tempfile){
    wb_name_tmp <- rlang::quos( ... ) |> rlang::exprs_auto_name() |>
      names() |> purrr::pluck(1) |> fs::path_sanitize(replacement = "#") |>
      stringr::str_sub(start = 1, end = 20)
    wb_name_tmp <- paste0(
      wb_name_tmp, '_',
      stringr::str_replace(format(Sys.time(), "%Y%m%d_%H%M-%OS3"),'\\.',''), '.xlsx')
    .path <- file.path(tempdir(), wb_name_tmp)
  } else if (!stringr::str_detect(.path, '(?i)\\.xlsx$')) {
    .path <- paste0(here::here(.path),'.xlsx')
  } else {
    .path <- here::here(.path)
  }



# flatten lists, prep sheet names, entibble data, adjust fieldnames ------------
  if(!is.null(.fieldname_spec)){ df_list <- purrr::map(df_list, .f = .fieldname_spec) }
  raw_tabnames <- names(df_list)
  for_scrub_tabnames <- list(tabnames = raw_tabnames, quiet = .quiet) |>
    purrr::list_assign(rlang::splice(.tabname_spec)) |>
    purrr::discard_at('name_spec') |> purrr::map_at(.at = c('sep','pad'), .f = scrub_tabnames)
  names(df_list) <- rlang::call2(xlr::scrub_tabnames, rlang::splice(for_scrub_tabnames)) |>
    rlang::eval_tidy()



# Sheet Titles -----------------------------------------------------------------
  no_titles <- identical(FALSE,.sheet_titles)
  if( no_titles ){
    start_row <- 1
  } else if( identical(TRUE,.sheet_titles) ){
    start_row <- 3
    .sheet_titles <- raw_tabnames |>
      janitor::make_clean_names(case = 'title', allow_dupes = T) |>
      as.list() |> purrr::set_names(names(df_list))
  } else {
    if(length(.sheet_titles)!=length(names(df_list))){
      cli::cli_abort('Input (.sheet_titles) must be TRUE, FALSE,
                   or a character vector/list of the same count as the spreadsheets.')
      }
    start_row <- 3
    names(.sheet_titles) <- names(df_list)
  }


# some spreadsheet formatting --------------------------------------------------
  options("openxlsx.maxWidth" = 45)
  options(openxlsx.dateFormat = "yyyy-mm-dd")
  date_style <- openxlsx::createStyle(numFmt = 'DATE')
  fieldname_style <- openxlsx::createStyle(
    textDecoration = "Bold", fontColour = '#ffffff', bgFill = '#aaaaaa',
    border = 'topbottom', borderStyle = 'thick')
  header_style <- openxlsx::createStyle(
    textDecoration = "Bold", fontSize = 12)


# build the workbook -----------------------------------------------------------
  for_buildWorkbook <- list(
    x = df_list,
    asTable = TRUE,
    name = NULL,
    startRow = start_row ) |>
    purrr::list_assign(rlang::splice(.workbook_spec))
  wb <- rlang::call2(openxlsx::buildWorkbook, rlang::splice(for_buildWorkbook)) |> rlang::eval_tidy()
  names(wb) <- names(df_list)
  start_row <- for_buildWorkbook$startRow


# apply formatting workbook ----------------------------------------------------
  for(sheet_name in names(df_list)){
    dt_cols <- which(purrr::map_lgl(df_list[[sheet_name]], lubridate::is.Date))
    px_cols <- which(purrr::map_lgl(df_list[[sheet_name]], lubridate::is.POSIXt))
    purrr::map(dt_cols, ~openxlsx::addStyle(wb, sheet_name, style = date_style, rows = 1:(nrow(df_list[[sheet_name]])+1), cols = .))
    openxlsx::addStyle(wb, sheet = sheet_name, style = fieldname_style, rows = start_row:start_row, cols = 1:ncol(df_list[[sheet_name]]))
    openxlsx::freezePane(wb, sheet = sheet_name, firstRow = TRUE)
    openxlsx::freezePane(wb, sheet = sheet_name, firstActiveRow = start_row+1)
    openxlsx::setColWidths(wb, sheet = sheet_name, widths = 'auto', cols = 1:ncol(df_list[[sheet_name]]))
    openxlsx::setColWidths(wb, sheet = sheet_name, widths = 18, cols = px_cols)
    openxlsx::setColWidths(wb, sheet = sheet_name, widths = 10, cols = dt_cols)
    openxlsx::addStyle(wb, sheet = sheet_name, style = header_style, rows = 1, cols = 1, stack = TRUE)
    if(!no_titles){openxlsx::writeData(wb, sheet = sheet_name, x = .sheet_titles[[sheet_name]], startCol = 1, startRow = 1, name = NULL, colNames = FALSE)}
  }


# save workbook ----------------------------------------------------------------
out <- openxlsx::saveWorkbook(wb, file = .path, overwrite = TRUE, returnValue = TRUE)



# pop wb open ------------------------------------------------------------------
  if( .open ){
    if(.Platform$OS.type == "unix") {
      system2('xdg-open', glue::glue("'{.path}'"))
    } else if((.Platform$OS.type == "windows")){
      shell.exec(.path)
    } else (
      browseURL(.path)
    )
  }


# unlink wb after ~5 min -------------------------------------------------------
  if(mk_tempfile){ later::later(~base::unlink(.path), 300) }


# prep user-specified direct output --------------------------------------------
  .return = match.arg(.return[[1]], .return, several.ok = FALSE)
  if(!is.null(.return)){
    out <- base::switch(
      EXPR = .return,
      savepath = .path,
      tibbles = df_list,
      workbook = wb
      )
  }


# report -----------------------------------------------------------------------
  cli::cat_line()
  cli::cat_line('Workbook save location:')
  cli::cli_alert('{(.path)}')
  cli::cat_line()


  return(invisible(out))


}



# is.posix <- function(x) any(grepl('POSIX', class(x)), na.rm = TRUE)
# px_cols <- which(purrr::map_lgl(df_list[[sheet_name]], is.posix))
