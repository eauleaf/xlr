#' View an R objects in a Libreoffice or Excel workbook
#'
#' Provides output of R datasets through the user's default spreasheet
#' program. Produces output similar to [tibble::view()], but in a spreadsheet,
#' which allows for larger datasets and a list of dataframes.
#'
#' Auto-corrects forbidden sheetnames
#' Flattens embedded lists with preserved name structure
#'
#'
#' @param ... A dataframe, list of dataframes, or inputs coerceable to one
#'   or more dataframes.
#' @param .path Optional path to save a copy of the workbook output. Uses
#'   [here::here()]. If `.path` is not specified, workbook is removed on or before
#'   closure.
#' @param .open TRUE (default); If FALSE, workbook will not open after being written.
#' @param .quiet TRUE or FALSE (default), denoting whether you want messaging.
#' @param .sheet_titles NULL, a character vector the same length as the number of
#' spreadsheets, or a purrr-style function/formula to apply to the default input names.
#' Default is the function `stringr::str_to_title`.
#' @param .dataframe_spec NULL (default), or a purrr-style function to apply
#'   across all dataframes; e.g. `~janitor::clean_names(., case = 'title'))` or
#'   `janitor::clean_names` to titlecase or snakecase fieldnames.
#' @param .tabname_spec a list that allows the user to define tab labeling
#'   arguments: sep = ".", pad = ".", name_spec = "{inner}"
#' @param .workbook_spec a list of arguments to pass to
#'   [openxlsx::buildWorkbook()], e.g. list(asTable = TRUE, orientation =
#'   'landscape', zoom = 70, startRow = 7)
#' @param .return one of NULL, 'savepath', 'tibbles', or 'workbook'
#' specifying the return information
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
#' xl(list('hi'))
#' enlist('hi') |> xl()
#' enlist(hi) |> xl()
#' xl(mtcars)
#' xl(NA)
#' xl(a = NA)
#' xl(character(0))
#' xl(';')
#' xl(TRUE)
#' list(c(list(1:5), list(5:1)), letters) |> xl()
#' xl(c(1:5))
#' c(1:5) |> xl()
#' xl(1)
#' xl(list(a = 1,5))
#' list(a = 1,b=5, 1:10, letters, as.matrix(warpbreaks[1:10,]) ) |> xl()
#' c(1,1:5, list(1:10)) |> xl()
#' a <- tibble::tibble(iris); xl(a)
#' rlang::set_names(as.list(letters), LETTERS) |> xl()
#' a_dataframe <- tibble::tibble(iris); xl(a_dataframe)
#' xl()
#' xl('')
#' xl(NULL)
#' xl(a = NULL)
#' xl(iris, dplyr::starwars, mtcars, cars)
#' }
#'
xl <- function(...
               ,.path = NULL
               ,.open = TRUE
               ,.quiet = TRUE
               ,.sheet_titles = stringr::str_to_title
               ,.dataframe_spec = NULL
               ,.tabname_spec = list(sep = ".", pad = ".", name_spec = "{inner}") # arguments passed to scrub tabnames, see [scrub_tabnames] for options; # collapse description for list-embedded tabnames grouped, passed to purrr::list_flatten @importParam
               ,.workbook_spec = list(asTable = TRUE, orientation = 'landscape', zoom = 70) # @seealso [opensxlsx::buildWorkbook] args as a named list
               ,.return = list(NULL, 'savepath', 'tibbles', 'workbook')
){


# do some input checks ---------------------------------------------------------
  checkmate::assert(
    checkmate::check_list(.tabname_spec, any.missing = FALSE, len = 3),
    checkmate::check_names(.tabname_spec, must.include = c('sep', 'pad', 'name_spec'))
  )
  checkmate::assert_string(.path, null.ok = TRUE, na.ok = FALSE)
  checkmate::assert_flag(.open, na.ok = FALSE, null.ok = FALSE)
  checkmate::assert_flag(.quiet, na.ok = FALSE, null.ok = FALSE)
  checkmate::assert(
    checkmate::check_formula(.dataframe_spec, null.ok = TRUE),
    checkmate::check_function(.dataframe_spec, null.ok = TRUE)
  )
  checkmate::assert_list(.workbook_spec, null.ok = FALSE)

# build list of tibbles --------------------------------------------------------
  df_list <- list_iron( ... , name_spec = .tabname_spec[['name_spec']], .f = entibble) |>
    purrr::discard( \(.x) identical(.x, entibble()) )
  df_list <- df_list[names(df_list) != '']

# some more input checks -------------------------------------------------------
  checkmate::assert(
    checkmate::check_function(.sheet_titles),
    checkmate::check_formula(.sheet_titles),
    checkmate::check_character(.sheet_titles, len = length(df_list)),
    checkmate::check_null(.sheet_titles)
  )

  if( length(df_list)==0 ){
    cli::cli_alert_danger('Insufficient data provided to create a workbook.')
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
  if(!is.null(.dataframe_spec)){ df_list <- purrr::map(df_list, .f = .dataframe_spec) }
  in_names <- names(df_list)
  for_scrub_tabnames <- list(tabnames = in_names, quiet = .quiet) |>
    purrr::list_assign(rlang::splice(.tabname_spec)) |>
    purrr::discard_at('name_spec') |> purrr::map_at(.at = c('sep','pad'), .f = scrub_tabnames)
  sheet_names <- rlang::call2(scrub_tabnames, rlang::splice(for_scrub_tabnames)) |>
    rlang::eval_tidy()
  names(df_list) <- sheet_names



# sheet titles -----------------------------------------------------------------
  start_row <- 3
  no_titles <- identical(NULL,.sheet_titles)
  if( no_titles ){
    start_row <- 1
  } else if( is.character(.sheet_titles) ){
    sheet_titles <- .sheet_titles
  } else {
    sheet_fun <- rlang::as_function(.sheet_titles)
    sheet_titles <- in_names |> sheet_fun()
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
  names(wb) <- sheet_names
  names(sheet_names) <- names(df_list)
  names(sheet_titles) <- names(df_list)


  # allows user to overwrite 'start_row' if provided in 'for_buildWorkbook'
  start_row <- for_buildWorkbook$startRow

# apply formatting workbook ----------------------------------------------------
  for(df_name in names(df_list)){
    df_nm <- df_list[[df_name]]
    sheet_nm <- sheet_names[[df_name]]
    dt_cols <- which(purrr::map_lgl(df_nm, lubridate::is.Date))
    px_cols <- which(purrr::map_lgl(df_nm, lubridate::is.POSIXt))
    purrr::map(dt_cols, ~openxlsx::addStyle(wb, sheet = sheet_nm, style = date_style, rows = 1:(nrow(df_nm)+1), cols = .))
    openxlsx::addStyle(wb, sheet = sheet_nm, style = fieldname_style, rows = start_row:start_row, cols = 1:ncol(df_nm))
    openxlsx::freezePane(wb, sheet = sheet_nm, firstRow = TRUE)
    openxlsx::freezePane(wb, sheet = sheet_nm, firstActiveRow = start_row+1)
    openxlsx::setColWidths(wb, sheet = sheet_nm, widths = 'auto', cols = 1:ncol(df_nm))
    openxlsx::setColWidths(wb, sheet = sheet_nm, widths = 18, cols = px_cols)
    openxlsx::setColWidths(wb, sheet = sheet_nm, widths = 10, cols = dt_cols)
    openxlsx::addStyle(wb, sheet = sheet_nm, style = header_style, rows = 1, cols = 1, stack = TRUE)
    if(!no_titles){openxlsx::writeData(wb, sheet = sheet_nm, x = sheet_titles[[df_name]], startCol = 1, startRow = 1, name = NULL, colNames = FALSE)}
  }
  # return(enlist(names(wb), names(df_list), sheet_names, sheet_titles))


# save workbook ----------------------------------------------------------------
out <- openxlsx::saveWorkbook(wb, file = .path, overwrite = TRUE, returnValue = TRUE)



# open and unlink wb after ~5 min ----------------------------------------------
  if( .open ){ sys_open(.path) }
  if(mk_tempfile){ later::later(~unlink(.path), 300) }


# prep user-specified direct output --------------------------------------------
  .return = match.arg(.return[[1]], .return, several.ok = FALSE)
  if(!is.null(.return)){
    out <- switch(
      EXPR = .return,
      savepath = .path,
      tibbles = df_list,
      workbook = wb
      )
  }


# report -----------------------------------------------------------------------
  cli::cat_line()
  cli::cat_line('Workbook location:')
  cli::cli_alert('{(.path)}')
  cli::cat_line()


  return(invisible(out))


}



# is.posix <- function(x) any(grepl('POSIX', class(x)), na.rm = TRUE)
# px_cols <- which(purrr::map_lgl(df_nm, is.posix))
#' background-color: #00A500;



#' Addin function to call [xl()]
#'
#' Requires RStudio
#'
#' @return void
#' @export
#'
#'
#' @examples \dontrun{
#' # Highlight each item below and press key-chord `ctrl + alt + shift + >`.
#' # To implement the quick-keys. Run {.fn set_xlr_key_chords}.
#'
#' mtcars
#' (1:5 * 10)
#' rep("🎊🌈",3)
#' dplyr::starwars |> head()
#' letters
#'}
#'
run_xl <- function(){

  if( !rstudioapi::isAvailable() ){
    cli::cli_abort(
      'RStudio is not available.
      {.fn run_enscript} is for interactive use in RStudio.'
    )
  }

  last_editor <- rstudioapi::documentId(allowConsole = FALSE)
  text_expr <- suppressWarnings(stringr::str_trim(rstudioapi::selectionGet(id = last_editor)$value))

  if( identical(text_expr, character(0)) || text_expr == '' ){
    cli::cli_bullets(c(
      "x" = '{.strong Nothing selected to {.fn xl} }',
      "i" = "Highlight an expression in your text editor, then press `ctrl+alt+shift+>`."
    ))
  } else {
    rstudioapi::sendToConsole(code = paste(text_expr, '|> xlr::xl()'))
  }

}



