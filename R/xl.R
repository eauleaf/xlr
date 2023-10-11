#' View an R data objects in a Libreoffice or Excel workbook
#'
#' Provides an output of datasets to view through the user's default spreadsheet
#' program.
#'
#' @details
#' If you use quick keys, the function [set_xlr_key_chords()] will assign
#' `ctrl + alt + shift + l` to the function `xl()`. The quick keys allow you to
#' open a spreadsheet by highlighting an expression or variable within an RStudio
#' text editor, and then pressing all 3 control keys `ctrl + alt + shift` as well
#' as the letter `L` to 'launch' a spreadsheet.
#'
#'
#' @param ... A dataframe, list of dataframes, or inputs coercible to one
#'   or more dataframes.
#' @param .path Optional path to save a copy of the workbook output. Uses
#'   [here::here()] for savepath completion. If `.path` does not end in '.xlsx',
#'   automatically adds the suffix. If `.path` is not specified, workbook is
#'   saved only momentarily in an auto-named temp file. Always creates the specified
#'   directory location.
#' @param .open If `FALSE`, workbook will not open after being written.
#' @param .quiet If `FALSE`, echoes function messages.
#' @param .sheet_titles `NULL`, a character vector the same length as the number of
#' spreadsheets, or a purrr-style function or formula to apply to the default input names.
#' The default is function: [stringr::str_to_title()].
#' @param .dataframe_spec `NULL` (default), or a purrr-style function to apply
#'   across all dataframes; e.g. `~janitor::clean_names(., case = 'title'))` or
#'   `janitor::clean_names` to titlecase or snakecase fieldnames.
#' @param .tabname_spec a list that allows the user to define tab labeling
#'   arguments: sep = ".", pad = ".", and name_spec = "{inner}"
#' @param .workbook_spec a list of arguments to pass to
#'   [openxlsx::buildWorkbook()], e.g. `list(asTable = TRUE, orientation =
#'   'landscape', zoom = 100, startRow = 7)`
#' @param .return return (invisibly) one of 'workbook' (default), 'savepath',
#' 'tibbles', or 'boolean' for workbook write success/failure
#'
#'
#' @seealso [openxlsx::buildWorkbook()]
#'
#' @return
#' The side-effect, a workbook of spreadsheets, is the primary output of this function.
#' The direct-return output is determined by the input parameter `.return` denoting
#' whether to return the `openxlsx` workbook-object, the list of input data cast as
#' tibbles, or the save path. Any output is returned invisibly.
#'
#' @export
#'
#' @examples \dontrun{
#' xl('hi')
#' xl(mtcars, iris)
#' xl(iris, mtcars)
#' xl(iris, iris)
#' xl(iris)
#' xl(mtcars, iris, .workbook_spec = list(asTable = FALSE, orientation = 'landscape', name = NULL))
#' xl(mtcars, .sheet_titles = NULL)
#' iris |> split(f = iris$Species) |> xl(.workbook_spec = list(startRow = c(6,4,2), zoom = 110))
#' iris |> split(f = iris$Species) |> xl(.sheet_titles=c('SMALL',NA,'LARGE'))
#' dplyr::starwars |> xl()
#' dplyr::starwars |> split(f = dplyr::starwars$eye_color) |> xl(.sheet_titles = toupper)
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
               ,.return = list('workbook', 'savepath', 'tibbles', 'boolean')
){

  # writeData(wb, sheet = "Data", x = data, colNames = FALSE, withFilter = FALSE, tableStyle = "none")

# do some input checks ---------------------------------------------------------
  .tabname_spec <- list(sep = ".", pad = ".", name_spec = "{inner}") |> purrr::list_assign(rlang::splice(.tabname_spec))
  checkmate::assert(
    checkmate::check_list(.tabname_spec, any.missing = FALSE, len = 3),
    checkmate::check_names(.tabname_spec, must.include = c('sep', 'pad', 'name_spec'))
  )
  # checkmate::assert_string(.path, null.ok = TRUE, na.ok = FALSE)
  checkmate::assert_flag(.open, na.ok = FALSE, null.ok = FALSE)
  checkmate::assert_flag(.quiet, na.ok = FALSE, null.ok = FALSE)
  checkmate::assert(
    checkmate::check_formula(.dataframe_spec, null.ok = TRUE),
    checkmate::check_function(.dataframe_spec, null.ok = TRUE)
  )
  checkmate::assert_list(.workbook_spec, null.ok = FALSE)
  .return <- checkmate::assert_character(.return[[1]], null.ok = FALSE)

  # build list of tibbles --------------------------------------------------------
  df_list <- list_iron( ... , name_spec = .tabname_spec[['name_spec']], .f = entibble) |>
    purrr::map_if(.p = \(.x) identical(.x, tibble::tibble()), .f = \(.x) entibble(`NULL` = "NULL"))
  len_wb <- length(df_list)
  in_names <- names(df_list)


  if( len_wb==0 ){
    cli::cli_abort('Insufficient data provided to create a workbook.')
  }

  # some more input checks -------------------------------------------------------
  checkmate::assert(
    checkmate::check_function(.sheet_titles),
    checkmate::check_formula(.sheet_titles),
    checkmate::check_character(.sheet_titles, len = len_wb),
    checkmate::check_null(.sheet_titles)
  )


# apply a user-specified function to dataframes ---------------------------
  if(!is.null(.dataframe_spec)){ df_list <- df_list |> purrr::map(.f = .dataframe_spec) }

# construct savename path ------------------------------------------------------
  path <- expr_savepath(..., .path = .path)


# flatten lists, prep sheet names, entibble data, adjust fieldnames ------------
  for_scrub_tabnames <- list(tabnames = in_names, quiet = .quiet) |>
    purrr::list_assign(rlang::splice(.tabname_spec)) |>
    purrr::discard_at('name_spec') |> purrr::map_at(.at = c('sep','pad'), .f = scrub_tabnames)
  names(df_list) <- sheet_names <- rlang::call2(scrub_tabnames, rlang::splice(for_scrub_tabnames)) |> rlang::eval_tidy()
  # note: `names(df_list)` and `sheet_names` can be different due to list naming rules in R


# determine sheet titles & start_row -------------------------------------------
  df_start_row <- 3
  min_startrow <- 2
  no_titles <- identical(NULL,.sheet_titles)
  if( no_titles ){
    df_start_row <- 1
    min_startrow <- 1
  } else if( !is.character(.sheet_titles) ){
    sheet_fun <- rlang::as_function(.sheet_titles)
    .sheet_titles <- in_names |> sheet_fun()
  }

  if(identical(.workbook_spec$asTable[1],TRUE)){
    unique_names <- df_list |> purrr::map(names) |> purrr::map( \(.) uniquify_names(tabnames = ., width = 100, sep = '_', pad = '0', quiet = .quiet) )
    df_list <- purrr::map2(.x = df_list, .y = unique_names, \(.x, .y) rlang::set_names(.x,.y))
  }

  # incorporate any user assigned workbook specs
  .workbook_spec <- list(
    x = df_list,
    asTable = FALSE,
    # asTable = TRUE,
    startRow = df_start_row,
    zoom = 70,
    withFilter = TRUE
  ) |> purrr::list_assign(rlang::splice(.workbook_spec))


  # return(.workbook_spec)

  # create vector of "df start row" in .workbook_spec$startRow
  # (start rows are important if the user wants to leave room to add data later or add additional table)
  .workbook_spec$startRow <- pmax(.workbook_spec$startRow, min_startrow)
  if(length(.workbook_spec$startRow)!=len_wb){
    .workbook_spec$startRow <- rep(x = .workbook_spec$startRow[[1]], times = len_wb)
  }


# build the workbook ------------------------------------------------------
  # wb <- rlang::call2(openxlsx::buildWorkbook, rlang::splice(.workbook_spec)) |> rlang::eval_tidy()
  wb <- rlang::call2(openxlsx::buildWorkbook, rlang::splice(.workbook_spec)) |> rlang::eval_tidy()
  names(wb) <- sheet_names
  openxlsx::modifyBaseFont(wb, fontSize = 10, fontName = 'Arial')


# spreadsheet formatting options --------------------------------------------------
  date_style <- openxlsx::createStyle(numFmt = 'DATE')
  fieldname_style <- openxlsx::createStyle(
    textDecoration = "Bold",
    fontColour = '#ffffff', bgFill = '#aaaaaa',
    border = 'topbottom', borderStyle = 'thick', fontSize = 11)
  header_style <- openxlsx::createStyle(
    textDecoration = "Bold", fontSize = 12)


  # apply formatting workbook ----------------------------------------------------
min_width <- 8
max_width <- 50
scale_width <- .90
  for(i in 1:len_wb){
    start_row <- .workbook_spec$startRow[[i]]
    current_df <- df_list[[i]]
    sheet_nm <- sheet_names[[i]]
    col_widths <- round(purrr::map_dbl(current_df, \(.) min(max(nchar(as.character(.)),min_width, na.rm = TRUE), max_width))*scale_width)
    dt_cols <- which(purrr::map_lgl(current_df, lubridate::is.Date))
    px_cols <- which(purrr::map_lgl(current_df, lubridate::is.POSIXt))
    purrr::map(dt_cols, \(.)openxlsx::addStyle(wb, sheet = sheet_nm, style = date_style, rows = start_row:(nrow(current_df)+start_row+1), cols = .))
    openxlsx::addStyle(wb, sheet = sheet_nm, style = fieldname_style, rows = start_row:start_row, cols = 1:ncol(current_df))
    openxlsx::freezePane(wb, sheet = sheet_nm, firstActiveRow = start_row+1)
    openxlsx::setColWidths(wb, sheet = sheet_nm, widths = col_widths, cols = 1:ncol(current_df))
    openxlsx::setColWidths(wb, sheet = sheet_nm, widths = 10, cols = dt_cols)
    openxlsx::setColWidths(wb, sheet = sheet_nm, widths = 18, cols = px_cols)
    openxlsx::addFilter(wb, sheet = i, rows = start_row, cols = 1:ncol(current_df))
    if(!no_titles){
      openxlsx::addStyle(wb, sheet = sheet_nm, style = header_style, rows = 1, cols = 1, stack = TRUE)
      openxlsx::writeData(wb, sheet = sheet_nm, x = .sheet_titles[[i]], startCol = 1, startRow = 1, colNames = TRUE)
    }
  }


# save workbook --------------------------------------------------------------
  if(!is.null(.path)){dir.create(path = dirname(path), showWarnings = FALSE, recursive = TRUE)}
  out <- openxlsx::saveWorkbook(wb, file = path, overwrite = TRUE, returnValue = TRUE)

# open and unlink wb after ~5 min ----------------------------------------------
  if( .open ){ sys_open(path) }
  # if temp file, delete
  if(is.null(.path)){ later::later(func = ~unlink(path), delay = 300 )}
  .locn_reporter(path, desc = 'Workbook ')



# prep user-specified return output --------------------------------------------
  .return = match.arg(.return[[1]], c('workbook', 'savepath', 'tibbles', 'boolean'), several.ok = FALSE)
    out <- switch(
      EXPR = .return,
      savepath = path,
      tibbles = df_list,
      workbook = wb,
      boolean = out
    )



  return(invisible(out))


}



#' Addin function to call [xl()]
#'
#' Requires RStudio
#'
#' @return void
#' @export
#'
#'
#' @examples \dontrun{
#' # Highlight each item below and press key-chord `ctrl + alt + shift + l`.
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
      "i" = "Highlight an expression in your text editor, then press `ctrl+alt+shift+L`."
    ))
  } else {
    rstudioapi::sendToConsole(code = paste(text_expr, '|> xlr::xl()'))
  }

}


### quick manual tests
# dplyr::starwars |> split(f = dplyr::starwars$eye_color) |> xl(.sheet_titles = ~stringr::str_to_title(paste(., 'eye color')))
# dplyr::starwars |> split(f = dplyr::starwars$eye_color) |> xl(.dataframe_spec = ~janitor::clean_names(., case = 'title'))
# dplyr::starwars |> xl(.open = F)
# dplyr::starwars |> xl(.path = 'boogabooga')
# file.remove(here::here('boogabooga.xlsx'))
