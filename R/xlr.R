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
#'   a minimal file name such as 'expenses' or a .path such as
#'   'my-data-stash/workbook.xlsx'. [.path] uses [here::here()]. If .path is not
#'   specified, workbook is removed on or before closure. If .path is specified
#'   but the folder structure does not exist, [xlr()] will attempt to
#'   construct it.
#' @param .open If FALSE, workbook will not open after being written.
#' @param .quiet TRUE or FALSE (default) denoting whether you want messaging
#' @param .fieldname_spec NULL (default) or a purrr-style function to apply
#' across all fieldnames; e.g. `~janitor::clean_names(., case = 'title'))` to
#' titlecase fieldnames, 'toupper' for all caps, or `janitor::clean_names` for snakecase
#' @param .tabname_spec
#' @param .workbook_spec
#' @param .return Type of output in R. Default is the tibbled dataframes. If workbook-object, then
#'
#' @seealso [openxlsx::buildWorkbook()]
#'
#' @return
#' the side-effect, a workbook of spreadsheets, is the primary output of this function
#' See
#' direct-effect: a flat list of tibbles (returned invisibly)
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
#'
xlr <- function(...
               ,.path = NULL
               ,.open = TRUE
               ,.quiet = FALSE
               ,.heading = entibble()
               ,.fieldname_spec = NULL
               ,.tabname_spec = list(sep = ".", pad = ".", name_spec = "{inner}") # arguments passed to scrub tabnames, see [scrub_tabnames] for options; # collapse description for list-embedded tabnames grouped, passed to purrr::list_flatten @importParam
               ,.workbook_spec = list(asTable = TRUE, orientation = 'landscape', zoom = 65) # @seealso [opensxlsx::buildWorkbook] args as a named list
               ,.return = c('dataframes', 'workbook-object', 'savepath')
){


  df_list <- list_iron( ... , name_spec = .tabname_spec[['name_spec']], .f = entibble) |>
    purrr::discard( \(.x) identical(.x, entibble()) )
  df_list <- df_list[names(df_list) != '']

  if( length(df_list)==0 ){
    if(!.quiet){ cli::cli_alert_danger('Insufficient data.') }
    return(invisible(NULL))
  }


# construct savename path ------------------------------------------------------
  mk_tempfile <- is.null(.path)

  if (mk_tempfile){

    wb_name_tmp <- rlang::quos( ... ) |> rlang::exprs_auto_name() |>
      names() |> purrr::pluck(1) |> fs::path_sanitize(replacement = "#") |>
      stringr::str_sub(start = 1, end = 20)
    # if( identical(wb_name_tmp, character(0)) ){ wb_name_tmp <- 'NULL' }
    # if(wb_name_tmp == ''){ wb_name_tmp <- 'tmp' }
    wb_name_tmp <- paste0(wb_name_tmp, '_',
                          stringr::str_replace(format(Sys.time(), "%Y%m%d_%H%M-%OS3"),'\\.',''), '.xlsx')
    # return(wb_name_tmp)
    .path <- file.path(tempdir(), wb_name_tmp)


  } else if (!stringr::str_detect(.path, '(?i)\\.xlsx$')) {
    .path <- paste0(here::here(.path),'.xlsx')
  } else {
    .path <- here::here(.path)
  }




# flatten lists, prep sheet names, entibble data, adjust fieldnames ------------
  if(!is.null(.fieldname_spec)){ df_list <- purrr::map(df_list, .f = .fieldname_spec) }
  for_scrub_tabnames <- list(tabnames = names(df_list), quiet = .quiet) |>
    purrr::list_assign(rlang::splice(.tabname_spec)) |>
    purrr::discard_at('name_spec') |>
    purrr::map_at(.at = c('sep','pad'), .f = scrub_tabnames)
  names(df_list) <- rlang::call2(xlr::scrub_tabnames, rlang::splice(for_scrub_tabnames)) |>
    rlang::eval_tidy()



# spreadsheet formatting --------------------------------------------------
  options("openxlsx.maxWidth" = 45)
  options(openxlsx.dateFormat = "yyyy-mm-dd")
  date_style <- openxlsx::createStyle(numFmt = 'DATE')
  header_style <- openxlsx::createStyle(
    textDecoration = "Bold", fontColour = '#ffffff', bgFill = '#aaaaaa',
    border = 'topbottom', borderStyle = 'thick')

  start_row <- nrow(.heading) + 1
  for_buildWorkbook <- list(
    x = df_list,
    asTable = TRUE,
    orientation = 'landscape',
    zoom = 65,
    startRow = start_row ) |>
    purrr::list_assign(rlang::splice(.workbook_spec))
  wb <- rlang::call2(openxlsx::buildWorkbook, rlang::splice(for_buildWorkbook)) |> rlang::eval_tidy()
  names(wb) <- names(df_list)


  # return(names(df_list))
# build & save workbook --------------------------------------------------------
  for(sheet_name in names(df_list)){
    dt_cols <- which(purrr::map_lgl(df_list[[sheet_name]], lubridate::is.Date))
    px_cols <- which(purrr::map_lgl(df_list[[sheet_name]], lubridate::is.POSIXt))
    purrr::map(dt_cols, ~openxlsx::addStyle(wb, sheet_name, style = date_style, rows = 1:(nrow(df_list[[sheet_name]])+1), cols = .))
    openxlsx::addStyle(wb, sheet = sheet_name, style = header_style, rows = start_row:start_row, cols = 1:ncol(df_list[[sheet_name]]))
    openxlsx::freezePane(wb, sheet = sheet_name, firstRow = T)
    openxlsx::freezePane(wb, sheet = sheet_name, firstActiveRow = start_row+1)
    openxlsx::setColWidths(wb, sheet = sheet_name, widths = 'auto', cols = 1:ncol(df_list[[sheet_name]]))
    openxlsx::setColWidths(wb, sheet = sheet_name, widths = 18, cols = px_cols)
    openxlsx::setColWidths(wb, sheet = sheet_name, widths = 10, cols = dt_cols)
  }
  openxlsx::saveWorkbook(wb, file = .path, overwrite = T, returnValue = T)


# pop open ---------------------------------------------------------------------
  if( .open ){
    if(.Platform$OS.type == "unix") {
      system2('xdg-open', glue::glue("'{.path}'"))
    } else if((.Platform$OS.type == "windows")){
      shell.exec(.path)
    } else (
      browseURL(.path)
    )
  }


# unlink in ~3 min -------------------------------------------------------------
  if(mk_tempfile){ later::later(~base::unlink(.path), 180) }


# return user-specified output -------------------------------------------------
  out <- base::switch(
    EXPR = match.arg(.return[[1]], c('dataframes', 'workbook-object', 'savepath'), several.ok = FALSE),
    dataframes = df_list,
    `workbook-object` = wb,
    savepath = .path)

  cli::cat_line()
  cli::cat_line('Workbook location:')
  cli::cli_alert('{(.path)}')
  cli::cat_line()


  return(invisible(out))


}



# is.posix <- function(x) any(grepl('POSIX', class(x)), na.rm = TRUE)
# px_cols <- which(purrr::map_lgl(df_list[[sheet_name]], is.posix))
