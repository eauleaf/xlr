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
#'   but the folder structure does not exist, [xl()] will attempt to
#'   construct it.
#' @param .open If FALSE, workbook will not open after being written.
#'
#' @param .fieldname_spec function #titlecase_fields
#' @param .return Type of output in R. Default is the tibbled dataframes. If workbook-object, then
#' @seealso [openxlsx::buildWorkbook()]
#'
#' @return A force-named flat list of tibbles, returned invisibly, and a workbook of spreadsheets as side-effect
#'
#' @export
#'
#' @examples \dontrun{
#' xl(mtcars)
#' xl(a = NULL)
#' xl(a = NA)
#' xl(NULL)
#' list(c(list(1:5), list(5:1)), letters) |> xl()
#' c(1:5) |> xl()
#' xl(c(1:5))
#' xl(1)
#' xl(list(a = 1,5))
#' list(a = 1,b=5, 1:10, letters, as.matrix(warpbreaks[1:10,]) ) |> xl()
#' c(1,1:5, list(1:10)) |> xl()
#' a <- tibble::tibble(iris); xl(a)
#' xl(NA)
#' xl(NULL)
#' xl(1)
#' 1:26 |> purrr::set_names(letters) |> xl()
#' as.matrix(warpbreaks[1:10,])
#'}
#'specification args
#'
xl <- function(...
               ,.path = NULL
               ,.open = TRUE
               ,.quiet = FALSE
               ,.fieldname_spec = list() # 'purrr' style function, like "~janitor::clean_names(., case = 'title')" to apply to across field names
               ,.tabname_spec = list(sep = ".", pad = ".", name_spec = "{outer}|{inner}") # arguments passed to scrub tabnames, see [scrub_tabnames] for options; # collapse description for list-embedded tabnames grouped, passed to purrr::list_flatten @importParam
               ,.workbook_spec = list(asTable = TRUE, orientation = 'landscape', zoom = 65, start_row=3) # @seealso [opensxlsx::buildWorkbook] args as a named list
               ,.return = c('tibble-list', 'workbook-object')
){


  df_list <- enflatten( ... , name_spec = .tabname_spec[['name_spec']])
  for_scrub_tabnames <- list(tabnames = names(df_list), quiet = .quiet) |> purrr::list_assign(rlang::splice(.tabname_spec)) |> purrr::map_at(.at = c('sep','pad'), .f = scrub_tabnames)
  names(df_list) <- rlang::call2(xlr::scrub_tabnames, rlang::splice(for_scrub_tabnames)) |> rlang::eval_tidy()
  # return(df_list)


  mk_tempfile <- is.null(.path)


  obj_name <- rlang::englue("{{df_list}}") |> stringr::str_remove_all('[^A-Za-z0-9 ]+') |> stringr::str_sub(start = 1, end = 10)
  if (mk_tempfile){
    .path <- tempfile(pattern = paste0(obj_name,'-tmp'), fileext = '.xlsx')
  } else if (!stringr::str_detect(.path, '(?i)\\.xlsx$')) {
    .path <- paste0(here::here(.path),'.xlsx')
  } else {
    .path <- here::here(.path)
  }

  # check_wb_df <- purrr::safely(tibble::as_tibble)
  # df_list <- purrr::map(df_list, check_wb_df)
  # df_list <- purrr::map(df_list, ~ purrr::pluck(.,1))
  # df_list <- purrr::discard(df_list, ~ base::is.null(.) | base::nrow(.) == 0)


  # format workbook sheets
  options("openxlsx.maxWidth" = 45)
  options(openxlsx.dateFormat = "yyyy-mm-dd")
  is.posix <- function(x) any(grepl('POSIX', class(x)), na.rm = TRUE)
  date_style <- openxlsx::createStyle(numFmt = 'DATE')
  header_style <- openxlsx::createStyle(textDecoration = "Bold", fontColour = '#ffffff', bgFill = '#aaaaaa', border = 'topbottom', borderStyle = 'thick')



  df_list <- purrr::map(df_list, ~janitor::remove_empty(., c('rows', 'cols')))
  if(!is.null(.fieldname_spec)){ df_list <- purrr::map(df_list, ~janitor::clean_names(., case = 'title')) }
  for_buildWorkbook <- list(x = df_list, asTable = TRUE, orientation = 'landscape', zoom = 65) |>
    purrr::list_assign(rlang::splice(.workbook_spec))
  wb <- rlang::call2(openxlsx::buildWorkbook, rlang::splice(for_buildWorkbook)) |> rlang::eval_tidy()
  # wb <- rlang::set_names(wb, names(df_list))
  # wb <- openxlsx::buildWorkbook(x = df_list, asTable = T, orientation = 'landscape', zoom = zoom, startRow = start_row)
  start_row <- 1

  for(sheet_name in names(df_list)){
    dt_cols <- which(purrr::map_lgl(df_list[[sheet_name]], lubridate::is.Date))
    px_cols <- which(purrr::map_lgl(df_list[[sheet_name]], is.posix))
    purrr::map(dt_cols, ~openxlsx::addStyle(wb, sheet_name, style = date_style, rows = 1:(nrow(df_list[[sheet_name]])+1), cols = .))
    openxlsx::addStyle(wb, sheet = sheet_name, style = header_style, rows = start_row:start_row, cols = 1:ncol(df_list[[sheet_name]]))
    # openxlsx::freezePane(wb, sheet = sheet_name, firstRow = T)
    openxlsx::freezePane(wb, sheet = sheet_name, firstActiveRow = start_row+1)
    openxlsx::setColWidths(wb, sheet = sheet_name, widths = 'auto', cols = 1:ncol(df_list[[sheet_name]]))
    openxlsx::setColWidths(wb, sheet = sheet_name, widths = 18, cols = px_cols)
    openxlsx::setColWidths(wb, sheet = sheet_name, widths = 10, cols = dt_cols)
  }

  print(file.exists(.path))
  openxlsx::saveWorkbook(wb, file = .path, overwrite = T, returnValue = T)
  print(file.exists(.path))
  print(.path)

  if(.open) {
    # openxlsx::openXL(.path)
    browseURL(.path)
    }
  # system2('xdg-open', glue::glue("'{.path}'"))
  if(mk_tempfile){ later::later(~base::unlink(.path), 60) }

  # TODO
  # cat .path to the Workbook unless quiet == T
  # return 'tibbles of adjusted data or workbook'

  return(.path)

}


