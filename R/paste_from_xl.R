#' Paste formatted data into Rstudio from a spreadsheet, with key chord
#' 'ctrl+alt+shift+v'.
#'
#' @description Pastes spreadsheet data from the user's clipboard to RStudio,
#' formatted in 1 of 2 ways.
#'
#' 1) If pasting to the console window, 'ctrl+alt+shift+v' will insert a tibble
#' of the copied spreadsheet data.
#'
#' 2) If pasting to the text editor window, 'ctrl+alt+shift+v' will insert an
#' expression to produce the tibble.
#'
#' Will also paste folder and file paths if user pressed 'ctrl + c' on the file
#' icons in a browser window.
#'
#' To set the quick keys.
#' @seealso [xlr::set_xlr_key_chords()]
#'
#' @note If you're copying a lot of data from a spreadsheet, instead read it in
#'   with [readxl::read_excel()].
#'
#'
#' @param has_fieldnames TRUE or FALSE indicating whether field names are
#'   present in the spreadsheet data you copied. If user supplies no value,
#'   [paste_from_xl()] guesses by looking at the clipboard data.
#'
#'   Not settable if using quick key shortcut. If data incorrectly guesses field
#'   names, go to the console window and write 'T' or 'F' in the echoed
#'   function.
#'
#' @details To set the quick keys in RStudio, run function
#' [xlr::set_xlr_key_chords()], or set the quick keys manually by following
#' these instructions.
#' [https://support.posit.co/hc/en-us/articles/206382178-Customizing-Keyboard-Shortcuts-in-the-RStudio-IDE]
#'
#' @return
#' * If user copied a single spreadsheet row into the clipboard memory,
#'   returns a vector.
#' * If user copied more than one spreadsheet row to the clipboard, returns a tibble.
#' * If user copied a local file to the clipboard, returns a string with the path to the file.
#' @export
#'
#' @examples \dontrun{
#'
#' Copy data from a spreadsheet into working memory with 'ctrl + c' or by
#' right-clicking with the mouse and selecting 'copy', then type into the R
#' console:
#'
#' (my_data <- paste_from_xl())
#' (my_data <- paste_from_xl(T))
#' (my_data <- paste_from_xl(F))
#' paste_from_xl()
#'
#' to paste the copied spreadsheet data into R.
#' }
#'
paste_from_xl <- function( has_fieldnames = NULL ){


  checkmate::assert_flag(has_fieldnames, null.ok = TRUE)

  # run checks & copy data -----------------------------------------------
  if( !base::interactive() ){
    cli::cli_abort('[.fn paste_from_xl] must be run interactively.')
  }

  xl_data <- clipr::read_clip()
  if ( base::is.null(xl_data) ) {
    cli::cli_alert_danger('The clipboard is empty. Nothing to paste.')
    return(invisible())
  }



  # process clip ------------------------------------------------------------
  if( base::length(xl_data) == 1 ){

    # clip to vector
    from_xl <- base::I(xl_data) |>
      readr::read_delim(delim = '\t', col_names = FALSE, show_col_types = FALSE, trim_ws = TRUE) |>
      base::unlist(use.names = FALSE)

  } else {

    # guess headers
    if( base::is.null(has_fieldnames) ){
      headers <- base::I(xl_data[1]) |>
        readr::read_delim( delim = '\t', col_names = FALSE, show_col_types = FALSE, name_repair = "universal_quiet") |>
        purrr::map_lgl(~ (base::is.character(.) | base::is.na(.)) & !grepl('[\\/]',.) ) |>
        base::all()
    } else if ( has_fieldnames ) {
      # unless user specified
      headers <- TRUE
    } else {
      headers <- FALSE
    }

    # clip to tibble
    from_xl <- base::I(xl_data) |>
      readr::read_delim(delim = '\t', col_names = headers, show_col_types = FALSE,
                        name_repair = "universal_quiet",trim_ws = TRUE) |>
      base::as.data.frame() |>
      entibble()

  }

  return(from_xl)


}




#' Addin function to call [paste_from_xl()]
#'
#' @return
#' @export
#'
#' @examples
run_paste_from_xl <- function(){

  if( !rstudioapi::isAvailable() ){
    cli::cli_abort(
      'RStudio is not available.
      {.fn run_paste_from_xl} is for interactive use in RStudio.'
    )
  }


  # ask user for a variable name --------------------------------------------
  input_name <- svDialogs::dlg_input(
    message = "Assign `<-` a variable name to the data?",
    default = 'datr')$res

  input_name <- xlr:::check_assigned_input(input_name)

  if( input_name != '' ){
    input_name <- paste0(input_name,' <- ')
  }


  rstudioapi::sendToConsole(code = base::paste0("(",input_name, 'xlr::paste_from_xl())'))


}




#' Used in [run_paste_from_xl()] to require usable console name
#'
#' @noRd
#'
#' @param input_name variable name as a string
#'
#' @return user assigned name as a string
#'
#' @examples
check_assigned_input <- function(input_name){
  # input_name <- svDialogs::dlg_list(choices = preselect)
  if( identical(input_name, as.character()) ){
    return('')
  } else {
    input_name <- stringr::str_trim(input_name)
    temp_name <- make.names(input_name)
    if(temp_name != input_name){
      input_name <- svDialogs::dlg_input(
        message = c("The assignment name is syntatically invalid. Use this name instead?"),
        default = temp_name)$res
      input_name <- check_assigned_input(input_name)
    }
    return(input_name)
  }
}


# determine what to do if in an editor

# # determine where to paste ------------------------------------------------
# if( rstudioapi::isAvailable() ){
#   paste_locn <- purrr::pluck(rstudioapi::getActiveDocumentContext(), "id")
# } else {
#   paste_locn <- "#console"
# }


# return ------------------------------------------------------------------

# base::assign(x = input_name, value = from_xl, envir = .GlobalEnv)
# if(paste_locn == '#console'){
#   cat(paste0('',input_name,' <- xlr::paste_from_xl()\n'))
#   return(from_xl)
# } else {
#   out_expr <- glue::glue_collapse( c(input_name,' <- ', base::deparse(from_xl)) ) |>
#     base::strwrap(exdent = 3, width = 70)
#   return(rstudioapi::insertText(text = out_expr, id = paste_locn))
# }
