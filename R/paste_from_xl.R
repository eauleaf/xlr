#' Paste data into Rstudio from a spreadsheet
#'
#'
#' @description
#' Imports spreadsheet data from the user's clipboard to RStudio. Also,
#' copies full file paths if copied from an OS file navigation window.
#'
#' Imported data formats in 1 of 2 ways.
#' 1) If pasting to the console window, 'ctrl+alt+shift+v' will insert a tibble
#' or vector of the copied spreadsheet data.
#' 2) If pasting to the text editor window, 'ctrl+alt+shift+v' will insert an
#' expression to produce the spreadsheet data.
#'
#' To set the quick keys, use [set_xlr_key_chords()]
#'
#' - If pasting multiple spreadsheet rows, the data imports as a tibble.
#' - If pasting a single spreadsheet row with multiple columns, the data imports as a vector.
#' - If pasting folder and file paths from an OS navigation window, the data is imports as a vector.
#'
#'
#' @note Clipboard memory is limited; if you're copying a large quantity of data from a
#'   spreadsheet, try reading in the data instead with [readxl::read_excel()].
#'
#' @param has_fieldnames TRUE or FALSE indicating whether field names are
#'   present in the spreadsheet data you copied. If user supplies no value,
#'   `paste_from_xl()` guesses by looking at the clipboard data. If the function
#'   incorrectly guesses the presence of field names, just place T or F into
#'   the function echoed to the console. If `has_fieldnames` is set to either T
#'   or F, the data always imports as a tibble.
#'
#' @details To set the quick keys in RStudio, run function:
#' [set_xlr_key_chords()].
#' Or, set the quick keys manually by following these RStudio instructions:
#' \url{https://support.posit.co/hc/en-us/articles/206382178-Customizing-Keyboard-Shortcuts-in-the-RStudio-IDE}
#'
#' @returns
#' ** If user copied a single spreadsheet row or column into clipboard memory,
#'   returns a vector.
#' ** If user copied more than one spreadsheet row or column to the clipboard, returns a tibble.
#' ** If user copied a local file to the clipboard, returns a string with the path to the file.
#'
#' @export
#'
#' @examples \dontrun{
#' # (All examples require the user to interactively select data.)
#' # Copy data from a spreadsheet into working memory with 'ctrl + c' or
#' # by right-clicking with the mouse and selecting 'copy', then,
#' # to paste the copied spreadsheet data into R, type into the R
#' # console:
#'
#' (my_data <- paste_from_xl())
#' (my_data <- paste_from_xl(T))
#' (my_data <- paste_from_xl(F))
#' paste_from_xl()
#'
#'
#' }
#'
paste_from_xl <- function( has_fieldnames = NULL ){


  checkmate::assert_flag(has_fieldnames, null.ok = TRUE)

  # run checks & copy data -----------------------------------------------
  if( !interactive() ){
    cli::cli_abort('[.fn paste_from_xl] must be run interactively.')
  }

  datr <- suppressWarnings(clipr::read_clip())
  if ( is.null(datr) ) {
    cli::cli_alert_danger('The clipboard is empty. Nothing to paste.')
    return(invisible())
  }


  # process clip ------------------------------------------------------------
  if(
    is.null(has_fieldnames) &&
    (
      length(datr) == 1 ||
      all(stringr::str_detect(datr, pattern = '\\t', negate = TRUE),na.rm = TRUE)
    )
    ){

    # clip to vector
    from_xl <- I(datr) |>
      readr::read_delim(
        delim = '\t', col_names = FALSE,
        show_col_types = FALSE, trim_ws = TRUE) |>
      unlist(use.names = FALSE)

  } else {

    # guess headers
    if( is.null(has_fieldnames) ){
      headers <- I(datr[1]) |>
        readr::read_delim( delim = '\t', col_names = FALSE,
                           show_col_types = FALSE,
                           name_repair = "universal_quiet") |>
        purrr::map_lgl(~ (is.character(.) | is.na(.)) & !grepl('[\\/]',.) ) |>
        all()
    } else if ( has_fieldnames ) {
      # unless user specified
      headers <- TRUE
    } else {
      headers <- FALSE
    }

    # clip to tibble
    from_xl <- I(datr) |>
      readr::read_delim(delim = '\t', col_names = headers, show_col_types = FALSE,
                        name_repair = "universal_quiet",trim_ws = TRUE) |>
      as.data.frame() |>
      entibble()

  }


  # check if windows path
  # Sys.info()['sysname']=='Windows'
  # chartr("\\", "/", from_xl)
  if( length(from_xl)==1 && all(stringr::str_detect(from_xl[[1]], '^((?:[A-Z]:\\\\)|(?:/))'), na.rm = TRUE) ){
    from_xl <- from_xl |> rlang::set_names('path')
  }


return(from_xl)


}





#' Rstudio addin function to call [paste_from_xl()]
#'
#' This function responds to key-chord `ctrl+alt+shift+v`.
#'
#' @returns void
#'
#' @export
#'
run_paste_from_xl <- function(){

  if( !rstudioapi::isAvailable() ){
    cli::cli_alert_danger(
      'RStudio is not available.
      {.fn run_paste_from_xl} is for interactive use in RStudio.'
    )
    return(invisible())
  }

  paste_locn <- purrr::pluck(rstudioapi::getActiveDocumentContext(), "id")


  datr <- suppressWarnings(clipr::read_clip())
  if ( is.null(datr) && .Platform$OS.type == "windows" ) {

    # Run Windows powershell to get paths and capture output
    powershell_script='
    [void] [System.Reflection.Assembly]::LoadWithPartialName("System.Drawing")
    [void] [System.Reflection.Assembly]::LoadWithPartialName("System.Windows.Forms")
    [System.Windows.Forms.Clipboard]::GetData("FileDrop")
    '
    datr <- shQuote(powershell_script) |>
      shell(shell = 'powershell', intern = TRUE) |> try(silent = T)
  }

  if (is.null(datr) || identical(datr, character(0)) || inherits(datr, "try-error") ){
    cli::cli_alert_danger('The clipboard is empty. Nothing to paste.')
    return(invisible())
  }



  out <- paste_from_xl()
  default_input_name <- 'datr'
  # if( is.null(dim(out)) && identical(names(out),'path') ){
  if( is.null(dim(out)) && all(stringr::str_detect(out, '^((?:[A-Z]:\\\\)|(?:/))'), na.rm = TRUE) ){
    default_input_name <- 'path'
  }

  # ask user for a variable name --------------------------------------------
  input_name <- svDialogs::dlg_input(
    message = "Assign `<-` a variable name to the data?",
    default = default_input_name)$res

  # check the variable name
  input_name <- .check_assigned_input(input_name)

  if( input_name != '' ){
    input_name <- paste0(input_name,' <- ')
  }



  # if pasting to script ----------------------------------------------------
  if( paste_locn != "#console" ){

    out_expr <- glue::glue('\n{input_name}{enscript({out}, to_clipboard = FALSE )}\n')
    row1 <- purrr::pluck(rstudioapi::getSourceEditorContext(id = paste_locn),'selection', 1, 'range', 'start', 'row')
    rstudioapi::insertText(text = out_expr, id = paste_locn)
    row2 <- purrr::pluck(rstudioapi::getSourceEditorContext(id = paste_locn),'selection', 1, 'range', 'start', 'row')
    rstudioapi::setSelectionRanges(c(row1,0,row2,Inf), id = paste_locn)
    # https://docs.posit.co/ide/server-pro/1.3.947-1/rstudio-ide-commands.html
    rstudioapi::executeCommand('reindent')
    try(rstudioapi::setCursorPosition(c(row1,0), id = paste_locn),silent = TRUE)

  }

  rstudioapi::sendToConsole(code = paste0("(",input_name, 'xlr::paste_from_xl())'), focus = FALSE)

}




#' Pop-up used in [run_paste_from_xl()]
#'
#' If user specified a syntactically invalid variable name, provides a
#' suggested name and asks user to accept or change it.
#'
#' @param input_name variable name as a string
#'
#' @return user assigned name as a string
#'
#' @examples \dontrun{ .check_assigned_input(' 1 bad name') }
#'
.check_assigned_input <- function(input_name){
  # input_name <- svDialogs::dlg_list(choices = preselect)
  if( identical(input_name, as.character()) ){
    return('')
  } else {
    input_name <- stringr::str_trim(input_name)
    temp_name <- make.names(input_name)
    if(temp_name != input_name){
      input_name <- svDialogs::dlg_input(
        message = c("Your assigned name is syntatically invalid. Perhaps this name instead?"),
        default = temp_name)$res
      input_name <- .check_assigned_input(input_name)
    }
    return(input_name)
  }
}

