#' Copies R data to your clipboard
#'
#' @description
#' The format of the copied data is tab-delimeted for pasting to a spreadsheet.
#'
#' There are 2 ways to pass data into the function:
#' 1) Type it out the same as any other function.
#' 2) Highlight a data object in your RStudio text editor and then press
#' key-chord: `ctrl + alt + shift + c`. (Use the quick keys just like you use
#' `ctrl + c` for copying, except that [copy_for_xl()] uses all 3 control keys
#' plus the 'c'.
#'
#'
#' @details
#' To set up the key-chord in RStudio. Run [set_xlr_key_chords()].
#'
#' Copying formatted output to the clipboard requires package `clipr`.
#'
#' If using Linux, make sure to install a clipboard tool:
#' apt-get install xclip
#' or
#' apt-get install xsel
#'
#' [copy_for_xl()] coerces the input data object to a tibble prior to copying to
#' clipboard.
#'
#'
#' @note
#' All quick-keys in {.pkg xlr} use all 3 control keys `ctrl + alt + shift` plus
#' a letter.)
#'
#' @param . a dataframe or object that can be coerced to a dataframe
#'
#' @return the data provided coerced to a tibble; clipboard data as side-effect
#' @export
#'
#'
#' @examples \dontrun{
#'
#' copy_for_xl(iris)
#' copy_for_xl(letters)
#' copy_for_xl(letters)
#' colours() |> copy_for_xl()
#' head(mtcars) |> copy_for_xl()
#' colours() |> rlang::set_names() |> entibble(another_col = colours()) |> copy_for_xl()
#' dplyr::bind_cols(letters, LETTERS) |> head() |> copy_for_xl()
#' letters |> rlang::set_names(LETTERS) |> copy_for_xl()
#'
#'
#' enlist(head(iris),head(mtcars)) |> copy_for_xl()
#'
#' # To use quick keys. Run {.fn set_xlr_key_chords}.
#' # Then highlight the word 'letters' below and press key-chord `ctrl + alt + shift + c`.
#' letters
#'
#' }
#'
copy_for_xl <- function(. = NULL){

  if( !interactive() ){
    cli::cli_abort('[.fn copy_for_xl] must be run interactively.')
  }

# catch expression symbol -------------------------------------------------
  r_label <- rlang::enquo(.) |> rlang::as_label()


# check on clipboard ---------------------------------------------------------
  if( !clipr::clipr_available() ){
    cli::cli_abort(
      "Your system's clipboard isn't available. Install package 'clipr' to
      use this function and make sure you have a clipboard program for
      your operating system."
    )
  }

# from console ------------------------------------------------------------
  .out <- entibble(!!.)
  if( length(.out) == 1 ){ .out <- rlang::set_names(.out, r_label) }
  if( length(.out) == 2 && names(.out)[1] == 'rowname'){ .out <- rlang::set_names(.out, c('rowname', r_label)) }

# nested tibble error ------------------------------------------------------
  if( any(purrr::map_lgl(.out, is.list)) ) {
    nested_cols <- purrr::map_lgl(.out, is.list)
    cli::cat_line()
    cli::cli_alert_danger('Data contains nested lists. Omitted {sum(nested_cols)} columns. ')
    cli::cat_line('For complete data, consider running:  ')
    cli::cli_alert('  xl({r_label})')
    cli::cat_line()
    .out <- .out[!nested_cols]
  }

  clipr::write_clip(.out)
  cli::cli_alert_success('Table copied to clipboard:')


  return(.out)

}




#' Addin function to call [copy_for_xl()]
#'
#' For interactive use in RStudio.
#'
#'
#' @return NULL
#' @export
#'
#'
#' @examples \dontrun{
#' # Highlight a text item below and press key-chord `ctrl + alt + shift + c`.
#' # To implement the key-chord in RStudio. Run [set_xlr_key_chords()].
#' # Note that setting [set_xlr_key_chords()] will restart the R session.
#'
#' head(iris)
#' colours()
#' letters
#' dplyr::bind_cols(letters, LETTERS)
#' letters |> rlang::set_names(LETTERS)
#'
#' # ragged list embedded tibbles should throw an error
#' enlist(head(iris),letters)
#'
#' }
run_copy_for_xl <- function(){


  if( !rstudioapi::isAvailable() ){
    cli::cli_abort(
      'RStudio is not available.
      {.fn run_copy_for_xl} is for interactive use in RStudio.'
    )
  }


  last_editor <- rstudioapi::documentId(allowConsole = FALSE)
  text_expr <- suppressWarnings(stringr::str_trim(rstudioapi::selectionGet(id = last_editor)$value))


  if( identical(text_expr, character(0)) || text_expr == '' ){

    cli::cli_bullets(c(
      "x" = '{.strong Nothing selected to {.fn copy_for_xl} }',
      "i" = "Highlight an expression in your text editor, then press `ctrl+alt+shift+c`."
    ))

  } else {

    rstudioapi::sendToConsole(code = paste(text_expr, '|> xlr::copy_for_xl()'))

  }


}








