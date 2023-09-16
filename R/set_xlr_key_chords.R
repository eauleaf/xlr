#' Sets key chord "ctrl + alt + shift + v" to auto-run [paste_from_xl()].
#' Sets key chord "ctrl + alt + shift + c" to auto-run [copy_from_xl()].
#' Restarts rstudio session.
#'
#' When in Rstudio, running this function allows the user to:
#'  * press the key chord "ctrl + alt + shift + v" to paste data copied from a spreadsheet into R directly where the cursor sits
#'  * press the key chord "ctrl + alt + shift + c" to copy a data object into memory by highlighted variable or expression
#'  * press the key chord "ctrl + alt + shift + n" to create a script to reproduce a highlighted data object or expression
#'
#' @details To set quick-keys manually, follow these instructions from Posit:
#' [https://support.posit.co/hc/en-us/articles/206382178-Customizing-Keyboard-Shortcuts-in-the-RStudio-IDE]
#'
#' @seealso [paste_from_xl()]
#' @seealso [copy_for_xl()]
#'
#'
#' @return NULL
#' @export
#'
#' @examples \dontrun{ set_xlr_key_chords() }
#'
set_xlr_key_chords <- function(){

  if( !rstudioapi::isAvailable() ){
    cli::cli_abort(
      'RStudio is not available.
      {.fn set_xlr_key_chords} requires RStudio.'
    )}

  if( !interactive() ){
    cli::cli_alert_danger(
      '{.fn set_xlr_key_chords} requires interactive use in RStudio.'
    )}

  cli::cli_alert_warning('Setting quick-keys will restart your RStudio Session.')

  rstudio.prefs::use_rstudio_keyboard_shortcut( "Ctrl+Alt+Shift+V" = "xlr::run_paste_from_xl" )
  rstudio.prefs::use_rstudio_keyboard_shortcut( "Ctrl+Alt+Shift+C" = "xlr::run_copy_for_xl" )
  rstudio.prefs::use_rstudio_keyboard_shortcut( "Ctrl+Alt+Shift+N" = "xlr::run_enscript" )

  rstudioapi::restartSession()

}


