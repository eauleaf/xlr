#' Sets key chord "ctrl + alt + shift + v" to auto-run [paste_from_xl()].
#' Sets key chord "ctrl + alt + shift + c" to auto-run [copy_from_xl()].
#' Restarts rstudio session.
#'
#' Running this function allows the user to press the key chord "ctrl + alt + shift
#' + v" to paste data copied from a spreadsheet into R directly where the cursor sits,
#' as well as allows the user to press "ctrl + alt + shift + c" to copy a data objects
#'  into memory by variable or expression hightlighted in rstudio.
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
#' @examples \dontrun{ xlr::set_xlr_key_chords() }
#'
set_xlr_key_chords <- function(){

  rstudio.prefs::use_rstudio_keyboard_shortcut( "Ctrl+Alt+Shift+V" = "xlr::run_paste_from_xl" )
  rstudio.prefs::use_rstudio_keyboard_shortcut( "Ctrl+Alt+Shift+C" = "xlr::run_copy_for_xl" )
  rstudio.prefs::use_rstudio_keyboard_shortcut( "Ctrl+Alt+Shift+N" = "xlr::run_enscript" )
  rstudioapi::restartSession()

}


