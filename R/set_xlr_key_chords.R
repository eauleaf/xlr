#' Sets xlr key-chords
#'
#' - Sets key-chord "ctrl + alt + shift + v" in RStudio to auto-run [paste_from_xl()].
#' - Sets key-chord "ctrl + alt + shift + c" in RStudio to auto-run [copy_for_xl()].
#' - Sets key-chord "ctrl + alt + shift + n" in RStudio to auto-run [enscript()].
#'
#' @details
#' To enable the quick keys after running `set_xlr_key_chords()`, you must close
#' and re-launch RStudio.
#'
#' To set quick-keys manually in RStudio, follow these instructions from Posit:
#' \url{https://support.posit.co/hc/en-us/articles/206382178-Customizing-Keyboard-Shortcuts-in-the-RStudio-IDE}
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

  rstudio.prefs::use_rstudio_keyboard_shortcut(
    # "Ctrl+Alt+Shift+L" = "xlr::run_xl",
    "Ctrl+Alt+Shift+V" = "xlr::run_paste_from_xl",
    "Ctrl+Alt+Shift+C" = "xlr::run_copy_for_xl",
    "Ctrl+Alt+Shift+N" = "xlr::run_enscript"
    )


  cli::cli_alert_warning('After setting quick-keys, you must terminate and re-launch RStudio.')

}


# NOT USED
# #' - Sets key-chord "ctrl + alt + shift + l" in RStudio to auto-run [xl()].
