#' Write your data into an expression for a script.
#'
#' Copies a formatted script to the clipboard and prints unformatted output
#' to the console.
#'
#'
#' @description
#' A convenience function for interactive use. [enscript()] obtains R's
#' deparsed internal representation of the input data object and prepares that
#' object to be input from a script. The function's purpose is to save the user
#' the inconvenience of reformatting console output data as input code.
#'
#' @details
#' Copying formatted output to the clipboard requires package [clipr].
#'
#' To set up the key-chord `ctrl+alt+shift+v` in RStudio. Run [xlr::set_xlr_key_chords()].
#'
#' If using Linux, make sure to install a clipboard tool:
#'  apt-get install xclip
#'   or
#'  apt-get install xsel
#'
#' @note
#' All quick-keys in {.pkg xlr} use all 3 control keys `ctrl + alt + shift` plus
#' a letter as key chord.)
#'
#' @param . The data object or expression to transform into script
#' @param to_clipboard default TRUE; whether to copy the resulting script to the clipboard
#' @param quiet TRUE or FALSE indicating whether to write the scripted
#'   expression out to the console.
#'
#' @return The formatted deparsed expression, invisibly if to_clipboard = TRUE.
#' The object's internal expression written to clipboard memory as side-effect.
#'
#' @export
#'
#' @examples \dontrun{
#'
#'
#' enscript(letters)
#' # [letters] is copied to clipboard; press `ctrl + v` to paste the output
#'
#' enscript(1:5 * 10)
#' rep("🎊🌈",3) |> enscript()
#'
#' dplyr::starwars |> head() |> enscript()
#'
#' (flowers = tail(iris,10))
#' enscript(flowers, quiet = F)
#'
#' (days_to_go <- seq.Date(Sys.Date(), by = 1, length.out = 7))
#' days_to_go |> enscript()
#'
#' letters |> purrr::set_names(LETTERS) |> enlist() |> enscript()
#'
#' colours() |> enscript()
#'
#' candy <- list('lollipops','gum')
#' enlist(candy, !!candy, !!!candy, rlang::list2(!!!candy)) |> enscript()
#' enlist(!!letters)
#'
#'
#' }
#'
# dplyr::starwars |> head() |> encodeString() |> deparse() |> clipr::write_clip()
#'
enscript <- function(. = NULL, to_clipboard = TRUE, quiet = FALSE ) {


  # from console --------------------------------------------------------------
  .quo <- rlang::enquo(.)
  assignment <- ""
  script_type <- 'Script'


  checkmate::assert_flag(to_clipboard)
  checkmate::assert_flag(quiet)
  width <- rstudioapi::readRStudioPreference( 'margin_column' ,base::getOption('deparse.cutoff'))
  width <- base::max(4, width, na.rm = TRUE)


  # check if user used a name assignment
  if (rlang::quo_is_symbol(.quo) ){
    assignment <- rlang::quo_name(.quo) |>
      base::paste0(" <- ")
  }



  # deparse & format expression text -----------------------------------------

  safe_deparse <- purrr::safely(base::deparse)
  # name_collection <- encodeString(.)
  deparsed_expr <- safe_deparse(., backtick = TRUE)



  if( base::is.null(deparsed_expr[[1]]) ){

    cli::cli_abort('{deparsed_expr[[2]]}')

  } else if(stringr::str_detect(deparsed_expr[[1]][1], '^function \\(')){

    script_type <- 'Function'
    deparsed_expr[[1]][1] <- paste0(assignment, deparsed_expr[[1]][1])
    to_console <- deparsed_expr <- glue::glue_collapse(deparsed_expr[[1]], sep = '\n')

  } else {

    # make syntactically valid names

    # encodeString()

    # deparsed_expr <- rlang::quo_squash(.) |>
    #   purrr::map(~deparse(., backtick = TRUE)) |>
    #   purrr::map(~.format_script(., snip_width = width))
    # # return(deparsed_expr)

    # deparsed_expr <- glue::glue_collapse(c(assignment, paste(deparsed_expr))) |> stringr::str_squish()
    # to_console <- deparsed_expr

    deparsed_expr <- glue::glue_collapse(c(assignment, deparsed_expr[[1]])) |>
      stringr::str_squish()
    to_console <- stringr::str_wrap(deparsed_expr, width = cli::console_width())
    deparsed_expr <- .format_script(deparsed_expr, snip_width = width)
  }


  # write to clipboard ------------------------------------------------------
  if( to_clipboard ){

    if( clipr::clipr_available() ){
      clipr::write_clip(deparsed_expr)
      if(!quiet){
        cli::cat_line()
        cli::cli_alert_success("{script_type} copied to clipboard:")
      }
    } else {
      cli::cli_alert_warning(
        'Cannot copy expression to clipboard; package {.pkg clipr} is unavailable.'
      )
    }


    # cat to console ------------------------------------------------------
    if(!quiet){

      cli::cat_rule()
      cli::cat_line("\n\n")
      cli::cat_line(to_console)
      cli::cat_line("\n\n")
      cli::cat_rule()

    }

    return( invisible( stringr::str_split_1(deparsed_expr, '\\n') ) )

  }

  # return( stringr::str_split_1(deparsed_expr, '\\n') )
  return( deparsed_expr )

}





#' Adds breaks to the text of a deparsed data structure script
#'
#' @param obj_expr text for a data structure
#' @param snip_width limit for the number of characters desired before data folding at comma-space breaks
#'
#' @return a string with embedded '/n' line breaks
#'
#' @examples
#'
#'
#'todo:
 # head(dplyr::starwars) |> enlist() |> deparse(backtick = TRUE) |> glue::glue_collapse() |> stringr::str_squish() |> .format_script() |> cat()
 # enlist(head(iris), "  `quick_text` = 'hello()'`", tail(dplyr::starwars)) |> deparse(backtick = T) |> glue::glue_collapse() |> stringr::str_squish() |> stringr::str_extract_all('(?<=\\(|, )`.+?` = ')
#'
.format_script <- function(obj_expr, snip_width = 70) {


  snip_width <- checkmate::assert_integerish(snip_width, lower = 4, max.len = 1, any.missing = FALSE)


  if( nchar(obj_expr) <= snip_width ) {
    return(obj_expr)
  }


  # separate everything encased in quotes
   # ((?<=\\(|, )`(?:[^`]|`.*?`)+?` = ) # text in backticks
   # "((?:[^"]|\\\\")*(?<!\\\\)",? ?) # text in quotes
  obj_expr <- obj_expr |>
    stringr::str_replace_all('((?<=\\(|, )`(?>[^`]|`.*?`)+?` = |"(?:[^"]|\\\\")*(?<!\\\\)",? ?)', '\n\\1\n') |>
    stringr::str_split_1('\\n')

  quoted <- obj_expr |> stringr::str_detect('^[`"]')

  # format backticked expressions
  obj_expr[quoted] <- stringr::str_replace(obj_expr[quoted], '^[`]', '\n`')

  # format non-quoted expression text
  regex_breaks <- c(
     '(?<=c|list|structure)\\('
    ,'((?<![(]0)\\),? ?)'
    ,'([{}])'
    ,'\\n++'
    ,paste0('(.{1,',snip_width-3,'}(?:\\n++|, ))')
    ,'\\n++'
  )

  regex_replacements = c(
     '(\n'
    ,'\n\\1\n'
    ,'\n\\1\n'
    ,'\n'
    ,'\\1\n'
    ,'\n'
  ) |> rlang::set_names( regex_breaks )

  obj_expr[!quoted] <- obj_expr[!quoted] |> stringr::str_replace_all(regex_replacements)

  # glue everything back together and set breaks for the quoted expressions
  obj_expr <- obj_expr |> glue::glue_collapse() |>
    stringr::str_replace_all('NA, \\n"', 'NA, "') |>
    stringr::str_replace_all('character\\(0\\), \\n+', 'character\\(0\\), ') |>
    stringr::str_replace_all(paste0('(.{1,',snip_width-2,'}(?:\\n++|(?:(?<!\\\\)(?:"|\\w\\(0\\))(?:, NA)*?), ))'), '\\1\n') |>
    stringr::str_replace_all('\\n++', '\n')


  return(obj_expr)

}





#' Addin function to call [xlr::enscript()]
#'
#' Requires RStudio
#'
#' @return void
#' @export
#'
#'
#' @examples \dontrun{
#' # Highlight each text item below and press key-chord `ctrl + alt + shift + n`.
#' # To implement quick key-chord. Run {.fn xlr::set_xlr_key_chords}.
#'
#' (1:5 * 10)
#' rep("🎊🌈",3)
#' dplyr::starwars |> head()
#' letters
#'}
#'
run_enscript <- function(){

  if( !rstudioapi::isAvailable() ){
    cli::cli_abort(
      'RStudio is not available.
      {.fn run_enscript} is for interactive use in RStudio.'
    )
  }

  last_editor <- rstudioapi::documentId(allowConsole = FALSE)
  text_expr <- base::suppressWarnings(stringr::str_trim(rstudioapi::selectionGet(id = last_editor)$value))

  if( base::identical(text_expr, base::character(0)) || text_expr == '' ){
    cli::cli_bullets(c(
      "x" = '{.strong Nothing selected to {.fn xlr::enscript} }',
      "i" = "Highlight an expression in your text editor, then press `ctrl+alt+shift+n`."
    ))
  } else {
    rstudioapi::sendToConsole(code = base::paste(text_expr, '|> xlr::enscript()'))
  }


}


