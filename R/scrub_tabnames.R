#' Conform names to spreadsheet workbook standards
#'
#' `scrub_tabnames()` preserves as much of the original
#' tabnames as possible while ensuring each tabname is unique and contains only
#' characters permissible as spreadsheet tab names
#'
#' @details
#' Replaces:
#'
#' - characters: /\: with: -
#' - characters: ?*&$ with: #
#' - characters: \[< with: (
#' - characters: \]> with: )
#' - tabnames with the single quote or double quote: ' or " with a backtick `
#' - the word 'history' in a tabname, with 'hist' while preserving capitalization
#' - names greater than 31 characters with a truncated characters
#' - duplicate sheetnames with numbered versions of the name to make each name within that group unique
#'
#' @references
#' - \url{https://help.libreoffice.org/latest/he/text/scalc/guide/rename_table.html}
#' - \url{https://support.microsoft.com/en-us/office/rename-a-worksheet-3f1f7148-ee83-404d-8ef0-9ff99fbad1f9}
#' - \url{https://colinfay.me/writing-r-extensions/creating-r-packages.html}
#'
#'
#' @param tabnames strings: character vector of names
#' @param max_width integerish between 0 and 31: Default is 31 characters;
#'   longer names will cause Excel export to fail.
#' @param truncate_side 'right', 'left', or 'center': describes which side of a
#'   long tabname do you want to snip excess characters from
#' @param paste_side 'right' or 'left': describing which side of a tab name you
#'   want to paste characters to, if required
#' @param sep chars: if pasting characters to a tab name, what character do you
#'   want to use to separate name and unique prefix/suffix; can be several
#'   characters like '...', but cannot be any of the forbidden characters
#'   described in `details`
#' @param pad char: if pasting characters to a tab name, what character do you
#'   want to use to pad so numbers align, (e.g. '0' for '001' or '.' for '..1'
#'   if 3 digits of differentiation are necessary). Must be at least 1 character
#'   but no longer. Can be a space ' ', but cannot be any of the forbidden characters
#'   described in `details`
#' @param quiet bool: to turn off warnings if you prefer with quiet = TRUE
#'
#' @return a character vector of names suitable for Excel or Librecalc tab/sheet
#'   names
#'
#' @export
#'
#' @examples
#' scrub_tabnames(1:5)
#' c('\\:blue/:', 'red', 'gr?*een///////', '[]', '[orange]','', NA) |> scrub_tabnames()
#' c('history', NA, 'HISTORY', 'Entire History of the Universe',NULL,NULL) |> scrub_tabnames()
#' c("'\'don't worry\''", '`NA`', NA, "'HIS'T''", 'Entire History of \'the\' \'Universe\'') |> scrub_tabnames()
#' c('\\/:[]?*', '\\?:*/[]', '~!@#$%^&()-_=+{}|;:,<.> ') |> scrub_tabnames()
#' paste0(names(datasets::precip),'/',datasets::precip) |> scrub_tabnames(max_width = 10)
#' paste0(names(datasets::precip),'/',datasets::precip) |> scrub_tabnames(max_width = 8, sep = '_#', pad = '0')
#' paste0(names(datasets::precip),'/',datasets::precip) |> scrub_tabnames(max_width = 0, sep = '...', pad = '0')
#' paste0(names(datasets::precip),'/',datasets::precip) |> scrub_tabnames(max_width = 0, sep = '..', pad = '.')
#' rep('', 15) |> scrub_tabnames(max_width = 0, sep = '..', pad = '.')
#' # rep('', 15) |> scrub_tabnames(max_width = 0, sep = '', pad = '') # expect_fail
#' # rep('', 15) |> scrub_tabnames(max_width = 0, sep = '', pad = ' ') # expect_fail
#' # dplyr::starwars |> dplyr::mutate(new_name = paste(name,'of', homeworld)) |>
#' # dplyr::pull(new_name) |> scrub_tabnames(max_width = 20, sep = '//', pad = ']') # expect_fail
#' # dplyr::starwars |> dplyr::mutate(new_name = paste(name,'of', homeworld)) |>
#' # dplyr::pull(new_name) |> scrub_tabnames(max_width = 0, sep = '.', pad = '.') # expect_fail
#'
#' # when stringr::str_trunc() gets fixed, the below code will work.
#' # dplyr::starwars |> dplyr::mutate(new_name = paste(name,'of', homeworld)) |>
#' # dplyr::pull(new_name) |> scrub_tabnames(max_width = 0, sep = '', pad = '.') # ??????
#'
#' # if you request a width of characters that is fewer than your replacement characters, you get some weird looking names:
#' dplyr::starwars |> dplyr::mutate(new_name = paste(name,'of', homeworld)) |>
#' dplyr::pull(new_name) |> scrub_tabnames(max_width = 2, sep = '...', pad = '.')
#'
#' # However, if you call zero width..., that's useful for naming.
#' dplyr::starwars |> dplyr::mutate(new_name = paste(name,'of', homeworld)) |>
#' dplyr::pull(new_name) |> scrub_tabnames(max_width = 0)
#' dplyr::starwars |> dplyr::mutate(new_name = paste(name,'of', homeworld)) |>
#' dplyr::pull(new_name) |> scrub_tabnames(max_width = 0, sep = 'tab > ', pad = '0')
#' dplyr::starwars |> dplyr::mutate(new_name = paste(name,'of', homeworld)) |>
#' dplyr::pull(new_name) |> scrub_tabnames(max_width = 0,truncate_side = 'center')
scrub_tabnames <- function(tabnames,
                           max_width = 31,
                           truncate_side = c("right", "left", "center"),
                           paste_side = c("right", "left"),
                           sep = ".",
                           pad = ".",
                           quiet = FALSE) {


  # check inputs 'pad', 'sep', 'quiet', 'max_width', etc., and ensure 'width' request is reasonable
  if (is.null(tabnames)){ cli::cli_abort("{.fn scrub_tabnames} called with no input for {.var tabnames}.") }
  original_tabnames <- tabnames |> paste()
  n_tabs <- length(original_tabnames)
  pad <- .check_forbidden_pad(pad)
  checkmate::qassert(quiet, "B1")
  sep <- checkmate::assert_scalar(sep,  na.ok = TRUE, null.ok = TRUE)
  if( is.na(sep) || is.null(sep) ){ sep <- '' } else { sep <- .fix_forbidden_tabnames(sep, quiet = quiet) }
  if( !checkmate::test_string(sep, na.ok = FALSE, max.chars = 31 - nchar(n_tabs), null.ok = FALSE) ){
    cli::cli_abort(c(
      "The specficied separator {.var sep}
       cannot be longer than 31 characters less the {nchar(n_tabs)}
       character{?s} required to uniquely assign numbers to possible repeats.",
      "x" = "You supplied '{sep}', which is {nchar(sep)} character{?s}."
    ))
  }
  width <- .check_tabwidth(width = max_width, min_width = nchar(sep) + nchar(n_tabs), quiet = quiet)
  paste_side <- match.arg(paste_side[1], choices = c("right", "left"))
  truncate_side <- match.arg(truncate_side[1], choices = c("right", "left", "center"))
  ellipsis <- if( truncate_side == "center" && max_width > 0 ){ "~" } else { "" }


    # chop string, remove forbidden characters, and make unique
  tabnames <- original_tabnames |>
    stringr::str_trunc(width = width, side = truncate_side, ellipsis = ellipsis) |>
    .fix_forbidden_tabnames(quiet = quiet) |>
    .uniquify_tabnames(truncate_side = truncate_side, paste_side = paste_side,
                       width = width, sep = sep, pad = pad, ellipsis = ellipsis,
                       quiet = quiet)



  # it is possible to get duplicates when renaming by truncation; recurse if required
  if( any(repeated(tolower(tabnames)), na.rm = TRUE) || any(nchar(tabnames) > width, na.rm = TRUE) ){

    if( !quiet ){
      message("\n****************************************************************************")
      message("***** There are still issues after scrubbing names. Rerunning function *****")
      message("****************************************************************************\n\n")
    }

    tabnames <- scrub_tabnames(tabnames = tabnames, max_width = width,
                               truncate_side = truncate_side,
                               paste_side = paste_side, sep = sep,
                               pad = pad, quiet = quiet)

  }


  return(tabnames)

}

#################################################################################
### All functions below are helpers to {.fn scrub_tabnames} ---------------------
#################################################################################

# #' @family single table verbs
# #' @inheritParams args_by
# #' @inheritParams arrange
# #' @inheritParams filter


#' Corrects tabnames by replacing reserved chars
#'
#' Replacing special characters one-for-one and notifies the user of replaced characters.
#'
#' @note
#' NA always returned as NA
#' NULL acts like c(), i.e c(NULL, NULL)
#'
#' @seealso
#' [scrub_tabnames()]
#' [.forbidden_chars_replace()]
#'
#' @param tabnames vector of tab/sheet names
#' @param quiet TRUE or FALSE to suppress notices of forbidden-char replacement
#'
#' @return the input vector with forbidden characters replaced/removed
#'
#' @examples
#' c("' '?HisTory,*?*!@#$%^&*()_+ '[{1'}]'[:/\\]''", "", NA, NULL) |>  xlr:::.fix_forbidden_tabnames()
#' " ''''''" |> xlr:::.fix_forbidden_tabnames()
#' "'''1' '" |> xlr:::.fix_forbidden_tabnames()
#' "'[ ]hi];'" |> xlr:::.fix_forbidden_tabnames()
#' "!@#$%^&*()_+ {HI} ^" |> xlr:::.fix_forbidden_tabnames()
#' "'histor'y'" |> xlr:::.fix_forbidden_tabnames()
#' "[:/] " |> xlr:::.fix_forbidden_tabnames()
#' "?/a\\" |> xlr:::.fix_forbidden_tabnames()
#' "?,*?*?'" |> xlr:::.fix_forbidden_tabnames()
#' "[history coursework]" |> xlr:::.fix_forbidden_tabnames()
#' "'HISTORIC' History '''" |> xlr:::.fix_forbidden_tabnames()
#' "*[history' buff]*' " |> xlr:::.fix_forbidden_tabnames()
#' NA |> xlr:::.fix_forbidden_tabnames()
#' NULL |> xlr:::.fix_forbidden_tabnames()
#' c(NULL, NA, '[hi?]') |> xlr:::.fix_forbidden_tabnames()
#' c(NULL, NULL) |> xlr:::.fix_forbidden_tabnames()
#' enlist(NULL, NA, '[hi?]') |>  purrr::map(xlr:::.fix_forbidden_tabnames)
#'
.fix_forbidden_tabnames <- function(tabnames, quiet = FALSE) {
  tabnames <- as.character(tabnames)
  # On average, faster to check if any issues exist before calling replacements.
  if( any(stringr::str_detect(tabnames, pattern = "(^')|[\\\\/\":?*&<>\\[\\]]|(?i)history|('$)"),na.rm = TRUE) ){
  # if( any(stringr::str_detect(tabnames, pattern = "['\\\\/\":?*&\\[\\]]|(?i)history"),na.rm = TRUE) ){

    # tabnames <- .forbidden_chars_replace(tabnames, pattern = "[\\\\/:]", replacement = "-", pattern_text = "\\, /, or :", quiet = quiet)
    tabnames <- .forbidden_chars_replace(tabnames, pattern = "\"", replacement = "`", pattern_text = "tick quotes", repl_text = "`", quiet = quiet)
    # tabnames <- .forbidden_chars_replace(tabnames, pattern = "'", replacement = "`", pattern_text = "tick quotes", repl_text = "`", quiet = quiet)
    tabnames <- .forbidden_chars_replace(tabnames, pattern = "[\\\\]", replacement = "-", pattern_text = "\\", quiet = quiet)
    tabnames <- .forbidden_chars_replace(tabnames, pattern = "/", replacement = "-", pattern_text = "/", quiet = quiet)
    tabnames <- .forbidden_chars_replace(tabnames, pattern = ":", replacement = "-", pattern_text = ":", quiet = quiet)
    tabnames <- .forbidden_chars_replace(tabnames, pattern = "[*]", replacement = "#", pattern_text = "*", quiet = quiet)
    tabnames <- .forbidden_chars_replace(tabnames, pattern = "[?]", replacement = "#", pattern_text = "?", quiet = quiet)
    tabnames <- .forbidden_chars_replace(tabnames, pattern = "[&]", replacement = "#", pattern_text = "?", quiet = quiet)
    tabnames <- .forbidden_chars_replace(tabnames, pattern = "\\[", replacement = "(", pattern_text = "[", repl_text = "(", quiet = quiet)
    tabnames <- .forbidden_chars_replace(tabnames, pattern = "]", replacement = ")", pattern_text = "]", repl_text = ")", quiet = quiet)
    tabnames <- .forbidden_chars_replace(tabnames, pattern = "<", replacement = "(", pattern_text = "<", repl_text = "(", quiet = quiet)
    tabnames <- .forbidden_chars_replace(tabnames, pattern = ">", replacement = ")", pattern_text = ">", repl_text = ")", quiet = quiet)
    tabnames <- .forbidden_chars_replace(tabnames, pattern = "^'+", replacement = "`", pattern_text = "a single quote ' at the start of a tabname", repl_text = "an empty string", quiet = quiet)
    tabnames <- .forbidden_chars_replace(tabnames, pattern = "'+$", replacement = "`", pattern_text = "a single quote ' at the end of a tabname", repl_text = "an empty string", quiet = quiet)
    tabnames <- .forbidden_chars_replace(tabnames, pattern = "(?i)(hist)ory", replacement = "\\1", pattern_text = "the word 'history'", repl_text = "hist", quiet = quiet)
  }

  return(tabnames)

}


#' Replace forbidden characters in sheet/tab or file names with a warning
#'
#' This is a non-exported function called in [scrub_tabnames()] to alter
#' spreadsheet sheet names.
#'
#' @seealso [.fix_forbidden_tabnames()]
#'
#' @param names vector of tab/sheet names
#' @param pattern a regular expression describing forbidden characters
#' @param replacement the text taking the place of the forbidden characters
#' @param pattern_text description of characters being replaced in the pattern regex, default is a repeat of pattern
#' @param repl_text description of the replacements for forbidden chars, default is a repeat of replacement
#' @param quiet TRUE or FALSE to quiet notices of forbidden char replacement
#'
#' @return the input vector with forbidden characters replaced/removed
#'
#'
#' @examples
#' # for tests:
#' tab_names <- c(NA, "''''''", "[]hi];", "'HI'", "'hist''", "[:/]", "?/a\\\\",
#' "'?,*?*'", "'[history coursework]'", "'HISTORIC'AL''''")
#' xlr:::.forbidden_chars_replace(tab_names, pattern = '[\\\\/:]', replacement = '-',
#'                                pattern_text = '\\, /, or :')
#' xlr:::.forbidden_chars_replace(tab_names, pattern = '[?*]', replacement = '#',
#'                                pattern_text = '? or *')
#' xlr:::.forbidden_chars_replace(tab_names, pattern = '(?i)(hist)ory',
#'                                replacement = '\\1', pattern_text = 'history',
#'                                repl_text = 'hist')
#' xlr:::.forbidden_chars_replace(tab_names, pattern = '\\[', replacement = '{',
#'                                pattern_text = '[brackets]', repl_text = '{curly braces}')
#' xlr:::.forbidden_chars_replace(tab_names, pattern = ']', replacement = '}', quiet = TRUE)
#' xlr:::.forbidden_chars_replace(tab_names, pattern = "^'+", replacement = '`',
#'                                pattern_text = "single quotes '' at tabname start or end",
#'                                repl_text = "the empty string ''")
#' xlr:::.forbidden_chars_replace(tab_names, pattern = "'+$", replacement = '', quiet = TRUE)
#'
#'
.forbidden_chars_replace <- function(names, pattern = "", replacement = "",
                                     pattern_text = pattern,
                                     repl_text = replacement, quiet = FALSE) {

  if( any(stringr::str_detect(names, pattern), na.rm = TRUE) ){
    if( !quiet ){
      cli::cli_alert_warning( "Sheet names should not contain:  `{pattern_text}`" )
      cli::cli_alert( "Replacing forbidden character with:  `{repl_text}`" )
    }
    stringr::str_replace_all(names, pattern, replacement)
  } else {
    names
  }

}



#' Error for prohibited padding or more than 1 char
#'
#' @seealso [scrub_tabnames()]
#'
#' @inheritParams scrub_tabnames
#'
#' @return a single character; throws an error if input is any of these characters \/:?*'[]
#'
#' @examples
#' xlr:::.check_forbidden_pad('.')
#' xlr:::.check_forbidden_pad('-')
#' \dontrun{
#'
#' # expect errors for the below: \/:?*'[]
#'
#' xlr:::.check_forbidden_pad(':')
#' xlr:::.check_forbidden_pad('')
#' xlr:::.check_forbidden_pad(NA)
#' xlr:::.check_forbidden_pad(NULL)
#' xlr:::.check_forbidden_pad("\\")
#'
#' }
#'
.check_forbidden_pad <- function(pad = "") {

  checkmate::assert_string(pad, n.chars = 1)

  if (stringr::str_detect(pad, "[\\\\/:?*\"\\[\\]']")) {
    cli::cli_abort(
      "The {.var pad} argument cannot include the character '{pad}'
        because spreadsheets cannot include it within a tabname."
    )
  }
  pad
}



#' Update permissible number of tabname characters
#'
#' @description
#' Adjust the specified tab/sheet names character count to be within reasonable
#' values and notify user with a warning when changes are made
#'
#' Note: min checks should already have been done in scrub_tabnames() before passing width info
#'
#' @param width the number of characters user requested for the tab/sheet names
#' @param min_width the minimum number of characters to permit in a tab/sheet name
#' @param quiet quiet overrides to the tab/sheet names
#'
#' @return between 0 and 31 where returned number is as close to user request
#'   as possible after adjusting for reasonableness on input data
#'
#' @examples
#' xlr:::.check_tabwidth(31)
#' xlr:::.check_tabwidth(3, 4)
#' xlr:::.check_tabwidth(34)
#'
.check_tabwidth <- function(width = 31, min_width = 0, quiet = FALSE) {

  # checkmate::qassert(width, "X1[0,)")
  checkmate::assert_integerish(width, lower = 0, len = 1, null.ok = FALSE, any.missing = FALSE)

  # update user-input length of tabnames (nchar(sep) + nchar(length(original_names)))
  if (width < min_width) {
    width <- min_width
    if (!quiet) {
      cli::cli_alert_warning(c(
        "The minimum width must be greater than the number of characters required
        to differentiate possible duplicated tabnames plus the characters in {.var sep}.
        \nSetting the tabname character width to {min_width}.\n"
        )
      )
    }
  } else if (width > 31) {
    # warn and reset max_width to 31 if greater
    width <- 31
    if (!quiet) {
      cli::cli_alert_warning(
        "Excel limits sheetnames to 31 characters.
        Longer names cause write failure.
        Setting max_width to 31 characters and truncating longer names.")
    }
  }

  return(width)
}



#' Paste name components together in order specified by paste_side
#'
#' @param prefix_suffix vector of padded numbers
#' @param tabnames vector of tab/sheet names
#' @param sep separator characters
#' @param paste_side 'right' or 'left', the side to attach the unique numbering to
#'
#' @return a vector of glued tabnames
#'
#' @examples
#' xlr:::.paste_names('--1', c('hello','goodbye'))
#' xlr:::.paste_names('..1', c('hello','goodbye'), '->', 'left')
#' xlr:::.paste_names(NA, c('hello','goodbye'), '->', 'left')
.paste_names <- function( prefix_suffix, tabnames, sep = ".", paste_side = "right" ){
  if( paste_side == "left" ){
    paste0(prefix_suffix, sep, tabnames)
  } else {
    paste0(tabnames, sep, prefix_suffix)
  }
}




#' Glue sequential padded numbers within each group of repeated names
#'
#' Note: tabname cleaning and parameter checks should already have been done in
#'  scrub_tabnames before passing args. This function is recursive and can
#'  call itself forever if width, sep, and pad aren't checked in advance.
#'
#'
#' @param tabnames character vector of tab/sheet names
#' @param truncate_side snip excess tabnames on 'left', 'right', or 'center'
#' @param paste_side 'left', 'right' for side of the tabnames to tack on rownumbers
#' @param width number of chars allowed in the tab/sheet name
#' @param sep tabname text and rownumber separator
#' @param pad padding characters used on left side of group rownumbering
#' @param ellipsis replacement characters during truncation
#' @param quiet quiet warning reports
#'
#'
#' @return vector or unique tabnames
#'
#' @examples
#' tabnames = c('', NULL, NULL, NA, NA, 'a', 'a', 'a', 'b', 'b', 'c', rep('d',11))
#' xlr:::.uniquify_tabnames(tabnames)
#' xlr:::.uniquify_tabnames(tabnames = tabnames, paste_side = 'left' , sep = '|')
#' xlr:::.uniquify_tabnames('supercalifragalisticexpealidocious')
#' xlr:::.uniquify_tabnames(c('supercalifragalisticexpealidocious','supercalifragalisticexpealidocious'), width = 10,  truncate_side = 'center', ellipsis = '~')
#' xlr:::.uniquify_tabnames(c('supercalifragalisticexpealidocious','supercalifragalisticexpealidocious'),truncate_side = 'center', ellipsis = '~', paste_side='left')
#'
.uniquify_tabnames <- function(tabnames, truncate_side = "right", paste_side = "right",
                               width = 31, sep = "->", pad = ".", ellipsis = "", quiet = FALSE) {

  # NULLs are to correct for: "no visible binding for global variable"
  n_obs <- snipped_names <- uniq_numbering <- uniq_tabnames <- NULL


  if( is.null(tabnames) ){ return() }
  checkmate::assert_string(pad, n.chars = 1)

  # initial truncation
  tabnames <- tabnames |>
    stringr::str_trunc(width = width, side = truncate_side, ellipsis = ellipsis) |>
    .fix_forbidden_tabnames(quiet = quiet)

  is_repeated <- repeated(tolower(tabnames)) | tabnames == ""
  if( any(is_repeated) ){

    if( !quiet ){
      cli::cli_alert_warning("Duplicate or empty tab/sheet names exist. Numbering within each group ... ")
    }

    # make tibble to update names by group
    repaired_names_tbl <- enlist(is_repeated, tabnames) |>
      dplyr::bind_cols() |>
      dplyr::group_by(tolower(tabnames)) |>
      dplyr::mutate(n_obs = dplyr::n(),
                    snipped_names = dplyr::if_else(
                      is_repeated,
                      stringr::str_trunc(
                        tabnames,
                        side = truncate_side,
                        ellipsis = ellipsis,
                        width = width - nchar(sep) - nchar(sum(is_repeated))
                        ),
                      tabnames
                    ),
                    uniq_numbering = dplyr::if_else(
                      is_repeated, stringr::str_pad(
                        dplyr::row_number(),
                        width = nchar(n_obs), side = "left", pad = pad), ""
                    )
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        uniq_tabnames = dplyr::if_else(
          is_repeated,
          .paste_names(uniq_numbering, snipped_names,sep, paste_side),
          snipped_names
        )
      )


    tabnames <- dplyr::pull(repaired_names_tbl, uniq_tabnames)

  }



  # return ---------------------------------------------------------------
  # check if tabnames are unique; if not:
  #  recall .uniquify_tabnames() with snipped_names instead of uniq_tabnames
  if( any(repeated(tabnames)) ){
    if( !quiet ){
    cli::cli_alert_warning("Duplicates remain after replacements. Trying again ... ")
    }
    dplyr::pull(repaired_names_tbl, snipped_names) |> .uniquify_tabnames(
      truncate_side = truncate_side, paste_side = paste_side, width = width,
      sep = sep, pad = pad, ellipsis = ellipsis, quiet = quiet
    )

  } else {
    tabnames
  }


}


