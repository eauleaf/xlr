#' Adds breaks to the text of a deparsed data structure script
#'
#' @param obj_expr text for a data structure
#' @param editor_width limit for the number of characters desired in data folding
#'
#' @return
#' @export
#'
#' @examples
format_script <- function(obj_expr, editor_width = 70) {

  checkmate::assert_integerish(editor_width, lower = 4, len = 1, any.missing = FALSE)


  if( nchar(obj_expr) <= cli::console_width() ) {
    return(obj_expr)
  }

  # 't("head(iris)" = s' |> stringr::str_replace_all('(?<=[(])"(.*?[)])"(?= = \\w)')
  # obj_expr <- obj_expr |> stringr::str_replace_all('(?<=[(])(?:"(.*?[)])")+?(?= = \\w)', "`\\1`")
#   obj_expr <- obj_expr |> stringr::str_replace_all('(?<=\\()"(.+?\\))" = ', "`\\1`")
#   print(obj_expr)
# return('hi')

  # separate everything encased in quotes
  obj_expr <- obj_expr |>
    stringr::str_replace_all('("(?:[^"]|\\\\")*(?<!\\\\)",? ?)', '\n\\1\n') |>
    stringr::str_split_1('\\n')

  quoted <- obj_expr |> stringr::str_detect('^"')

  # format non-quoted expression text
  regex_breaks <- c(
    '\\('
    ,'(\\),? ?)'
    ,'([{}])'
    ,'(^[\\w.]++\\()'
    ,'\\n++'
    ,paste0('(.{1,',editor_width-3,'}(?:\\n++|, ))')
    ,'\\n++'
    ,'\\(\\n(.?)\\n\\)'
  )

  regex_replacements = c(
    '(\n'
    ,'\n\\1\n'
    ,'\n\\1\n'
    ,'\n\\1'
    ,'\n'
    ,'\\1\n'
    ,'\n'
    ,'(\\1)'
  ) |> rlang::set_names( regex_breaks )

  obj_expr[!quoted] <- obj_expr[!quoted] |> stringr::str_replace_all(regex_replacements)

  # glue everything back togeter and break within quoted expressions
  obj_expr <- obj_expr |> glue::glue_collapse() |>
    stringr::str_replace_all('NA, \\n"', 'NA, "') |>
    stringr::str_replace_all(paste0('(.{1,',editor_width-2,'}(?:\\n++|(?:(?<!\\\\)"(?:, NA)*?), ))'), '\\1\n') |>
    stringr::str_replace_all('\\n++', '\n')


  return(obj_expr)

}
