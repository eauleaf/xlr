#' Create a list with auto-named list elements.
#' If passed a list, `enlist()` does not add an additional list layer.
#'
#' @description
#' Use in place of list() to produce a list with auto-assigned names and to avoid adding excess list structure.
#'
#' @details Specifically:
#' -- `enlist` creates a list where each list element is force-named by the input expression if no list element name is provided, .e.g  `enlist(mtcars)`.
#'
#' -- If no list exists, `enlist` places its arguments into a list, e.g. `enlist(greetings = c('hi', 'hello'))`.
#'
#' -- If passed a single bare list, `enlist` does not add an additional list layer. That is, enlist() doesn't stack lists for structure's sake alone, e.g. `enlist(enlist(letters))`.
#'
#' -- `enlist` takes dots, splicing, and injection, and is okay to use in meta-programming, e.g. enlist(!!!letters).
#'
#' -- `enlist` ignores all input argument separator commas, e.g. enlist(,,,,,).
#'
#' -- Naming can be performed by function through the parameter `.label`, e.g. .label = ~paste0('name_',.)
#'
#' --
#'
#'
#' @param ... data objects; if unnamed, enlist() forces names by input expression
#' @param .label a function or character vector to rename list elements, e.g. .label = ~substr(.,1,5).
#' `.label` is passed directly to `nm` in 'rlang::set_names()'
#'
#'
#' @return an evaluated named list
#' @export
#'
#' @examples
#' # Examples to compare `enlist()` to `list()`:
#'
#' # Auto-naming by input expression:
#' list(head(iris), tail(mtcars))
#' enlist(head(iris), tail(mtcars))
#'
#' # Naming a list element works similar to `list()`:
#' list(some_name = letters)
#' enlist(some_name = letters)
#'
#' # Naming by passing a function to `.label`:
#' enlist(letters, .label =  'some_name')
#' enlist(letters, .label =  ~paste0(.,'_1'))
#' enlist('black','white','cyan', .label =  'color_grp_1')
#' letters |> enlist( .label =  ~'')  # removes names
#'
#'
#' # List embedding:
#' list(list(list(letters))) # 3 lists deep
#' enlist(enlist(enlist(letters))) # 1 list deep
#'
#' # This dis-embedding behavior can be useful in a function when the dots `...` can be arguments or a list of arguments.
#'
#' # Non-standard evaluation:
#' candy <- list('lollipops','gum')
#' enlist(candy, !!!candy)
#'
enlist <- function(..., .label =  NULL){

  .quos <- rlang::quos(...) |> rlang::exprs_auto_name()
  .quos <- .quos[names(.quos) != "<empty>"]
  evaled_list <- .quos |>  purrr::map(rlang::eval_tidy)

  # dis-embed list if user passed just a single expression that evals to a bare list
  if( length(.quos) == 1 && rlang::is_bare_list(evaled_list[[1]]) ){
    evaled_list <- evaled_list[[1]]
  }

  out <- rlang::set_names(evaled_list, nm = names(.quos)) |> .remove_doublequotes()

  if (!is.null(.label)){
    out <- out |> rlang::set_names(nm = .label)
  }

  return( out )

}

# quos_raw <- rlang::quos(...)
# .quos <- quos_raw |> rlang::exprs_auto_name()
# quos_raw <- quos_raw[names(.quos) != "<empty>"]
# is.null(.label) &&
# identical(names(quos_raw), "")


# #### ORIGINAL
# enlist <- function(..., .label =  NULL) {
#
#
#     .quos <- rlang::quos(...) |>
#         rlang::exprs_auto_name()
#     .quos <- .quos[names(.quos) != "<empty>"]
#
#     .quo <- purrr::pluck(.quos, 1)
#
#     # if quos list has only 1 quo, and that quo is a list, pull quo and rewrite its expr
#     # w/auto-names then place rewritten expr in orig quo (to preserve orig expr names) and eval,
#     # otherwise, just map eval every item in the orig quos list
#     # return(.quos)
#     # if (length(.quos) == 1 && is.null(.label) && stringr::str_detect(base::names(.quos), "^(?:\\()*?(?:xlr::|base::|rlang::|<)?(?:en|dots_)?list2?[(>]") ){
#     if (length(.quos) == 1 && is.null(.label) && stringr::str_detect(rlang::as_label(.quo), "^(?:\\()*?(?:xlr::|base::|rlang::|<)?(?:en|dots_)?list2?[(>]") ){
#
#         rewritten_expr <- .quo |>
#             rlang::quo_get_expr()
#
#           out_list <- .quo |>
#             rlang::quo_set_expr(rewritten_expr) |>
#             rlang::eval_tidy()
#
#     } else if(length(.quos) == 1 && stringr::str_detect(rlang::as_label(.quo), "^(?:\\()*?(?:base::)?as\\.list?\\(")) {
#
#       out_list <- .quo |> rlang::eval_tidy() |> rlang::set_names(nm = ~names(.quos))
#
#     } else {
#
#         out_list <- .quos |>  purrr::map(rlang::eval_tidy)
#
#     }
#     # out_list <- rlang::dots_list(..., .ignore_empty = 'all', .named = TRUE)
#
#
#     # remove external-quotes from names
#     out_list <- .remove_doublequotes(out_list)
#
#
#     # if list elements need to be named by user-function
#     if (!is.null(.label)) {
#       out_list <- out_list |> rlang::set_names(nm = .label)
#     }
#
#
#     return(out_list)
#
# }



#' Removes external set of quotes for named list where some names have double-quotes
#'
#' @param named_list
#'
#' @return named list where names have no doublequotes
#'
#' @examples
#' tibble::tibble(!!!letters) |> as.list() |> .remove_doublequotes()
.remove_doublequotes <- function(named_list = NULL){
  # remove external-quotes from list with names
  double_quoted <- names(named_list) |> stringr::str_detect('^".*"$')
  if( base::any(double_quoted, na.rm = TRUE)){
    base::names(named_list)[double_quoted] <- base::names(named_list)[double_quoted] |>
      stringr::str_extract('(?<=^").*(?="$)')
  }

  return(named_list)

}


