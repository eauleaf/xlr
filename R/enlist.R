#' Create a list with auto-named list elements.
#'
#'
#' @description
#' When passed a list, `enlist()` does not add an additional list layer.
#' Use in place of [list] to produce a list with auto-assigned names and to avoid adding excess list structure.
#'
#' @details Specifically:
#' -- `enlist` creates a list where each list element is force-named by the input expression if no list element name is available, .e.g  `enlist(mtcars)`.
#'
#' -- If no list exists, `enlist` places its arguments into a list, e.g. `enlist(c('hi', 'hello'))`.
#'
#' -- If passed a single bare list, `enlist` does not add an additional list layer. That is, enlist() doesn't stack lists for structure's sake alone, e.g. `enlist(enlist(letters))`.
#'
#' -- `enlist` takes dots, splicing, and injection, and is okay to use in non-standard evaluation or meta-programming, e.g. enlist(!!!letters).
#'
#' -- `enlist` ignores input argument separator commas, e.g. enlist(,,,,,).
#'
#' -- Naming can be performed by function through the parameter `.label`, e.g. enlist('me', .label = ~paste0('name_',.))
#'
#'
#' @param ... data objects; if unnamed, enlist() forces names by input expression
#' @param .label a function or character vector to rename list elements, e.g. .label = ~substr(.,1,5).
#' `.label` is passed to `nm` in 'rlang::set_names()'
#'
#'
#' @return a named list
#' @export
#'
#' @examples
#' # Examples compare `enlist()` to `list()`:
#'
#' # Auto-naming by input expression:
#' enlist(head(iris), tail(mtcars))
#' list(head(iris), tail(mtcars))
#'
#' # Naming a list element works similar to `list()`:
#' enlist(some_name = letters)
#' list(some_name = letters)
#'
#' # Naming by passing a function to `.label`:
#' enlist(letters, .label =  'some_name')
#' enlist(letters, .label =  ~paste0(.,'_1'))
#' enlist('black','white','cyan', .label =  'color_grp_1')
#' enlist(letters, .label =  ~'')  # removes names
#'
#'
#' # List embedding:
#' list(list(list(letters))) # 3 lists deep
#' enlist(enlist(enlist(letters))) # 1 list deep
#'
#' # This dis-embedding behavior can be useful in a function when the dots `...` can be arguments or a list of arguments.
#'
#' # Handles non-standard evaluation:
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
    # keep embedded list names if they exist, otherwise overwrite with outer-list name
    if( identical(names(evaled_list),NULL) ){
      evaled_list <- evaled_list |> rlang::set_names(nm = names(.quos))
    }
  }

  out <- evaled_list |> .remove_doublequotes()

  if (!is.null(.label)){
    out <- out |> rlang::set_names(nm = .label)
  }

  return( out )

}



#' Removes external set of quotes for named list if the list names have double-quotes
#'
#' @param named_list list with names
#'
#' @return named list where names have no double-quotes
#'
#' @examples
#' tibble::tibble(!!!letters) |> as.list() |> .remove_doublequotes()
.remove_doublequotes <- function(named_list = NULL){
  # remove external-quotes from list with names
  double_quoted <- names(named_list) |> stringr::str_detect('^".*"$')
  if( any(double_quoted, na.rm = TRUE)){
    names(named_list)[double_quoted] <- names(named_list)[double_quoted] |>
      stringr::str_extract('(?<=^").*(?="$)')
  }

  return(named_list)

}


