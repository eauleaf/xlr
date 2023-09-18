#' Iron embedded lists out of a list of objects
#'
#' Recursively dis-embed objects in lists of lists to a single, linear list of the objects.
#' If object is not in a list, `list_iron` places the object in a list.
#' Removes all empty embedded lists.
#' Removes empty comma-separated arguments.
#'
#'
#' @details
#' Because spreadsheets tabs have no hierarchical structure, any list of data
#' objects to prepare for presentation in a workbook must be flattened to sheets.
#' This function is a precursor for printing listed data objects with [xlr].
#' `list_iron` requires all objects in an embedded object list to form a
#' single-file list, and optionally conform to a function passed by the user, e.g. [entibble].
#' Specifically, `list_iron` recursively flattens an embedded object list, acting
#' similarly to [purrr::list_flatten], but collapsing all embedded lists rather
#' than a single layer. Concatenates embedded-list names via parameter `name_spec`.
#'
#' @inheritParams purrr::list_flatten
#' @param ... objects or list of objects.
#' @param .f function to apply to every object in the list; default function is `identity`.
#'
#' @return A flat named list of objects
#' @export
#'
#' @examples
#' head(iris) |> list_iron()
#' head(iris) |> list() |> list() |> list() |> list_iron()
#' list_iron(list(list(iris)), mtcars, .f = ~tail(., 2))
#'
#' # Compare list structure to purrr::list_flatten():
#' a_list <- list(list(1, list(), 2, list(3)))
#' a_list |> str()
#' a_list |> purrr::list_flatten() |> str()
#' a_list |> list_iron() |> str()
#'
#' # Naming examples:
#' messy_list <- list(list(list(1:5), a = list(5:1, 'green', list('blue')), letters), list('red', list('orange')))
#' messy_list |> str()
#' messy_list |> list_iron() |> str()
#' messy_list |> list_iron(name_repair = 'unique') |> names()
#' messy_list |> list_iron(name_spec = '', name_repair = 'unique') |> names()
#' messy_list |> list_iron(name_spec = '{outer}', name_repair = 'unique') |> names()
#'
list_iron <- function(...,
                      name_spec = "{outer}|{inner}",
                      name_repair = c("minimal", "unique", "check_unique","universal", "unique_quiet", "universal_quiet"),
                      .f = identity
) {

  checkmate::assert_string(name_spec, null.ok = FALSE, na.ok = FALSE)

  # out <- rlang::dots_list(..., .named = TRUE) |>
  out <- enlist(...) |>
    purrr::modify_tree(leaf = rlang::as_function(.f), post = ~purrr::list_flatten(., name_spec = name_spec, name_repair = name_repair)) |>
    suppressMessages()

  if( identical(out, list()) ){ out <- structure(list(), names = character(0)) }

  return(out)

}



