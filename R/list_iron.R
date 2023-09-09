#' Creates a single flat list of objects.
#' If given a list of objects embedded within lists, [list_iron] dis-embeds lists
#' to a single, linear list of objects.
#' If object is not in a list, [list_iron] places the object in a list.
#' Removes all empty embedded lists.
#'
#'
#' @details
#' Because spreadsheets tabs have no hierarchical structure, any list of data
#' objects to prepare for presentation in a workbook must be flattened to sheets.
#' Thus, this function is a precurser for printing listed data objects with [xlr].
#' [list_iron] requires all objects in an embedded object list to form a
#' single-file list, and optionally conform to a function passed by the user, e.g. [tibble].
#' Specifically, [list_iron] recursively flattens an embedded object list, acting
#' similarly to [purrr::list_flatten], but collapsing all embedded lists rather
#' than a single layer. Concatenates embedded-list names with `name_spec`.
#'
#' @param ... A data object, data objects, or a list of data objects.
#' @inheritParams purrr::list_flatten
#' name_spec
#' name_repair
#' @param .f function to apply at every leaf of the dis-embedded lists; default function is `identity`
# @inheritParams purrr::list_flatten return
# @inheritDotParams purrr::list_flatten foo name_spec name_repair
#'
#' @return A list flattened to one list deep, with function, if specified, applied to each item of the list
#' @export
#'
#' @examples
#'
#' head(iris) |> list_iron()
#' head(iris) |> list() |> list() |> list()
#' head(iris) |> list() |> list() |> list() |> list_iron()
#' list_iron(list(list(iris)), mtcars, .f = head)
#' list_iron(list(list(iris)), mtcars, .f = entibble)
#'
#' Compare to purrr::list_flatten:
#' list(list(1, list(), 2, list(3))) |> str()
#' list(list(1, list(), 2, list(3))) |> purrr::list_flatten() |> str()
#' list(list(1, list(), 2, list(3))) |> list_iron() |> str()
#'
#' list(c(list(1:5), a = list(5:1, 'green', list('blue'))), purrr::set_names(letters, LETTERS)) |> str()
#' list(c(list(1:5), a = list(5:1, 'green', list('blue'))), purrr::set_names(letters, LETTERS)) |> list_iron() |> str()
#'
#' Naming examples:
#' (embedded_list <- list(list(list(1:5), a = list(5:1, 'green', list('blue')), purrr::set_names(letters, LETTERS)) |> list(head(iris), tail(mtcars))))
#' embedded_list |> str()
#' embedded_list |> names()
#' embedded_list |> list_iron() |> str()
#' embedded_list |> list_iron() |> names()
#' embedded_list |> list_iron(name_repair = 'unique') |> names()
#' embedded_list |> list_iron(name_spec = '', name_repair = 'unique') |> names()
#' embedded_list |> list_iron(name_spec = '{outer}', name_repair = 'unique') |> names()
#' embedded_list |> list_iron(name_spec = '{inner}', name_repair = 'unique') |> names()
#' embedded_list |> list_iron(name_spec = '{outer}|{inner}', name_repair = 'unique') |> names()
#'
#' Other examples:
#' list() |> list_iron()
#' list(c(list(1:5), list(5:1)), letters) |> list_iron()
#' letters |> head() |> as.list() |> list_iron(name_spec = '{inner}')
#'
#' Very messy, embedded list:
#' messy_list <- list()
#' messy_list$tori <- list(
#'   c = list(b = iris),
#'   d = mtcars,
#'   e = list(list(grp_a = 2:1, grp_b = list(b1 = 2, b2 = list(list(f = iris))), grp_c = list(c2 = 3, c1 = 4)))
#' )
#' messy_list$b$f$w <- list(as.matrix(warpbreaks[1:10,]), tibble::tibble(a=c(1,2,3), b=c(1,2,45)))
#'
#' Iron out the messy list:
#' messy_list |> list_iron(.f = ~tail(., 2))
#'
list_iron <- function(...,
                      name_spec = "{outer}|{inner}",
                      name_repair = c("minimal", "unique", "check_unique","universal", "unique_quiet", "universal_quiet"),
                      .f = identity
                      ) {
  xlr::enlist(...) |>
    purrr::modify_tree(leaf = rlang::as_function(.f), post = ~purrr::list_flatten(., name_spec = name_spec, name_repair = name_repair)) |>
    suppressMessages()
}
