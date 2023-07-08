#' Flatten an embedded object list to a single flat list
#'
#' @description
#' Recursively flattens an embedded object list
#' Calls [entibble] to coerce internal objects to tibbles
#' Concatenate embedded-list names with name_spec like '|' or '_'.
#'
#' @param ... A data object, data objects, or a list of data objects.
#' @inheritParams purrr::list_flatten
#' name_spec
#' name_repair
#' @param .f function to apply at every leaf of the dis-embedded list; default is `entibble`
# @inheritParams purrr::list_flatten return
# @inheritDotParams purrr::list_flatten foo name_spec name_repair
#'
#' @return A flattened list of tibbles.
#' @export
#'
#' @examples
#' enflatten(iris, mtcars)
#'
#' list(c(list(1:5), a = list(5:1, 'green', list('blue'))), purrr::set_names(letters, LETTERS)) |> str()
#' list(c(list(1:5), a = list(5:1, 'green', list('blue'))), purrr::set_names(letters, LETTERS)) |>
#'   enflatten() |> str()
#' (embedded_list <- list(list(list(1:5), a = list(5:1, 'green', list('blue')), purrr::set_names(letters, LETTERS)) |> list(head(iris), tail(mtcars))))
#' embedded_list |> str()
#' embedded_list |> names()
#' embedded_list |> enflatten() |> str()
#' embedded_list |> enflatten() |> names()
#' embedded_list |> enflatten(name_repair = 'unique') |> names()
#' embedded_list |> enflatten(name_spec = '{outer}_outer_|{inner}_inner_') |> names()
#' embedded_list |> enflatten(name_spec = '', name_repair = 'unique') |> names()
#' list(list(list(1:5), list(5:1, 'green', list('blue')), purrr::set_names(letters, LETTERS)) |>
#' list(head(iris), tail(mtcars))) |> enflatten(name_spec = 'tbl_{inner}') |> names()
#' list(list(list(1:5), list(5:1, 'green', list('blue')), purrr::set_names(letters, LETTERS))) |>
#' list(head(iris), tail(mtcars)) |> enflatten(name_spec = 'lst_grp1.{inner}') |> names()
#' some_list <- list(list(grp_a = 2:1, grp_b = list(b1 = 2, b2 = list(list(f = iris))), grp_c = list(c2 = 3, c1 = 4)))
#' enflatten(some_list)
#'
enflatten <- function(...,
                      name_spec = "{outer}|{inner}",
                      name_repair = c("minimal", "unique", "check_unique","universal"),
                      .f = ~entibble(.)
                      ) {
    xlr::enlist(...) |>
        purrr::modify_tree(leaf = rlang::as_function(.f), post = ~purrr::list_flatten(., name_spec = name_spec, name_repair = name_repair))
}
#' tests
#' as.list(letters) |> enflatten()
#' list(c(list(1:5), list(5:1)), letters) |> enflatten()
#' list(a = 1,b=5, 1:10, letters,  ) |> xl()
#' c(1,1:5, list(1:10)) |> xl()
#' a <- tibble::tibble(iris); xl(a)
#' xl(NA)
#' xl(NULL)
#' xl(1)
#' 1:26 |> purrr::set_names(letters) |> xl()
#' as.matrix(warpbreaks[1:10,])
#' a <- list()
#' a$tori = list(
#'   c = list(b = iris),
#'   d = mtcars
#' )
#' a$b$f$e <- tibble::tibble(a=c(1,2,3), b=c(1,2,45))
