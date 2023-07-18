#' Convert a data object or list of data objects to a tibble with rownames
#'
#' Same as [tibble()] with different defaults.
#' [entibble]:
#'  * Always includes rownames as a column
#'  * Is not concerned with duplicate column names unless specified by user with .name_repair
#'  * Does not repeat input elements to obtain common lengths
#'  * Transforms lists with uniform arrays into columns of a tibble
#'  * Treats ragged list of input elements similar to [tibble()] but includes names as column if possible
#'  * Flattens lists of lists to
#'  flat table if possible
#'
#'  @details
#'  Entibble is designed to work with the function [xl()] to produce a viewable
#'  output of presented data. To help ensure [xl()] succeeds, [entibble()] is
#'  liberal about names and makes an effort to produce a single flat table.
#'
#'
#' @param ... object or expression to convert to a tibble
#' @param .rowname String to name the column of rownames if passed a data object with rownames or names
#' @param .name_repair
#'
#' @seealso [tibble::tibble()]
#'
#' @return a tibble with rownames in a column
#' @export
#'
#' @examples
#' letters |> entibble()
#' letters |>  purrr::set_names(LETTERS) |> entibble()
#' letters |>  purrr::set_names(LETTERS) |> entibble(.rowname = 'user-specified rowname')
#' (example_data <- letters |>  purrr::set_names(LETTERS))
#' example_data |> entibble()
#' example_data |> entibble(.rowname = 'user-specified rowname')
#' example_data |> entibble(.name_repair = ~c('name_A', 'name_B'))
#' tibble::tibble(example_data, !!!letters) # ragged data
#' entibble(example_data, !!!letters) # ragged data
#' entibble(!!!letters)
#' tibble::tibble(!!!letters)
#' entibble(example_data, example_data)
#' tibble::tibble(example_data, example_data)
#'
#' letters |> purrr::set_names(LETTERS) |> as.list() |> tibble::tibble()
#' letters |> purrr::set_names(LETTERS) |> as.list() |> entibble()
#'
#'
#' enlist(head(iris), tail(mtcars)) |> tibble::tibble()
#' enlist(head(iris), tail(mtcars)) |> entibble()

#' enlist(head(iris), head(mtcars)) |> entibble()
#' entibble(head(iris), head(mtcars))
#' tibble::tibble(head(iris), head(mtcars))
#' entibble(head(iris), head(mtcars, 11))
#' tibble::tibble(head(iris), head(mtcars,11))
#'
#'
#' tibble::tibble(y = 1:3,x = 11:13)
#' entibble(y = c(1, 2, 3),x = c(11, 12, 13))
#' tibble::tibble(x = 1:3,x = 11:13)
#' entibble(x = 1:3, x = 11:13)
#' entibble(x = 1:3,x = 10:14, .name_repair = 'minimal')
#' tibble::tibble(x = 1:3,x = 10:14, .name_repair = 'minimal')
#' tibble::tibble(x = 1:3,x = 11:13, .name_repair = 'unique')
#' entibble(x = 1:3,x = 11:13, .name_repair = 'unique')
#' entibble(x = 1:3,x = 11:13, .name_repair = 'unique_quiet')
#' entibble(x = 1:3,x = 11:13, .name_repair = ~scrub_tabnames(., sep = ' <-- ', quiet = T))
#' enlist(head(iris), head(mtcars))
#'
#' tibble::tibble(x = 1:3,x = 11:13, .name_repair = ~scrub_tabnames(., sep = ' <-- ', quiet = T))
#'
#' as.matrix(1:10) |> rownames()
#' as.matrix(1:10) |> entibble()
#' as.matrix(list(1:10,11:20)) |> entibble()
#' as.matrix(list(1:10,11:20)) |> tibble::tibble()
#' as.matrix(1:10) |> tibble::tibble()
#' as.matrix(warpbreaks[1:10, ]) |> tibble::has_rownames()
#' as.matrix(warpbreaks[1:10, ]) |> entibble()
#' # ignores unused rownames
#' as.matrix(warpbreaks[1:10, ]) |> entibble(.rowname = 'row_nums')
#' matrix(c(1, 2, 3, 11, 12, 13), nrow = 2, ncol = 3, byrow = TRUE, dimnames = list(c('row1', 'row2'), c('C.1', 'C.2', 'C.3'))) |> entibble()
#' tail(iris) |> tibble::has_rownames()
#' tail(iris) |> entibble()
#' mtcars |> entibble()
#' mtcars |> entibble(.rowname = 'vehicle')
#' 5 |> entibble()
#' NA |> entibble()
#' NULL |> entibble()
#' enlist(head(iris), head(mtcars)) |> purrr::map(entibble)
#' enlist(letters, LETTERS) |> purrr::map(entibble)
#' enlist(letters, LETTERS) |> purrr::map(entibble, .name_repair = ~'a-z')
#' enlist(head(iris), head(mtcars)) |> entibble()
#' head(iris) |> purrr::map(entibble)
#' entibble(!!!head(iris))
#' enlist(head(iris), head(mtcars)) |> entibble()
#' enlist(head(iris), head(mtcars)) |> tibble::tibble()
#'
#' letters |> purrr::set_names(LETTERS) |> tibble::tibble()
#' letters |> purrr::set_names(LETTERS) |> entibble()
#'
#' letters |> purrr::set_names(LETTERS) |> tibble::enframe( name = 'row', value = 'blue')
#'
#' # If rowname is specified, but the dataframe has no rownames, rowname is ignored.
#' entibble(iris, , .rowname = 'flower_name')
#' letters |> as.list() |> tibble::tibble()
#' letters |> as.list() |> entibble(.rowname = 'a-z')
#' letters |> list() |> tibble::tibble()
#' letters |> list() |> entibble(.rowname = 'a-z')
#' letters |> enlist() |> entibble(.rowname = 'a-z')
#'
#' colours() |> rlang::set_names() |> entibble() |> entibble(another_col = colours())
#' colours() |> rlang::set_names() |> entibble(another_col = colours())
#' letters |> as.list() |> enlist() |> entibble()
#' dplyr::bind_cols(letters, LETTERS) |> copy_for_xl()
#' letters |> rlang::set_names(LETTERS) |> copy_for_xl()
#'
#' entibble(1:10,11:20)
#' entibble(c(1:10,11:20))
#'
#' entibble(list(1:10,11:20))
#' tibble::tibble(list(1:10,11:20))
#'
#' entibble(enlist(1:10,11:20))
#' entibble(enlist(1:10,11:20), .rowname = 'nums')
#' entibble(list(1:10),list(11:20))
#' tibble::tibble(enlist(1:10),enlist(11:20))
#' entibble(enlist(1:10),enlist(11:20))
#' entibble(enlist(1:10),enlist(11:20), .rowname = 'blue')
#' entibble(enlist(1:10),enlist(11:20), .rowname = 'a column name', .name_repair = 'unique')
#' entibble(enlist(1:10),enlist(11:20), .rowname = 'a column name', .name_repair = 'universal')
#' entibble(enlist(1:10),enlist(11:20), .rowname = 'a column name', .name_repair = 'universal_quiet')
#' entibble(enlist(list(1:10),list(11:20)))
#' entibble(enlist((1:10),(11:20)))
#'
#' # test
#' as.matrix(warpbreaks[1:10, ]) |> asplit(2) |> entibble()
#' lattice::Rows()
#' entibble |> entibble()
#'
entibble <- function(
    ...,
    .rowname = 'rowname',
    .name_repair = c("minimal", "check_unique", "unique", "universal", "unique_quiet", "universal_quiet")
){

  in_exprs <- enlist(...)


  if ( length(in_exprs) == 0 ){

    return(tibble::tibble())

  } else {

    checkmate::assert_string(.rowname, null.ok = FALSE, na.ok = FALSE)
    out <- enflatten(in_exprs, .f = identity, name_spec = "{inner}") |>
      purrr::imap(.f = ~.to_tibble(.x, .rowname = .rowname, .vec_name = .y))
    row_counts <- out |> purrr::map_int(nrow) |> unique()
    is_ragged <- length(row_counts)!=1

    if(is_ragged){

      out <- .vector_2_tibble(.in_vec = out, .rowname = .rowname, .vec_name = 'list(irregular_elements)')

    } else {

      out <- out |> purrr::reduce(.f = tibble::tibble, .name_repair = 'minimal')

    }


  }


  names(out) <- vctrs::vec_as_names(names(out), repair = .name_repair)
  return(out)

}



#' Internal function used in entibble.
#' Process dataframes, matrices, vectors, factors to named tibble.
#'
#' @param .in_data user data to tibble
#' @param .rowname user specified rowname; ignored if no rownames
#' @param .vec_name The name of the expression; passed to [.vector_2_tibble()]
#'
#' @return a tibble
#' @export
#'
#' @examples .to_tibble(head(mtcars), 'vehicle')
.to_tibble <- function(.in_data = NULL,
                       .rowname = 'rowname',
                       .vec_name = ''
){


  if( base::is.data.frame(.in_data) || is.matrix(.in_data) ){

    if( tibble::is_tibble( .in_data ) ){
      out <- .in_data
    } else {
      out <- base::asplit(x = .in_data, MARGIN = 2) |> purrr::map(as.vector) |> dplyr::bind_cols()
      if( tibble::has_rownames( .in_data ) ){
        out <-  tibble::add_column(.data = out, !!.rowname := rownames(.in_data), .before = 1, .name_repair = "minimal")
      }
    }


  } else if (base::is.vector( .in_data ) || base::is.factor(.in_data) ){

    out <- .vector_2_tibble(.in_vec = .in_data, .rowname = .rowname, .vec_name = .vec_name )

  } else {

    # tibble anything else
    out <- tibble::tibble(!!.in_data, .name_repair = 'minimal')

  }

  return(out)

}




#' Internal function used in [.to_tibble()].
#' Transform a named vector, factor, or list to a one or 2 column tibble
#'
#' @param .in_vec input data
#' @param .rowname user specified name for rows
#' @param .vec_name name for vector, typically an expression
#'
#' @return tibble
#' @export
#'
#' @examples letters |> rlang::set_names() |> .vector_2_tibble()
#'
.vector_2_tibble <- function(.in_vec, .rowname = 'rowname', .vec_name = ''){

  if (!base::is.null(base::names( .in_vec ))) {
    out <- .in_vec |> tibble::enframe( name = .rowname, value = .vec_name )
  } else {
    out <- .in_vec |> tibble::tibble( .name_repair = ~.vec_name )
  }

  return(out)

}

