#' Convert data objects to a tibble with defaults optimized for spreadsheet presentation
#'
#' Default behaviors:
#'  * Always includes rownames in tibble, if available
#'  * Does not repeat input elements to obtain common lengths
#'  * Arranges lists with uniform arrays into columns of a single tibble
#'  * A list of ragged elements becomes a two column tibble where the first column is names and
#'  the second column is a nested list
#'  * Is unconcerned with duplicate column names unless specified by user with .name_repair
#'
#'
#'  @details
#'  Entibble is designed to work with the function [xl()] to produce a viewable
#'  output of R data within spreadsheets. To help `xl()` succeed as often as possible, `entibble()` is
#'  liberal about names, such as duplicate column names, and makes an effort to produce a flat table
#'  in place of a nested [tibble::tibble()].
#'
#' @inheritParams tibble::tibble
#' @param ... object or expression to convert to a tibble
#' @param .rowname string; to name the column containing rownames. If rownames are not
#' present in the data, `.rowname` is ignored.
#'
#' @seealso [tibble::tibble()]
#'
#' @return a tibble with rownames in a column
#' @export
#'
#' @examples
#' letters |> entibble()
#'
#' # naming examples
#' (example_data <- letters |>  purrr::set_names(LETTERS))
#' example_data |> entibble()
#' example_data |> entibble(.rowname = 'a user-specified name')
#' example_data |> entibble(.name_repair = ~c('name_A', 'name_B'))
#' entibble(example_data, example_data)
#' entibble(x = 1:3, x = 11:13, .name_repair = 'unique_quiet')
#' mtcars |> entibble(.rowname = 'vehicle')
#'
#' # nests ragged data
#' entibble(example_data, !!!letters)
#'
#' # spreads conforming rows and lists of conforming rows
#' entibble(x = 1:3, y = 11:13)
#' entibble(x = 1:3, y = 10:14)
#'
#' # be somewhat careful about conforming row inputs; it is different from `tibble()`
#' enlist(head(iris), tail(mtcars)) |> entibble()
#' as.matrix(list(1:10,11:20)) |> entibble()
#'
#' # if you want to ensure separate lists, call [purrr::map()] or [list_iron()]
#' enlist(head(iris), tail(mtcars)) |> purrr::map(entibble)
#' enlist(head(iris), tail(mtcars)) |> list_iron(.f = entibble)
#'
entibble <- function(
    ...,
    .rowname = 'rowname',
    .name_repair = c("minimal", "check_unique", "unique", "universal", "unique_quiet", "universal_quiet")
){

  in_exprs <- enlist(...) |> rlang::eval_tidy()


  if ( length(in_exprs) == 0 ){
    return(tibble::tibble())
  } else {

    checkmate::assert_string(.rowname, null.ok = FALSE, na.ok = FALSE)

    out <- in_exprs |> purrr::imap(.f = \(.x,.y).to_tibble(.x, .rowname = .rowname, .vec_name = .y))
    row_counts <- out |> purrr::map_int(nrow) |> unique()
    is_ragged <- length(row_counts)!=1

    if(is_ragged){

      out <- .vector_2_tibble(.in_vec = out, .rowname = .rowname, .vec_name = 'list(ragged_elements)')

    } else {

      out <- out |> purrr::reduce(.f = tibble::tibble, .name_repair = 'minimal')

    }


  }


  names(out) <- vctrs::vec_as_names(names(out), repair = .name_repair)
  return(out)

}




#' Process dataframes, matrices, vectors, factors to named tibble
#'
#' Function called in [entibble()].
#'
#' @param .in_data user data to tibble
#' @param .rowname user specified rowname; ignored if no rownames
#' @param .vec_name The name of the expression; passed to [.vector_2_tibble()]
#'
#' @importFrom rlang :=
#'
#' @return a tibble
#' @export
#'
#' @examples .to_tibble(head(mtcars), 'vehicle')
#' iris |> .to_tibble()
#' letters |> .to_tibble()
#' letters |> rlang::set_names() |> .to_tibble()
#' list(1:10,11:20) |> .to_tibble()
#' as.matrix(list(1:3,11:13,c('a','b','c'))) |> entibble()
#'
.to_tibble <- function(.in_data = NULL,
                       .rowname = 'rowname',
                       .vec_name = ''
){



  # if( is.data.frame(.in_data) || is.matrix(.in_data) || is.table(.in_data)){
  if( !is.null(dim(.in_data)) ){

    if( tibble::is_tibble( .in_data ) ){
      out <- .in_data
    } else {

      stor_names <- NULL
      if( is.table(.in_data) ){ .in_data <- data.frame(.in_data, check.names = FALSE) }
      if( is.data.frame(.in_data) ){ out <- .in_data |> purrr::map(as.vector)
      } else {
        if(is.matrix(.in_data)){stor_names <- rownames(.in_data)}
        out <- asplit(x = .in_data, MARGIN = 2) |> purrr::map(as.vector)
      }

      if(length(out)==1 && is.null(names(out)) ){ names(out) <- .vec_name }
      out <- out |> dplyr::bind_cols()
      if( tibble::has_rownames( .in_data ) || !is.null(stor_names) ){
        out <-  tibble::add_column(.data = out, !!.rowname := rownames(.in_data), .before = 1, .name_repair = "minimal")
      }
      # prior if-not-tibble code:
      # matrix_names <- if(is.matrix(.in_data)){ rownames(.in_data)} else { NULL }
      #
      # if( is.table(.in_data) ){ .in_data <- data.frame(.in_data, check.names = FALSE) }
      #
      # out <- asplit(x = .in_data, MARGIN = 2) |> purrr::map(as.vector)
      # if(length(out)==1 && is.null(names(out)) ){ names(out) <- .vec_name}
      # out <- out |> dplyr::bind_cols()
      # if( tibble::has_rownames( .in_data ) || !is.null(matrix_names) ){
      #   out <-  tibble::add_column(.data = out, !!.rowname := rownames(.in_data), .before = 1, .name_repair = "minimal")
      # }

    }


  } else if (is.vector( .in_data ) || is.factor( .in_data ) ){

    out <- .vector_2_tibble(.in_vec = .in_data, .rowname = .rowname, .vec_name = .vec_name )

  } else if (is.null(.in_data)) {

    out <- tibble::tibble()

  } else {

    # tibble anything else tibble-able
    out <- tibble::tibble({{.in_data}}, .name_repair = 'minimal') |> try(silent = TRUE)
    # tibble formulas
    if( !is.data.frame(out) ){
      # must be two lines for evaluation of formulas
      out <- deparse({{.in_data}}) |> try(silent = TRUE)
      out <- out |> tibble::tibble() |> try(silent = TRUE)
    } else if(!is.data.frame(out)){
      cli::cli_abort('Cannot coerce {(.in_data)} to a tibble.')
    }

    if( identical(1L, ncol(out)) ){
      out <- out |> rlang::set_names(.vec_name)
    }

  }

  return(out)

}



#' Transform a named vector, factor, or list to a one or 2 column tibble
#'
#' Function called in [.to_tibble()].
#'
#' @param .in_vec input data
#' @param .rowname user specified name for rows
#' @param .vec_name name for vector, typically an expression
#'
#' @return tibble
#' @export
#'
#' @examples letters[1:5] |> rlang::set_names() |> .vector_2_tibble()
#' letters[1:5] |> rlang::set_names() |> .vector_2_tibble(.rowname = 'a_name', .vec_name = 'alphas')
.vector_2_tibble <- function(.in_vec, .rowname = 'rowname', .vec_name = ''){

  if (!is.null(names( .in_vec ))) {
    out <- .in_vec |> tibble::enframe( name = .rowname, value = .vec_name )
  } else {
    out <- .in_vec |> tibble::tibble( .name_repair = ~as.character(.vec_name) )
  }

  return(out)

}




