#' Convert to tibble with rownames
#'
#' Same as [tibble::tibble()], except it automatically includes a rownames
#' column if passed a named dataframe, matrix, or vector.
#'
#' @param ... object or expression to convert to a tibble
#' @param .rowname String to name the column of rownames if passed a dataframe or matrix with rownames, or a vector with names; default is column name is 'rowname'. If .rownames = NULL, [entibble()] acts identical to [tibble::tibble()].
#' @param .name_repair
#'
#' @seealso [tibble::tibble()]
#'
#' @return a tibble with rownames in a column
#' @export
#'
#' @examples
#' letters |> entibble()
#' letters |>  purrr::set_names() |> entibble()
#' letters |>  purrr::set_names() |> entibble(.rowname = 'something from user')
#' letters |>  purrr::set_names() |> entibble(.rowname = 'something from user', .name_repair = ~c('hello', 'goodbye'))
#' 1:5 |> entibble()
#' tibble::tibble(y = 1:3,x = 11:13)
#' entibble(y = c(1, 2, 3),x = c(11, 12, 13))
#' tibble::tibble(x = 1:3,x = 11:13)
#' entibble(x = 1:3, x = 11:13)
#' tibble::tibble(x = 1:3,x = 11:13, .name_repair = 'minimal')
#' entibble(x = 1:3,x = 11:14, .name_repair = 'minimal')
#' tibble::tibble(x = 1:3,x = 11:14, .name_repair = 'minimal')
#' tibble::tibble(x = 1:3,x = 11:13, .name_repair = 'unique')
#' entibble(x = 1:3,x = 11:13, .name_repair = 'unique')
#' entibble(x = 1:3,x = 11:13, .name_repair = 'unique_quiet')
#' entibble(x = 1:3,x = 11:13, .name_repair = ~scrub_tabnames(., sep = ' <-- ', quiet = T))
#' tibble::tibble(x = 1:3,x = 11:13, .name_repair = ~scrub_tabnames(., sep = ' <-- ', quiet = T))
#'
#' as.matrix(1:10) |> rownames()
#' as.matrix(1:10) |> entibble()
#' as.matrix(1:10) |> tibble::tibble()
#' as.matrix(warpbreaks[1:10, ]) |> rownames()
#' as.matrix(warpbreaks[1:10, ]) |> entibble()
#' as.matrix(warpbreaks[1:10, ]) |> entibble(.rowname = 'row_nums')
#' matrix(c(1, 2, 3, 11, 12, 13), nrow = 2, ncol = 3, byrow = TRUE, dimnames = list(c('row1', 'row2'), c('C.1', 'C.2', 'C.3'))) |> entibble()
#' tail(iris) |> tibble::has_rownames()
#' tail(iris) |> entibble()
#' mtcars |> entibble()
#' mtcars |> entibble(.rowname = 'make')
#' 5 |> entibble()
#' NA |> entibble()
#' NULL |> entibble()
#' enlist(head(iris), head(mtcars)) |> purrr::map(entibble)
#' enlist(head(iris), head(mtcars)) |> entibble()
#' head(iris) |> purrr::map(entibble)
#' entibble(!!!head(iris))
#' enlist(head(iris), head(mtcars)) |> entibble()
#' enlist(head(iris), head(mtcars)) |> tibble::tibble()
#'
#' letters |> purrr::set_names(LETTERS) |> tibble::tibble()
#' letters |> purrr::set_names(LETTERS) |> entibble()
#'
#' letters |> purrr::set_names(LETTERS) |> as.list() |> tibble::tibble()
#' letters |> purrr::set_names(LETTERS) |> as.list() |> entibble()
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
#' entibble(list(1:10,11:20))
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
#'
entibble <- function(
    ...,
    .rowname = 'rowname',
    .name_repair = c("minimal", "check_unique", "unique", "universal", "unique_quiet", "universal_quiet")
    ){

  in_exprs <- rlang::quos( ... ) |> rlang::exprs_auto_name()
  # in_exprs <- enlist(...)
  in_names <- names(in_exprs)


  checkmate::assert_string(.rowname, null.ok = FALSE, na.ok = FALSE)


  if ( length(in_exprs) == 0 ){
    return(tibble::tibble())
  } else {
    out <- purrr::map2(
      .x = in_exprs,
      .y = in_names,
      .f = ~.quo_2_tibble(
        .in_quo = .x,
        .quo_expr = .y,
        .rowname = .rowname,
        .name_repair = "minimal"
      )
    )


    row_counts <- out |> purrr::map_int(nrow) |> unique()
    is_ragged <- length(row_counts)!=1
    if(is_ragged){
      out <- tibble::tibble(enlist(!!!out))
    } else {
      out <- out |>
        purrr::reduce(.f = tibble::tibble, .name_repair = 'minimal')
    }


  }



  return(tibble::tibble(out, .name_repair = .name_repair))
  # return(purrr::pluck(out, 1))
  # tibble::tibble( !!!out, .name_repair = .name_repair )

}



.quo_2_tibble <- function(.in_quo = NULL,
                       .quo_expr = NULL,
                       .rowname = 'rowname',
                       .name_repair = "minimal" ){


  in_data <- rlang::eval_tidy(.in_quo)

  if( tibble::is_tibble( in_data ) ){

    out <- tibble::tibble(in_data, .name_repair = .name_repair)

  } else if( base::is.data.frame( in_data ) ) {
    # dataframe

    if (tibble::has_rownames( in_data )){
    out <- in_data |>
      tibble::rownames_to_column( var = .rowname ) |>
      tibble::tibble(.name_repair = .name_repair)
    } else {
      out <- in_data |>
        tibble::tibble(.name_repair = .name_repair)
    }

  } else if( base::is.matrix( in_data ) ){
    # matrix

    out <- .matrix_2_tibble(
      .in_matrix = in_data, .quo_expr = .quo_expr,
      .rowname = .rowname,.name_repair = .name_repair)

  } else if( base::is.list(in_data) ){
    # list

    out <- .list_2_tibble(in_list = in_data, .quo_expr = .quo_expr,
                         .rowname = .rowname, .name_repair = .name_repair)

  } else if (base::is.vector( in_data ) ){
    # vector

    out <- .vector_2_tibble(in_vec = in_data, .rowname = .rowname, .quo_expr = .quo_expr)

  } else {
    # anything else  I didn't think about

    out <- tibble::tibble(in_data, .name_repair = .name_repair)

  }


  return(out)

}




#' Transforms matrix to named tibble
#'
#' Different defaults from [tibble::tibble()]:
#' - Matrices become list objects with multiple columns rather than 1
#' - Rownames become first column if they exist
#' - If only 1 column, it is named by input expression
#' - Otherwise columns are named like [as.data.frame()], eg. 'V1', 'V2', etc.
#' @inheritParams xlr::entibble
#'
#' @return a tibble with names
#'
#' @examples
#' as.matrix(1:10) |> .matrix_2_tibble()
#' as.matrix(1:10) |> .matrix_2_tibble(.rowname = 'hello') # should ignore rowname
#' as.matrix(warpbreaks[1:10, ]) |> .matrix_2_tibble()
#' as.matrix(warpbreaks[1:10, ]) |> .matrix_2_tibble(.rowname = 'observer num')
#' matrix(c(1, 2, 3, 11, 12, 13), nrow = 2, ncol = 3, byrow = TRUE, dimnames = list(c('row1', 'row2'), c('C.1', 'C.2', 'C.3'))) |> .matrix_2_tibble()
#' matrix(c(1, 2, 3, 11, 12, 13), nrow = 2, ncol = 3, byrow = TRUE, dimnames = list(c('row1', 'row2'), c('C.1', 'C.2', 'C.3'))) |> .matrix_2_tibble(.rowname = 'rownums')
#' matrix(c(1, 2, 3, 11, 12, 13), nrow = 2, ncol = 3, byrow = TRUE) |> .matrix_2_tibble()
#' as.matrix(1:10) |> .matrix_2_tibble(.name_repair = ~paste(., 'hello'))
#' as.matrix(enlist(1:10,11:20)) |> .matrix_2_tibble(.name_repair = "universal_quiet")
#' as.matrix(warpbreaks[1:10, ]) |> .matrix_2_tibble()
#' as.matrix(warpbreaks[1:10, ]) |> .matrix_2_tibble(.rowname = 'observer num')
#' matrix(c(1, 2, 3, 11, 12, 13), nrow = 2, ncol = 3, byrow = TRUE, dimnames = list(c('row1', 'row2'), c('C.1', 'C.2', 'C.3'))) |> .matrix_2_tibble()
#' matrix(c(1, 2, 3, 11, 12, 13), nrow = 2, ncol = 3, byrow = TRUE, dimnames = list(c('row1', 'row2'), c('C.1', 'C.2', 'C.3'))) |> .matrix_2_tibble(.rowname = 'rownums')
#' matrix(c(1, 2, 3, 11, 12, 13), nrow = 2, ncol = 3, byrow = TRUE) |> .matrix_2_tibble()
#'
#' base::asplit(2) might have worked better
#'
.matrix_2_tibble <- function(.in_matrix = NULL, .quo_expr = 'as.matrix(...)', .rowname = 'rowname',.name_repair = 'minimal'){
  matrix_names <- dimnames(.in_matrix)
  has.rownames <- !is.null(matrix_names[[1]])
  has.colnames <- !is.null(matrix_names[[2]])
  out <- .in_matrix |>
    as.data.frame(stringsAsFactors = FALSE, make.names = FALSE) |>
    tibble::tibble()

  # deal with row names
  if( has.rownames ){
    out <- out |> tibble::add_column(!!.rowname := matrix_names[[1]], .before = 1, .name_repair = .name_repair)
  }

  # deal with column names
  if( has.colnames && has.rownames ){
    out <- rlang::set_names(x = out, c(.rowname, matrix_names[[2]])) |>
      tibble::tibble(.name_repair = .name_repair)
  } else if ( has.colnames ){
    out <- out |> rlang::set_names(matrix_names[[2]]) |>
      tibble::tibble(.name_repair = .name_repair)
  } else if ( !has.colnames && ncol(out)==1 ){
    out <- out |> rlang::set_names(.quo_expr) |>
      tibble::tibble(.name_repair = .name_repair)
  } else if ( !has.colnames && ncol(out)>1 ){
    out <- out
  } else {
    out <- out |> tibble::tibble(.name_repair = .name_repair)
  }

  out

}



.vector_2_tibble <- function(in_vec, .rowname = 'rowname', .quo_expr){

  if (!base::is.null(base::names( in_vec ))) {
    out <- in_vec |>
      tibble::enframe( name = .rowname, value = .quo_expr )
  } else {
    out <- in_vec |>
      tibble::tibble() |>
      rlang::set_names(.quo_expr)
  }

  return(out)

}


.list_2_tibble <- function(in_list, .quo_expr, .rowname = 'rowname', .name_repair = 'minimal'){

  list_lengths <- in_list |> base::lengths(use.names = FALSE) |> unique()
  is_ragged <- length(list_lengths)!=1

  if( is_ragged ){

    out <- .vector_2_tibble(in_vec = in_list, .rowname = .rowname, .quo_expr = .quo_expr)

  } else {

    out <- tibble::tibble(!!!enlist(!!!in_list), .name_repair = .name_repair)

  }

  return(out)

}


