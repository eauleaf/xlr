#' Open a file or folder with the OS default app
#'
#' Open files or folders through the operating system default launch application
#'
#' @details If you aren't able to open a file with `sys_open()`, right-click on
#'   the file to make sure your operating system has a default application
#'   assigned to that file type, and that double-clicking the file opens it in that
#'   default application.
#'
#' @param ... path strings to the files or folder you want to open
#' -- inputs can be any combination of lists, vectors, or comma-separated inputs
#' -- no input opens the current working folder location
#' -- paths are automatically expanded using [normalizePath()]
#'
#' @return character vector of named paths
#'
#' @export
#'
#' @examples \dontrun{
#' # open current working directory
#' sys_open('.')
#' sys_open(here::here())
#'
#' # open parent of current working directory
#' sys_open('..')
#'
#' # open user home dir
#' sys_open('~')
#'
#' # open system dir
#' sys_open('/')
#'
#' # open the first 3 files or folders in the current directory
#' list.files(here::here())[1:3] |> sys_open()
#' }
#'
sys_open <- function( ... ){

  if( ...length()==0 ){
    paths <- here::here()
  } else {
    paths <- list( ... ) |> unlist(use.names = FALSE) |> unique()
    checkmate::assert_character(paths, any.missing = FALSE, null.ok = FALSE)
    paths <- paths |> rlang::set_names() |> normalizePath(mustWork = TRUE)
  }


  if ((.Platform$OS.type == "windows")) {
    results <- paths |> purrr::map(shell.exec)
    # return(result)
  } else if(.Platform$OS.type == "unix") {
    results <- paths |> purrr::map(\(path) system2('open', glue::glue("'{path}'"), stdout = NULL, stderr = NULL, timeout = 10))
    # return(result)
  } else if( .Platform$OS.type == "FreeBSD" ){
    # freebsd not tested...
    results <- paths |> purrr::map(\(path) system2('handlr', glue::glue("open {path}"), stdout = NULL, stderr = NULL, timeout = 10))
    # return(result)
  } else {
    cli::cli_abort('Unable to recognize your OS so cannot open paths.')
  }

    return(paths)

}


