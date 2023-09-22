#' Open a file or folder with the OS default app
#'
#' Open files or folders through the operating system default launch application.
#' Also open URLs.
#'
#' @details If you aren't able to open a file with `sys_open()`, right-click on
#'   the file to make sure your operating system has a default application
#'   assigned to that file type, and that double-clicking the file opens it in that
#'   default application.
#'
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
#' sys_open()
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
#'
#' # open the current temp directory
#' sys_open(tempdir())
#'
#' # also opens a browser if sent a path that begins with 'https://'
#' # or 'http://' (or contains '.com/','.org/', etc. or ends in them but without forward slash)
#' sys_open('google.org')
#'
#' # last value can be empty, which will open the working directory
#' sys_open('google.org', )
#'
#' }
#'
sys_open <- function( ... ){

  if( ...length()==0 ){
    paths <- sys_paths <- here::here()
  } else {
    # prep paths for system2()

    paths <- rlang::list2( ... ) |> unlist(use.names = FALSE) |> unique()

    # run input checks
    checkmate::assert_character(paths, any.missing = FALSE, null.ok = FALSE)

    paths <- rlang::set_names(paths) |> stringr::str_trim()

    # collect URLs & correct strings ending in '.org' and '.com' with 'https://'
    url_bools <- stringr::str_detect(paths, '^(?i)(?:https?://)')
    incomplete_url_bools <- stringr::str_detect(paths, '(?i)\\.(?:com/|org/|gov/|net/|edu/|mil/|com$|org$|gov$|net$|edu$|mil$)') & !url_bools
    paths[incomplete_url_bools] <- paste0('https://',paths[incomplete_url_bools])
    url_bools <- stringr::str_detect(paths, '^(?i)(?:https?://)')

    # open any URLs
    if(any(url_bools)){
      url_paths <- paths[url_bools]
      results <- purrr::map(url_paths, .f = utils::browseURL)
      if(all(url_bools)){ return(url_paths) }
    }

    sys_paths <- paths[!url_bools]
    paths[!url_bools] <- sys_paths <- normalizePath(sys_paths, mustWork = TRUE)

  }


  if(( .Platform$OS.type == "windows" )) {
    results <- sys_paths |> purrr::map(shell.exec)
    # return(result)
  } else if(.Platform$OS.type == "unix") {
    results <- sys_paths |> purrr::map(\(path) system2('open', glue::glue("\"{path}\""), stdout = "", stderr = "", timeout = 10))
    # return(result)
  } else if( .Platform$OS.type == "FreeBSD" ){
    # freebsd not tested...
    results <- sys_paths |> purrr::map(\(path) system2('handlr', glue::glue("open {path}"), stdout = NULL, stderr = NULL, timeout = 10))
    # return(result)
  } else {
    cli::cli_alert_danger('Unable to recognize your OS and so cannot open your system paths.
                          Run `.Platform$OS.type` to identify your operating system and report it
                          as an issue at `https://github.com/eauleaf/xlr.`')
  }

    return(paths)

}


