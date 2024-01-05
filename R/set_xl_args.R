#' Set persistent default arguments for function `xlr::xl()`
#'
#' Reconciles the parameters with the arguments from `xl()` and
#' stores the new defaults session in [options()]
#'
#' @param ... parameters and arguments from `xl()`
#' @param reset remove the global `xl()` option arguments currently set
#'
#' @seealso [xlr::xl()]
#'
#' @return current default args (invisibly)
#' @export
#'
#' @examples
#' (store_options <- getOption('xlr.xl'))
#'
#' set_xl_args(reset = TRUE)
#' set_xl_args(.quiet = TRUE, .tabname_spec = list(zoom = 65))
#' getOption('xlr.xl')
#' set_xl_args(.workbook_spec = list(asTable = FALSE, zoom = 85, withFilter = TRUE))
#' getOption('xlr.xl')
#' set_xl_args(.quiet = FALSE, reset = TRUE)
#' set_xl_args(.sheet_titles = toupper)
#'
#' # restore original options
#' set_xl_args(store_options, reset = TRUE)
#'
set_xl_args <- function(..., reset = FALSE){

  args_to_set <- list(...)
  no_args <- length(args_to_set)==0

  if(reset){
    cli::cli_alert_info("Removing existing `xl()` default arguments:")
    cli::cat_line()
    print(getOption('xlr.xl'))
    cli::cat_line()
    options(xlr.xl = NULL)
    if( no_args ){return(invisible(NULL))}
  }

  if( no_args ){
    cli::cli_abort('No arguments to set in options(xlr.xl).')
  }

  function_args <- formals(xlr::xl)[-1]
  bad_args <- setdiff(names(args_to_set), names(function_args))
  if( length(bad_args) > 0 ){
    cli::cli_abort(c(
      'Incorrect or missing xl() parameter names: {.var {bad_args}}.',
      ' No options set.'))
  }

  option_args <- enlist(getOption('xlr.xl')) |>
    utils::modifyList(args_to_set, keep.null = TRUE) |>
    purrr::discard_at('getOption("xlr.xl")')

  if(length(option_args)==0){option_args <- NULL}

  options( xlr.xl = option_args )
  cli::cli_alert_info("Options set:")
  cli::cat_line()
  print(option_args)
  cli::cat_line()

  return(invisible(option_args))

}

