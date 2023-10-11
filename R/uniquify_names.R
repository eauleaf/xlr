#' Glue sequential padded numbers within each group of repeated names
#'
#' Note: tabname cleaning and parameter checks should already have been done in
#'  scrub_tabnames before passing args. This function is recursive and can
#'  call itself forever if width, sep, and pad aren't checked in advance.
#'
#'
#' @param tabnames character vector of tab/sheet names
#' @param truncate_side snip excess tabnames on 'left', 'right', or 'center'
#' @param paste_side 'left', 'right' for side of the tabnames to tack on rownumbers
#' @param width number of chars allowed in the tab/sheet name
#' @param sep tabname text and rownumber separator
#' @param pad padding characters used on left side of group rownumbering
#' @param ellipsis replacement characters during truncation
#' @param quiet quiet warning reports
#'
#'
#' @return vector or unique tabnames
#'
#' @export
#'
#' @examples
#' tabnames = c('', NULL, NULL, NA, NA, 'a', 'a', 'a', 'b', 'b', 'c', rep('d',11))
#' uniquify_names(tabnames)
#' uniquify_names(tabnames = tabnames, paste_side = 'left' , sep = '|')
#'
uniquify_names <- function(tabnames, truncate_side = "right", paste_side = "right",
                           width = 31, sep = "->", pad = ".", ellipsis = "", quiet = FALSE) {

  # NULLs to correct for: "no visible binding for global variable"
  n_obs <- snipped_names <- uniq_numbering <- uniq_tabnames <- NULL


  if( is.null(tabnames) ){ return() }
  checkmate::assert_string(pad, n.chars = 1)

  # initial truncation
  tabnames <- tabnames |>
    stringr::str_trunc(width = width, side = truncate_side, ellipsis = ellipsis) |>
    .fix_forbidden_tabnames(quiet = quiet)

  is_repeated <- repeated(tolower(tabnames)) | tabnames == ""
  if( any(is_repeated) ){

    if( !quiet ){
      cli::cli_alert_warning("Duplicate or empty names exist. Numbering within each group ... ")
    }

    # make tibble to update names by group
    repaired_names_tbl <- enlist(is_repeated, tabnames) |>
      dplyr::bind_cols() |>
      dplyr::group_by(tolower(tabnames)) |>
      dplyr::mutate(n_obs = dplyr::n(),
                    snipped_names = dplyr::if_else(
                      is_repeated,
                      stringr::str_trunc(
                        tabnames,
                        side = truncate_side,
                        ellipsis = ellipsis,
                        width = width - nchar(sep) - nchar(sum(is_repeated))
                      ),
                      tabnames
                    ),
                    uniq_numbering = dplyr::if_else(
                      is_repeated, stringr::str_pad(
                        dplyr::row_number(),
                        width = nchar(n_obs), side = "left", pad = pad), ""
                    )
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        uniq_tabnames = dplyr::if_else(
          is_repeated,
          .paste_names(uniq_numbering, snipped_names,sep, paste_side),
          snipped_names
        )
      )


    tabnames <- dplyr::pull(repaired_names_tbl, uniq_tabnames)

  }


  # return ---------------------------------------------------------------
  # check if tabnames are unique; if not:
  #  recall uniquify_names() with snipped_names instead of uniq_tabnames
  if( any(repeated(tabnames)) ){
    if( !quiet ){
      cli::cli_alert_warning("Duplicates names remain after replacements. Trying again ... ")
    }
    dplyr::pull(repaired_names_tbl, snipped_names) |> uniquify_names(
      truncate_side = truncate_side, paste_side = paste_side, width = width,
      sep = sep, pad = pad, ellipsis = ellipsis, quiet = quiet
    )

  } else {
    tabnames
  }


}
