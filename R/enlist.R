#' Create a list with each list element force-named by the input expression.
#' If passed a single list, does not add an additional layer.
#'
#'
#' @description
#' Use in place of list() to produce a list with auto-assigned names that does not
#' stack lists unless the list is named or the list houses more than one element.
#'
#' @details Embedding:
#' enlist() embedding behavior is different from list(); enlist() only embeds
#' stacked lists if embed is named or if list contains lists, i.e., enlist() doesn't
#' stack lists for structure's sake alone.
#' All superfluous commas are ignored
#' # Compare:
#' `enlist(enlist(enlist(letters))) # one list deep`
#' `list(list(list(letters))) # three lists deep`
#'
#' Unlike list(), enlist() removes its own embedding layer if not assigned
#' a user-specified name, or if embedding implies no change in list structure other
#' than depth, i.e., enlist() doesn't embed its list for embedding's sake alone.
#'
#' # For example, compare:
#' `enlist(enlist(enlist(letters))) # one list deep`
#' `list(list(list(letters))) # three lists deep`
#'
#' But if you name a list element, enlist() retains that information by embedding a named list:
#' enlist(enlist(user_named_list = enlist(letters))) # two lists deep
#'
#' # However, these do embed list structure:
#' enlist(a_name = enlist(letters)) # b/c specified name
#' enlist(enlist(letters), .f = 'a_name')  # b/c specified name
#' enlist(enlist(letters),enlist(letters)) # b/c housing additional list elements (list of lists)
#'
#'
#' This careful embedding behavior is convenient if you want to use enlist() to
#' catch and name dots `...` inside a function where the user wants to handle a
#' list of items within the function, but doesn't want to require the user to have
#' to pass in a named list.
#'
#' For example, compare these functions a() and b() below with listed inputs:
#' a <- function(...){ enlist(...) } # function catching inputs with enlist()
#' b <- function(...){ list(...) } # function catching inputs with list()
#' a(letters, LETTERS) # named flat input list
#' b(letters, LETTERS) # flat unnamed flat input list
#' list(letters, LETTERS) |> a() # still a named flat input list despite user
#' passing in a list() of elements
#' list('green','blue') |> b() # an embedded input list
#'
#'
#' @param ... any objects; if unnamed, enlist() forces names by input expression
#' @param .f a function to rename list where forced names are input to a naming
#'   function, e.g. .f = 'hi', .f = ~gsub('[' ]', '', .) or .f = ~substr(.,1,5)
#'
#'
#' @return an evaluated named list
#' @export
#'
#' @examples
#' enlist(letters)
#' enlist(letters,LETTERS)
#' enlist(head(iris))
#' enlist(tail(mtcars),head(iris))
#' enlist(mtcars,iris) |> purrr::map(head)
#'
#' # auto-naming is overridden if you specify a name
#' enlist(some_named_list_element = tail(iris))
#'
#' enlist(letters, head(iris), enlist('green', blue = 'blue'), .f = 'grp')
#' enlist(letters, head(iris), 'green ish', blue = 'blue', .f = ~paste0(., '-grp_A'))
#'
#' ## you can embed enlist() similar to list()
#' enlist(letters, head(iris), enlist('green', blue = 'blue'))
#' # but embedding behavior is not exactly the same as list())
#' # enlist() does not double embed without a specified name or some change in list structure
#' # compare to depth from embedding list() and enlist()
#' list(list(list(head(iris)))) |> list() |> list()
#' enlist(enlist(enlist(head(iris)))) |> enlist() |> enlist()
#' # enlist() removes its own layer of list() depth if passed a bare list(); compare these
#' list(head(iris)) |> enlist() # list depth 1
#' list(head(iris)) |> list() # list depth 2
#' # but, enlist() only removes it's own layer of list(), so doubling up on list() will embedded list items like you'd expect from list()
#' list(list(head(iris))) |> str()
#' list(list(head(iris))) |> enlist() |> str()
#' # however, if an embedded list element is user-named, then enlist() will embed the list to keep that name info, compare:
#' enlist(embedded = enlist(head(iris))) # list depth 2
#' enlist(enlist(head(iris))) # list depth 1
#' # not adding embedded structure is useful if you want to use enlist() to catch and name dots `...` inside a function where user might or might not pass in a list, e.g.
#' a <- function(...){enlist(...)}
#' a('green','blue') # flat input list
#' list('green','blue') |> a() # still a flat input list
#'
#' ## you can change/override names in a particular enlist() call by passing a lambda naming function with ~, (powered by rlang's rlang::as_function())
#' list(letters, head(iris), colors = enlist('green', blue = 'blue', .f = ~stringr::str_replace_all(., "['aeiou]",'-'))) |> enlist()
#' enlist(mtcars,iris) |> purrr::map(head) |> enlist(.f = ~gsub('.*', 'new_name_here', .))
#' enlist(letters, head(iris), colors = enlist('green', blue = 'blue', .f = ~dplyr::if_else(. == 'blue', 'a_very_deep_blue','a_very_deep_green')))
#' enlist(letters, embed_grp1 = enlist(head(iris)), embed_grp2 = enlist('green', blue = 'blue'), .f = ~paste0('lvl1_',.)) |> enscript()
#' letters |> enlist( .f = ~'')  # removes names
#'
#' ## enlist() behavior with map() (the latter two below are equivalent)
#' candy <- c('lollipops','gum')
#' enlist(letters, candy, cars = tail(mtcars)) |> purrr::map(~enlist(.x))
#' enlist(letters, candy, cars = tail(mtcars)) |> purrr::map(~enlist(!!.x))
#' enlist(letters, candy, tail(mtcars)) |> purrr::map(enlist)
#' # without !!, purrr::map() returns the function's expression text '.x' as a list name since enlist() captures the function's internal expr()
#' # the `<chr>` and `<df[,11]>` names occur because map() performs a layer of evaluation using substitute(arg), and that's what's one eval level below `letters` and `mtcars`
#'
#' # handles non-standard evaluation
#' candy <- list('lollipops','gum')
#' enlist(letters, candy, rlang::list2(!!!candy))
#' enlist(letters)
#' enlist(!!letters)
#' enlist(!!!letters)
#' enlist(!!!candy)
#' enlist(!!!iris)
#' enlist(!!!candy)
#' enlist(candy)
#'
#'
#' # if you don't want to write the names of the columns you're binding
#' dplyr::bind_cols(letters,LETTERS)
#' dplyr::bind_cols(enlist(letters,LETTERS))
#' dplyr::bind_cols(enlist(letters,LETTERS))
#' enlist() # if no args passed, returns an empty named list like list()
#'
# enlist <- function( ... , .f = NULL){ out_list <- rlang::quos(...) |> eval_enlist() if (

enlist <- function(..., .f = NULL) {
    .quos <- rlang::quos(...) |>
        rlang::exprs_auto_name()

    .quos <- .quos[names(.quos) != "<empty>"]


    # if quos list has only 1 quo, and that quo is a list, pull quo and rewrite its expr
    # w/auto-names then place rewritten expr in orig quo (to preserve orig expr names) and eval,
    # otherwise, just map eval every item in the orig quos list
    if (length(.quos) == 1 && is.null(.f) && stringr::str_detect(base::names(.quos), "^(?:xlr::|base::)?(?:en)?list\\(") ){
        .quo <- purrr::pluck(.quos, 1)
        rewritten_expr <- .quo |>
            rlang::quo_get_expr() |>
            rlang::exprs_auto_name()
        out_list <- .quo |>
            rlang::quo_set_expr(rewritten_expr) |>
            rlang::eval_tidy()
    } else {
        out_list <- .quos |>  purrr::map(rlang::eval_tidy)
        # out_list <- rlang::dots_list(..., .ignore_empty = 'all', .named = TRUE)
    }


    # remove external-quotes from names
    double_quoted <- names(out_list) |> stringr::str_detect('^".*"$')
    if( base::any(double_quoted,na.rm = TRUE)){
      base::names(out_list)[double_quoted] <- base::names(out_list)[double_quoted] |>
        stringr::str_extract('(?<=^").*(?="$)')
    }


    # if list elements need to be named by user-function
    if (!is.null(.f)) {
      out_list <- out_list |> rlang::set_names(nm = .f)
    }


    return(out_list)

}



# additional tests ## compare the output of these 2; one's all list(), the other's all enlist()
# candy <- c('lollipops','gum') list(candy, list(nums = 3:1, c = list(b1 = 2, list(new =
# c('red','green'), f = list(head(iris)))), b = list(c2 = 3, c1 = 4:5, letters[1:10])))
# enlist(candy, enlist(nums = 3:1, c = enlist(b1 = 2, enlist(new = c('red','green'), f =
# enlist(head(iris)))), b = enlist(c2 = 3, c1 = 4:5, letters[1:10])))
