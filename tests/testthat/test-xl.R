# Tests:
# https://testthat.r-lib.org/articles/third-edition.html
# withr::deferred_run()
#
# test_that('xl() writes a dataframe',{
#   expect_true(
#     xl(dplyr::starwars, .open = F, .path = testthat::test_path('test.xlsx'), .return = 'bool')
#   )
# })

#' xl(NA)
#' xl(a = NA)
#' xl(character(0))
#' xl(';')
#' xl(TRUE)
#' list(c(list(1:5), list(5:1)), letters) |> xl()
#' xl(c(1:5))
#' c(1:5) |> xl()
#' xl(1)
#' xl(list(a = 1,5))
#' list(a = 1,b=5, 1:10, letters, as.matrix(warpbreaks[1:10,]) ) |> xl()
#' c(1,1:5, list(1:10)) |> xl()
#' a <- tibble::tibble(iris); xl(a)
#' rlang::set_names(as.list(letters), LETTERS) |> xl()
#' xl(!!!letters)
#' a_dataframe <- tibble::tibble(iris); xl(a_dataframe)
#' xl()
#' xl('')
#' xl(NULL)
#' xl(a = NULL)
#' xl(iris, dplyr::starwars, mtcars, cars)


### manual tests
# dplyr::starwars |> split(f = dplyr::starwars$eye_color) |> xl(.sheet_titles = ~stringr::str_to_title(paste(., 'eye color')))
# dplyr::starwars |> split(f = dplyr::starwars$eye_color) |> xl(.dataframe_spec = ~janitor::clean_names(., case = 'title'))
# dplyr::starwars |> xl(.open = F)
# dplyr::starwars |> xl(.path = 'boogabooga')
# file.remove(here::here('boogabooga.xlsx'))

