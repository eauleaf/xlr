# Tests:
# https://testthat.r-lib.org/articles/third-edition.html
# withr::deferred_run()

# test 'asTable' and duplicate colnames
# xl(entibble(iris, iris))
#

### quick manual tests
# dplyr::starwars |> splitter(eye_color) |> xl(.sheet_titles = ~stringr::str_to_title(paste(., 'eye color')))
# dplyr::starwars |> splitter(eye_color) |> xl(.dataframe_spec = ~janitor::clean_names(., case = 'title'))
# dplyr::starwars |> xl(.open = F)
# dplyr::starwars |> xl(.path = 'boogabooga')
# file.remove(here::here('boogabooga.xlsx'))
# xl(
#   date_formats = entibble(
#     date = lubridate::ymd(20221231),
#     datetime = lubridate::ymd_hms(20221231081001))
#)

#' xl(TRUE)
#' xl(1)
#' xl(c(1:5))
#' # flattens lists and treats lists like comma-separated inputs
#' list(c(list(1:5), list(5:1)), letters) |> xl()
#' xl(list(a = 1,5))
#' c(1,1:5, list(1:10)) |> xl()
#' a <- tibble::tibble(iris); xl(a)
#' rlang::set_names(as.list(letters), LETTERS) |> xl()
#' xl(!!!letters)
#' a_dataframe <- tibble::tibble(iris); xl(a_dataframe)



store_options <- getOption('xlr.xl')

test_that("reset and new args set in the options list", {
  expect_equal(
    set_xl_args(.quiet = TRUE, .tabname_spec = list(zoom = 65), reset = TRUE),
    getOption('xlr.xl')
  )
})

test_that("new args accumulate", {
  expect_equal(
    set_xl_args(.workbook_spec = list(asTable = FALSE, zoom = 85, withFilter = TRUE)),
    list(.quiet = TRUE,.tabname_spec = list(zoom = 65),.workbook_spec = list(
           asTable = FALSE, zoom = 85, withFilter = TRUE))
    )
})

test_that("reset clears on its own", {
  expect_equal(set_xl_args(reset = T), NULL)
})

test_that("empty args returns error", {
  expect_error(set_xl_args())
})

test_that("misnamed args returns an error", {
  expect_error(set_xl_args(green = T, 'blue', 'red'))
})

test_that("original settings are restored", {
  set_xl_args(store_options, reset = T)
  expect_equal(
    getOption('xlr.xl'),
    store_options
  )
})



test_that('xl() correctly writes a dplyr::starwars .xlsx file',{

  xl_test_file <- testthat::test_path('starwars_test.xlsx')
  out <- xl(dplyr::starwars, .open = F, .path = xl_test_file, .return = 'boolean') |> expect_output()
  withr::defer(unlink(xl_test_file))
  starwars_table <- readxl::read_excel(xl_test_file, col_names = FALSE, .name_repair = 'minimal')

  # write success
  expect_true(out)
  expect_true(file.exists(xl_test_file))

  #test that table fieldnames wrote out to line 3
  expect_equal(
    starwars_table[3,1:3] |> unlist() |> as.vector(),
    c("name", "height", "mass")
  )

  # test that title wrote out to line 1
  expect_equal(
    starwars_table[[1]][[1]],
    "Dplyr::Starwars"
  )

})



test_that('xl() writes NA and NULL output xlsx file',{

  xl_test_file <- testthat::test_path('NA_test.xlsx')
  withr::defer(unlink(xl_test_file))
  out <- xl(NA, NULL, a = NA, character(0), integer(),
            .open = F, .path = xl_test_file, .return = 'boolean',
            .workbook_spec = list(asTable = TRUE)) |> expect_output()
  out_table <- readxl::excel_sheets(path = xl_test_file) |>
    rlang::set_names() |>
    purrr::map(\(.) readxl::read_excel(path = xl_test_file, sheet = ., col_names = FALSE, .name_repair = 'minimal'))

  # write success
  expect_true(file.exists(xl_test_file))

  #test that NAs and NULLs write out as expected
  expect_equal(
    out_table,
    list(
      `NA` = structure(
        list(
          c(
            "Na", NA, "x"
          )
        ),
        class = c(
          "tbl_df", "tbl", "data.frame"
        ),
        row.names = c(
          NA, -3L
        ),
        names = ""
      ),
      `NULL` = structure(
        list(
          c(
            "Null", NA, "NULL", "NULL"
          )
        ),
        class = c(
          "tbl_df", "tbl", "data.frame"
        ),
        row.names = c(
          NA, -4L
        ),
        names = ""
      ),
      a = structure(
        list(
          c(
            "A", NA, "x"
          )
        ),
        class = c(
          "tbl_df", "tbl", "data.frame"
        ),
        row.names = c(
          NA, -3L
        ),
        names = ""
      ),
      `character(0)` = structure(
        list(
          c(
            "Character(0)", NA, "x", NA
          )
        ),
        class = c(
          "tbl_df", "tbl", "data.frame"
        ),
        row.names = c(
          NA, -4L
        ),
        names = ""
      ),
      `integer()` = structure(
        list(
          c(
            "Integer()", NA, "x", NA
          )
        ),
        class = c(
          "tbl_df", "tbl", "data.frame"
        ),
        row.names = c(
          NA, -4L
        ),
        names = ""
      )
    )
  )


})


test_that('xl() errors if no data',{
  xl() |> expect_error()
})


test_that('xl .return options work ',{


  # test savepath
  xl_test_file <- testthat::test_path('iris.xlsx')
  withr::defer(unlink(xl_test_file))
  xl(iris, .open = F, .path = xl_test_file, .return = 'savepath') |> expect_output() |>
    expect_equal(here::here(xl_test_file))

  # test tibbles
  xl_test_file2 <- testthat::test_path('iris2.xlsx')
  withr::defer(unlink(xl_test_file))
  xl(iris, .open = F, .path = xl_test_file2, .return = 'tibbles') |> expect_output() |>
    expect_equal(list(iris = entibble(iris)))

  # test 'workbook' and default wb output
  xl_test_file3 <- testthat::test_path('iris3.xlsx')
  withr::defer(unlink(xl_test_file))
  xl(iris, .open = F, .path = xl_test_file3, .return = 'workbook') |> expect_output() |>
    expect_s4_class('Workbook') |> expect_equal(expect_output(xl(iris, .open = F, .path = xl_test_file)))


})


test_that('xl irons and tibbles embedded list objects',{

  # test savepath
  xl_test_file <- testthat::test_path('iris.xlsx')
  withr::defer(unlink(xl_test_file))

  # flattens lists and treats lists like comma-separated inputs
  list(c(list(1:5), list(5:1)), letters[1:5]) |>
    xl(.open = F, .path = xl_test_file, .return = 'tibbles') |> expect_output() |>
    expect_equal(
      list(
        `1` = structure(
          list(
            x = 1:5
          ),
          class = c(
            "tbl_df", "tbl", "data.frame"
          ),
          row.names = c(
            NA, -5L
          )
        ),
        `2` = structure(
          list(
            x = 5:1
          ),
          class = c(
            "tbl_df", "tbl", "data.frame"
          ),
          row.names = c(
            NA, -5L
          )
        ),
        `3` = structure(
          list(
            x = c(
              "a", "b", "c", "d", "e"
            )
          ),
          class = c(
            "tbl_df", "tbl", "data.frame"
          ),
          row.names = c(
            NA, -5L
          )
        )
      )
    )

})


test_that('NSE works',{

  xl_test_file <- testthat::test_path('letters.xlsx')
  withr::defer(unlink(xl_test_file))

  xl(!!!letters, .path = xl_test_file, .open = FALSE, .return = 'tibbles')|> expect_output() |>
    length() |> expect_equal(26)

})





