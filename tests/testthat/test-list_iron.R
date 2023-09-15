test_list <- list(
  `1:2` = structure(
    list(
      x = 1:2
    ),
    class = c(
      "tbl_df", "tbl", "data.frame"
    ),
    row.names = c(
      NA, -2L
    )
  ),
  `1:2` = structure(
    list(
      x = 1:2
    ),
    class = c(
      "tbl_df", "tbl", "data.frame"
    ),
    row.names = c(
      NA, -2L
    )
  )
)

test_that(".f applies function to each object; elements are named by expression; ", {
  expect_equal(list_iron(1:2, 1:2, .f = entibble), test_list)
})


deep_list <- list() |> list() |> list() |> list() |> list()

test_that("list_iron() dis-embeds multi-nested lists", {
  expect_equal(
    list_iron(deep_list)
    ,structure(list(), names = character(0))
  )
})

test_that("list_iron() always returns a named list; always creates list", {
  expect_equal(
    list_iron()
    ,structure(list(), names = character(0))
  )
})

test_that("nulls return nulls in named list", {
  expect_equal(
    list_iron(NULL, NULL)
    ,list(`NULL` = NULL, `NULL` = NULL)
  )
})

