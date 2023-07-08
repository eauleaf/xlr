test_that("empty case returns empty named list similar to list()", {
  expect_equal(
    enlist()
    ,structure(list(), names = character(0))
    )
})
