test_that("empty case returns empty named list", {
  expect_equal(
    enlist()
    ,structure(list(), names = character(0))
    )
})


test_that("dis-embeds itself when passed a list", {
  expect_equal(
    purrr::pluck_depth(enlist(list('hi')))
    ,purrr::pluck_depth(enlist('hi'))
  )

  expect_equal(
    purrr::pluck_depth(enlist(as.list('hi')))
    ,purrr::pluck_depth(enlist('hi'))
  )
})


test_that("labels via parameter .label", {
  expect_equal(
    enlist('hi', .label = 'green')
    ,list(green = 'hi')
  )
})


test_that("removes all empty args", {
  expect_equal(
    enlist(,,,,'hi',,,, .label = 'green')
    ,list(green = 'hi')
  )
})

test_that("double-quotes removed from names", {
  expect_equal(
    enlist('a') |> names()
    ,'a'
  )

  expect_equal(
    names(xlr:::.remove_doublequotes(as.list(tibble::tibble(!!!letters)))),
    letters
  )

})




