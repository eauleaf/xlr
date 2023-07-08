test_that("returns a bool of same length as input", {
  expect_equal(repeated(c(1, 2, 3, 4, 5, 5, 5)), c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE) )
})

test_that("removes NULL and counts everything else", {
  expect_equal(repeated(c(NULL, NULL, NULL, NaN, Inf, NA, NA)), c(FALSE, FALSE, TRUE, TRUE))
})

test_that("NULL is returned as a logical zero to mimic duplicated()", {
  expect_equal(repeated(NULL), logical(0))
})


