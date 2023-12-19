test_that("errors out if session not interactive", {
  if(!interactive()) expect_error(paste_from_xl())
})


