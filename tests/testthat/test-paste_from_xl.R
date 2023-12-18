test_that("throws error if run non-interactively", {
  if(!interactive()) expect_error(paste_from_xl())
})


