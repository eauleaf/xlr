test_that("expression-based or user defined savepaths work", {

  expr_savepath(.path = 'hi') |> expect_equal(here::here('hi.xlsx'))
  iris |> expr_savepath() |> stringr::str_detect('/xlr-iris.*') |> expect_true()
  # prefix and suffix work
  expr_savepath(iris, .file_suffix = '.pdf', .tmp_prefix = 'boing-') |>
    stringr::str_detect('/boing-iris.*\\.pdf$') |> expect_true()
  # .path, if specified, overrides prefix (no prefix)
  expr_savepath(.path = 'hi', .file_suffix = '.pdf', .tmp_prefix = 'boing-') |>
      stringr::str_detect('/boing-') |> expect_false()
  # user can add sub-directories
  iris |> expr_savepath(.path = 'somewhere/hi/bye', .file_suffix = '.pdf') |>
    expect_equal(here::here('somewhere/hi/bye.pdf'))

})
