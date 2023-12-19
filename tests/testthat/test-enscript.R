#' # maybe fix formatting for these:
#' # head(dplyr::starwars) |> enlist() |> deparse(backtick = TRUE) |> glue::glue_collapse() |> stringr::str_squish() |> xlr:::.format_script() |> cat()
#' # enlist(head(iris), "`quick_text`='hello'`", tail(dplyr::starwars)) |> deparse(backtick = T) |> glue::glue_collapse() |> stringr::str_squish() |> stringr::str_extract_all('(?<=\\(|, )`.+?` = ')

test_that('.format_script ', {
  expect_equal(
    list(tail(iris,2)) |> deparse1(backtick = TRUE) |> xlr:::.format_script(),
    "list(\nstructure(\nlist(\nSepal.Length = c(\n6.2, 5.9\n), \nSepal.Width = c(\n3.4, 3\n), \nPetal.Length = c(\n5.4, 5.1\n), \nPetal.Width = c(\n2.3, 1.8\n), \nSpecies = structure(\nc(\n3L, 3L\n), \nlevels = c(\n\"setosa\", \"versicolor\", \"virginica\"\n), \nclass = \"factor\"\n)\n), \nrow.names = 149:150, \nclass = \"data.frame\"\n)\n)\n"
  )
})


test_that('simple deparsed outputs', {
  expect_equal(
    1:5 |> enscript(to_clipboard = F),
    "1:5"
  )
  expect_equal(
    enscript(1:5),
    "1:5"
  )
  expect_equal(
    enscript(to_clipboard = F),
    "NULL"
  )
})


test_that('invisible deparsed output', {
  expect_equal(
    iris |> head(2) |> enscript(to_clipboard = F) |> paste(collapse = ''),
    "structure(\nlist(\nSepal.Length = c(\n5.1, 4.9\n), \nSepal.Width = c(\n3.5, 3\n), \nPetal.Length = c(\n1.4, 1.4\n), \nPetal.Width = c(\n0.2, 0.2\n), \nSpecies = structure(\nc(\n1L, 1L\n), \nlevels = c(\n\"setosa\", \"versicolor\", \"virginica\"\n), \nclass = \"factor\"\n)\n), \nrow.names = 1:2, \nclass = \"data.frame\"\n)\n"
  )
})


test_that('vectors enscript', {
  expect_equal(
    rlang::set_names(letters, LETTERS) |> enlist() |> enscript() |> paste(collapse = ''),
    "list(`rlang::set_names(letters, LETTERS)` = c(A = \"a\", B = \"b\", C = \"c\", D = \"d\", E = \"e\", F = \"f\", G = \"g\", H = \"h\", I = \"i\", J = \"j\", K = \"k\", L = \"l\", M = \"m\", N = \"n\", O = \"o\", P = \"p\", Q = \"q\", R = \"r\", S = \"s\", T = \"t\", U = \"u\", V = \"v\", W = \"w\", X = \"x\", Y = \"y\", Z = \"z\"))"
  )
})





