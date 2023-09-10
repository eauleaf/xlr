orig_str <- list(
  "list(tail(iris, 2),tail(mtcars, 2))|1" = structure(
    list(
      rowname = c("149", "150"),
      Sepal.Length = c(6.2, 5.9),
      Sepal.Width = c(3.4, 3),
      Petal.Length = c(5.4, 5.1),
      Petal.Width = c(2.3, 1.8),
      Species = structure(
        c(3L, 3L),
        levels = c("setosa", "versicolor", "virginica"),
        class = "factor")),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -2L)),
  "list(tail(iris, 2),tail(mtcars, 2))|2" = structure(
    list(
      rowname = c("Maserati Bora", "Volvo 142E"),
      mpg = c(15, 21.4 ),
      cyl = c(8, 4),
      disp = c(301, 121),
      hp = c(335, 109),
      drat = c(3.54, 4.11),
      wt = c(3.57, 2.78),
      qsec = c(14.6, 18.6),
      vs = c(0, 1),
      am = c(1, 1),
      gear = c(5, 4),
      carb = c(8, 2)),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -2L))
  ) |> setNames(1:2)


test_that("list_iron() tibbles multiple dataframes", {
  expect_equal(orig_str, list_iron(tail(iris,2), tail(mtcars,2)) |> setNames(1:2))
})

test_that("list_iron() dis-embeds multi-level lists", {
  expect_equal(orig_str , list_iron(list(list(tail(iris,2), list(tail(mtcars,2))))) |> setNames(1:2))
})

cat('much to do in list_iron tests')

#' list(c(list(1:5), a = list(5:1, 'green', list('blue'))), purrr::set_names(letters, LETTERS)) |> str()
#' list(c(list(1:5), a = list(5:1, 'green', list('blue'))), purrr::set_names(letters, LETTERS)) |>
#'   list_iron() |> str()
#' (embedded_list <- list(list(list(1:5), a = list(5:1, 'green', list('blue')), purrr::set_names(letters, LETTERS)) |> list(head(iris), tail(mtcars))))
#' embedded_list |> str()
#' embedded_list |> names()
#' embedded_list |> list_iron() |> str()
#' embedded_list |> list_iron() |> names()
#' embedded_list |> list_iron(name_repair = 'unique') |> names()
#' embedded_list |> list_iron(name_spec = '{outer}_outer_|{inner}_inner_') |> names()
#' embedded_list |> list_iron(name_spec = '', name_repair = 'unique') |> names()
#' list(list(list(1:5), list(5:1, 'green', list('blue')), purrr::set_names(letters, LETTERS)) |>
#' list(head(iris), tail(mtcars))) |> list_iron(name_spec = 'tbl_{inner}') |> names()
#' list(list(list(1:5), list(5:1, 'green', list('blue')), purrr::set_names(letters, LETTERS))) |>
#' list(head(iris), tail(mtcars)) |> list_iron(name_spec = 'lst_grp1.{inner}') |> names()
#' some_list <- list(list(grp_a = 2:1, grp_b = list(b1 = 2, b2 = list(list(f = iris))), grp_c = list(c2 = 3, c1 = 4)))
#' list_iron(some_list)



#' tests
#' as.list(letters) |> list_iron()
#' list(c(list(1:5), list(5:1)), letters) |> list_iron()
#' list(a = 1,b=5, 1:10, letters,  ) |> xl()
#' c(1,1:5, list(1:10)) |> xl()
#' a <- tibble::tibble(iris); xl(a)
#' xl(NA)
#' xl(NULL)
#' xl(1)
#' 1:26 |> purrr::set_names(letters) |> xl()
#' as.matrix(warpbreaks[1:10,])
#' a <- list()
#' a$tori = list(
#'   c = list(b = iris),
#'   d = mtcars
#' )
#' a$b$f$e <- tibble::tibble(a=c(1,2,3), b=c(1,2,45))
