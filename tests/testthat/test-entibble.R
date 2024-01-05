test_that("check test examples", {
  expect_equal(
    matrix(c(1, 2, 3, 11, 12, 13), nrow = 2, ncol = 3, byrow = TRUE, dimnames = list(c('row1', 'row2'), c('C.1', 'C.2', 'C.3'))) |> entibble(),
    structure(list(
      rowname = c("row1", "row2"),
      C.1 = c(1, 11), C.2 = c(2, 12), C.3 = c(3,13)),
      row.names = c(NA, -2L), class = c("tbl_df", "tbl", "data.frame"))
  )
  expect_equal(
    entibble(tail(iris, 2)),
    structure(
      list(rowname = c("149", "150"),
           Sepal.Length = c(6.2, 5.9), Sepal.Width = c(3.4, 3),
           Petal.Length = c(5.4, 5.1), Petal.Width = c(2.3, 1.8),
           Species = c("virginica", "virginica")),
      row.names = c(NA, -2L), class = c("tbl_df", "tbl", "data.frame"))
  )
  expect_equal(
    mtcars |> head(2) |> entibble(.rowname = 'vehicle'),
    structure(
      list(vehicle = c("Mazda RX4", "Mazda RX4 Wag"), mpg = c(21, 21),
           cyl = c(6,6), disp = c(160, 160), hp = c(110, 110), drat = c(3.9, 3.9),
           wt = c(2.62, 2.875), qsec = c(16.46, 17.02), vs = c(0, 0), am = c(1, 1),
           gear = c(4, 4), carb = c(4, 4)), row.names = c(NA, -2L),
      class = c("tbl_df", "tbl", "data.frame"))
  )
})



test_that("NULL and empty values create an empty tibble or empty element within tibble", {

  expect_identical(entibble(NULL), entibble())
  expect_identical(entibble(a = NULL), entibble())
  expect_identical(dim(entibble(NULL, a = 1)), c(2L,2L))
  expect_identical(entibble(b = 1, NULL, c = 3)$rowname, c("b", "NULL", "c"))
  expect_identical(entibble(a = c()), entibble())
  expect_equal(entibble(''), structure(list(""), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -1L), names = ""))
  expect_equal(
    enlist(head(iris), head(mtcars), NULL, NULL) |> entibble() |> purrr::pluck(2,3),
    entibble()
  )
  expect_equal(entibble(NA),structure(list(`NA` = NA), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -1L)))
})

test_that('handles matrices and multi-dim tables', {
  expect_equal(
    as.matrix(warpbreaks[1:10, ]) |> entibble() |> head(2),
    structure(list(rowname = c("1", "2"), breaks = c("26", "30"),
                   wool = c("A", "A"), tension = c("L", "L")),
              row.names = c(NA,-2L), class = c("tbl_df", "tbl", "data.frame"))
  )
  expect_equal(
    entibble(Titanic) |> head(2),
    structure(list(Class = c("1st", "2nd"), Sex = c("Male", "Male"),
                   Age = c("Child", "Child"), Survived = c("No", "No"),
                   Freq = c(0, 0)), row.names = c(NA, -2L),
              class = c("tbl_df", "tbl", "data.frame"))
  )
})

test_that("matrices are tibbled with rownames", {
  expect_equal(
    as.matrix(warpbreaks[1:2, ]) |> entibble(),
    structure(list(rowname = c("1", "2"), breaks = c("26", "30"),
                   wool = c("A", "A"), tension = c("L", "L")),
              row.names = c(NA, -2L), class = c("tbl_df", "tbl", "data.frame"))
  )
})

test_that("factors become chars if not in a tibble, but other atomic types are preserved", {
  expect_equal(
    warpbreaks |> entibble() |> purrr::map_chr(class),
    c(breaks = "numeric", wool = "character", tension = "character")
  )
  expect_equal(
    iris |> entibble() |> purrr::map_chr(class),
    c(Sepal.Length = "numeric", Sepal.Width = "numeric", Petal.Length = "numeric",
      Petal.Width = "numeric", Species = "character")
  )

})

test_that("test preserved names and user naming for row and col names", {
  expect_equal(entibble(x = list(y = 1:3, z = 4:5))$rowname, c("y", "z"))
  expect_equal(entibble(x = c(y = 1, z = 2)) |> names(), c('rowname', "x"))
  expect_equal(
    entibble(head(mtcars,1), .rowname = 'vehicle') |> names(),
    c("vehicle", "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
  )
  expect_equal(
    entibble(enlist(1:10),enlist(11:20), .rowname = 'blue') |> names(),
    c("blue", "enlist(1:10)", "blue", "enlist(11:20)")
  )
  example_data <- rlang::set_names(LETTERS[1:5])
  expect_equal(
    entibble(example_data, example_data) |> names(),
    c("rowname", "example_data", "rowname", "example_data")
  )
  expect_equal(
    as.matrix(1:2) |> entibble(),
    structure(list(`as.matrix(1:2)` = 1:2), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -2L))
  )
  expect_equal(
    entibble(enlist(1:10),enlist(11:20), .rowname = 'a column name', .name_repair = 'unique') |> names() |> suppressMessages(),
    c("a column name...1", "enlist(1:10)", "a column name...3", "enlist(11:20)")
  )
  expect_equal(
    entibble(enlist(1:10),enlist(11:20), .rowname = 'a column name', .name_repair = 'universal_quiet') |> names(),
    c("a.column.name...1", "enlist.1.10.", "a.column.name...3", "enlist.11.20.")
  )
  # rowname var is ignored if no names
  expect_true(
    is.null(suppressWarnings(entibble(as.matrix(warpbreaks[1:2, ]), .rowname = 'row_nums')$rowname))
  )
})

test_that("entibble returns correct number of rows/cols if handed ragged and non-ragged elements; and length 1 vectors are not recycled", {
  expect_equal(dim(entibble(value = 1:10)), c(10L,1L))
  expect_equal(dim(entibble(value = 1:10, name = "dont_recycle_me")), c(2L,2L))
  expect_equal(dim(entibble(name = "dont_recycle_me", value = 1:10, value2 = 11:20)), c(3L,2L))
  expect_equal(dim(entibble(value = 1:10, name = rep("dont_recycle_me",10), value2 = 11:20)), c(10L, 3L))
  expect_equal(dim(entibble(x = 1:10)), c(10L, 1L))
  expect_equal(dim(entibble(as.matrix(list(1:10,11:20)))), c(10L, 2L))
  expect_equal(enlist(head(iris), tail(mtcars)) |> entibble() |> dim(), c(6L, 17L))
})

test_that("pre-listed elements return as single tibble that is spread, if possible, or embedded list objects if ragged)", {
  expect_identical(dim(entibble(enlist(letters, LETTERS))), c(26L, 2L)) # spread
  expect_identical(dim(entibble(enlist(iris, mtcars, c = 3, NULL))), c(4L, 2L)) # named list
  expect_identical(dim(entibble(x = 1:10, y = entibble(z = rep(1, 10)))), c(10L, 2L)) # spread
  expect_equal(
    colours() |> rlang::set_names() |> entibble() |> entibble(another_col = colours()),
    colours() |> rlang::set_names() |> entibble(another_col = colours()) )
  expect_equal(entibble(enlist((1:2),(11:12))),entibble((1:2),(11:12)))  # spread (w/extra parens)
  expect_equal(
    entibble(enlist(list(1:2),list(11:12))), #spread list of lists
    structure(list(`list(1:2)` = list(1:2), `list(11:12)` = list( 11:12)),
              class = c("tbl_df", "tbl", "data.frame"), row.names= c(NA, -1L)))
})

test_that("functions get deparsed for printing", {
  expect_identical(
    substr(head(entibble(lm),1)[[1]], 1,10),
    "function ("
  )
  expect_identical(
    entibble(a = new.env()),
    structure(list(a = "<environment>"), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -1L))
  )
  expect_identical(
    entibble(a = quote(a)),
    structure(list(a = "a"), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -1L))
  )
  expect_length(
    entibble(lm(Height ~ Girth + Volume, data = datasets::trees))[[1]], 75
  )
  expect_length(
    entibble(sapply(lm(Height ~ Girth + Volume, data = trees), list_iron))[[1]], 12
  )

})

test_that("entibble produces an output in cases where tibble does not", {
  expect_no_error( entibble(a = 1, a = 1) )
  expect_no_error( entibble(a = rlang::new_environment()) )
  expect_no_error( entibble(a = 1, b = 2:3, c = 4:6, d = 7:10) )
})

test_that("NSE works", {
  expect_equal( dim(entibble(!!!head(letters))), c(1L, 6L) )
  expect_equal( names(entibble(!!!head(letters))), c("a", "b", "c", "d", "e", "f") )
  x <- 3
  expect_equal(
    entibble(x = 1, y = !!x),
    structure(list(x = 1, y = 3), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -1L))
  )
  expect_equal(
    entibble(!!!list(x = rlang::quo(1:3), y = quote(x * 2))),
    structure(
      list(rowname = c("x", "y"),
           `list(ragged_elements)` = list(
             structure(
               list(x = 1:3),
               class = c("tbl_df", "tbl", "data.frame"),
               row.names = c(
                 NA, -3L
               )),
             structure(
               list(y = 6),
               class = c("tbl_df", "tbl", "data.frame"),
               row.names = c(NA, -1L)
             ))),
      class = c("tbl_df", "tbl", "data.frame"),
      row.names = c(NA, -2L)
    ))

})


test_that("missing names are inferred", {
  x <- 1:3
  expect_equal(
    entibble(x, y = x),
    structure(list(x = 1:3, y = 1:3), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -3L))
  )
})

test_that("empty input makes 0 x 0 tbl_df", {
  expect_s3_class(entibble(), "tbl_df")
  expect_equal(dim(entibble()), c(0L, 0L))
  expect_identical(attr(entibble(), "names"), character(0L))
})

test_that("can create a tibble with an expression column", {
  foo <- entibble(x = expression(1 + 2))
  expect_equal(as.list(foo$x), as.list(expression(1 + 2)))
})

test_that("types preserved in spread or ragged element list()", {
  expect_equal(
    class(entibble(a = 1:2, b = as.difftime(1, units = "hours"))[[2]][[2]][[1]]), "difftime"
  )
  expect_equal(
    entibble(a = 1:2, b = as.difftime(c(1, 1), units = "hours")),
    structure(list(a = 1:2, b = structure(c(1, 1), class = "difftime", units = "hours")),
              class = c("tbl_df", "tbl","data.frame"), row.names = c(NA, -2L))
  )
})

test_that("subsetting forces rownames", {
  expect_equal(
    entibble(trees[1, ]) |> ncol(), 4
  )
  expect_equal(
    entibble(trees[2:3, ])$rowname,
    c("2", "3")
  )
})

test_that("vector gets tibbled with names", {
  expect_equal(
    letters[1:5] |> entibble(),
    structure(list(`letters[1:5]` = c("a", "b", "c", "d", "e")), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -5L))
  )
  expect_equal(
    1:5 |> entibble(),
    structure(list(`1:5` = 1:5), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -5L))
  )
})

test_that("forced rownames on vector", {
  expect_equal(
    letters[1:3] |> purrr::set_names() |> entibble()
    ,structure(list(rowname = c("a", "b", "c"),
                    `purrr::set_names(letters[1:3])` = c("a", "b", "c")),
               class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -3L))
  )
})

test_that("autonaming", {
  expect_equal(
    5 |> entibble()
    ,structure(list(
      "5" = 5),
      class = c("tbl_df", "tbl", "data.frame"),
      row.names = c(NA, -1L))
  )
})

test_that("named NA", {
  expect_equal(
    NA |> entibble()
    ,structure(list(
      `NA` = NA),
      class = c("tbl_df", "tbl", "data.frame"),
      row.names = c(NA, -1L))
  )
})

test_that("NULL returns empty tibble", {
  expect_equal(
    NULL |> entibble()
    ,
    structure(
      list(
      ),
      class = c(
        "tbl_df", "tbl", "data.frame"
      ),
      row.names = integer(0),
      names = character(0)
    )
  )
})

test_that("unnamed lists do not create names", {
  expect_equal(
    list(tail(iris), head(mtcars)) |> entibble() |> rownames()
    ,1:6 |> paste()
  )
})


test_that("functions are named in list", {
  expect_equal(
    purrr::map |> entibble() |> names(), "purrr::map"
  )
})

test_that('.name_repair works', {
  expect_equal(
    rlang::set_names(letters) |> entibble(.name_repair = ~c('name_A', 'name_B')) |> names(),
    c('name_A', 'name_B')
  )
  expect_equal(
    entibble(x = 1, x = 2, .name_repair = "unique_quiet") |> names(),
    c("x...1", "x...2")
  )

  expect_equal(
    suppressMessages(entibble(`a 1` = 1, `a 2` = 2, .name_repair = "universal")) |> names(),
    c("a.1", "a.2")
  )
  expect_equal(
    entibble(x = 1, x = 2, .name_repair = make.unique) |> names(),
    c("x", "x.1")
  )
  fix_names <- function(x) gsub("\\s+", "_", x)
  expect_equal(
    entibble(`year 1` = 1, `year 2` = 2, .name_repair = fix_names) |> names(),
    c("year_1", "year_2")
  )
})


test_that("NULL and empty values, paired with data, create a ragged list, but NA or '' values do not", {

  expect_equal(
    entibble(list(
      `Arctic/Longyearbyen` = "Arctic/Longyearbyen",
      posixrules = character(0)
    )),
    structure(
      list(rowname = c("Arctic/Longyearbyen", "posixrules"),
           `list(ragged_elements)` = list(
             structure(
               list(`Arctic/Longyearbyen` = "Arctic/Longyearbyen"),
               class = c("tbl_df", "tbl", "data.frame"),
               row.names = c(NA, -1L)
             ),
             structure(
               list(posixrules = character(0)),
               class = c("tbl_df", "tbl", "data.frame"),
               row.names = integer(0)))
      ),
      class = c("tbl_df", "tbl", "data.frame"),
      row.names = c(NA, -2L)
    )
  )

  expect_equal(
    entibble(list(
      `Arctic/Longyearbyen` = "Arctic/Longyearbyen",
      posixrules = NA
    )), structure(
      list(`Arctic/Longyearbyen` = "Arctic/Longyearbyen", posixrules = NA),
      class = c("tbl_df", "tbl","data.frame"), row.names = c(NA, -1L)
    )
  )

  expect_equal(
    entibble(list(
      `Arctic/Longyearbyen` = "Arctic/Longyearbyen",
      posixrules = ""
    )), structure(
      list(`Arctic/Longyearbyen` = "Arctic/Longyearbyen", posixrules = ""),
      class = c("tbl_df", "tbl","data.frame"), row.names = c(NA, -1L)
    )
  )

})


# does not handle .data, .env, and internal mutate() semantics; should it?
# # Not sure it's necessary for xl presentation...
#
# test_that(".data pronoun; Use .data, .env to refer explicitly to columns", {
#   expect_identical(entibble(a = 1, b = .data$a), entibble(a = 1, b = 1))
# a <- 1
# expect_equal( entibble(a = 2, b = .data$a), entibble(a = 2, b = .data$a) )
# expect_equal( entibble(a = 2, b = .env$a), entibble(a = 2, b = .env$a) )
# })
#
# # does not handle evaluation inputs within a single environment; should it?
# # Not sure it's necessary for xl presentation...
# test_that("mutate() semantics for entibble()", {
#   expect_equal(
#     entibble(a = 1:2, b = 1, c = b / sum(b)),
#     entibble(a = 1:2, b = c(1, 1), c = c(0.5, 0.5))
#   )
#
#   expect_equal(
#     entibble(b = 1, c = b / sum(b), a = 1:2),
#     entibble(b = c(1, 1), c = c(1, 1), a = 1:2)
#   )
# })
#
# test_that("auto-splicing anonymous tibbles (#581)", {
#   df <- entibble(a = 1, b = 2)
#   expect_identical(
#     entibble(df, c = b),
#     tibble::add_column(df, c = 2)
#   )
# })
