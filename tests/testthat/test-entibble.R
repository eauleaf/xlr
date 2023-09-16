test_that("names in list columns become rownames", {
  foo1 <- tibble::tibble(x = list(y = 1:3, z = 4:5))
  foo2 <- entibble(x = list(y = 1:3, z = 4:5))
  expect_equal(names(foo1$x), foo2$rowname)
})


test_that("names are maintained vectors (#630)", {
  foo1 <- tibble::tibble(x = c(y = 1, z = 2))
  foo2 <- entibble(x = c(y = 1, z = 2))
  expect_equal(names(foo2), c('rowname', "x"))
  expect_equal(names(foo1$x), foo2$rowname)
})


test_that("entibble returns correct number of rows with all combinatinos", {
  expect_equal(nrow(entibble(value = 1:10)), 10)
  expect_equal(nrow(entibble(value = 1:10, name = "dont_recycle_me")), 2)
  expect_equal(nrow(entibble(name = "recycle_me", value = 1:10, value2 = 11:20)), 3)
  expect_equal(nrow(entibble(value = 1:10, name = "recycle_me", value2 = 11:20)), 10L)
})

test_that("NULL is ignored (#580)", {
  expect_identical(entibble(a = NULL), entibble())
  expect_identical(entibble(a = NULL, a = 1), entibble(a = 1))
  expect_identical(entibble(a = NULL, b = 1, c = 2:3), entibble(b = 1, c = 2:3))
  expect_identical(entibble(b = 1, NULL, c = 2:3), entibble(b = 1, c = 2:3))
})

test_that("NULL is ignored when passed by value (#895, #900)", {
  expect_identical(entibble(a = c()), entibble(a = NULL))
  expect_identical(entibble(a = c(), a = 1), entibble(a = 1))
})

test_that("bogus columns raise an error", {
  expect_error( entibble(a = new.env()) )
  expect_error( entibble(a = quote(a)) )
})

test_that("length 1 vectors are recycled", {
  expect_equal(nrow(entibble(x = 1:10)), 10)
  expect_equal(nrow(entibble(x = 1:10, y = 1)), 10)
  expect_error( entibble(x = 1:10, y = 1:2) )
})

test_that("length 1 vectors in hierarchical data frames are recycled (#502)", {
  expect_identical(
    entibble(x = 1:10, y = entibble(z = 1)),
    entibble(x = 1:10, y = entibble(z = rep(1, 10)))
  )
  expect_identical(
    entibble(y = entibble(z = 1), x = 1:10),
    entibble(y = entibble(z = rep(1, 10)), x = 1:10)
  )
  expect_identical(
    entibble(x = 1, y = entibble(z = 1:10)),
    entibble(x = rep(1, 10), y = entibble(z = 1:10))
  )
  expect_identical(
    entibble(y = entibble(z = 1:10), x = 1),
    entibble(y = entibble(z = 1:10), x = rep(1, 10))
  )
})

test_that("missing names are imputed from call", {
  x <- 1:10
  df <- entibble(x, y = x)
  expect_equal(names(df), c("x", "y"))
})

test_that("empty input makes 0 x 0 tbl_df", {
  zero <- entibble()
  expect_s3_class(zero, "tbl_df")
  expect_equal(dim(zero), c(0L, 0L))
  expect_identical(attr(zero, "names"), character(0L))
})

test_that("can create a tibble with an expression column (#657)", {
  foo <- entibble(x = expression(1 + 2))
  expect_equal(as.list(foo$x), as.list(expression(1 + 2)))
})

test_that("attributes are preserved", {
  df <- structure(
    data.frame(x = 1:10, g1 = rep(1:2, each = 5), g2 = rep(1:5, 2)),
    meta = "this is important"
  )
  res <- entibble(df)

  expect_identical(attr(res, "meta"), attr(df, "meta"))
})

test_that(".data pronoun", {
  expect_identical(entibble(a = 1, b = .data$a), entibble(a = 1, b = 1))
})

test_that("mutate() semantics for entibble() (#213)", {
  expect_equal(
    entibble(a = 1:2, b = 1, c = b / sum(b)),
    entibble(a = 1:2, b = c(1, 1), c = c(0.5, 0.5))
  )

  expect_equal(
    entibble(b = 1, a = 1:2, c = b / sum(b)),
    entibble(b = c(1, 1), a = 1:2, c = c(0.5, 0.5))
  )

  expect_equal(
    entibble(b = 1, c = b / sum(b), a = 1:2),
    entibble(b = c(1, 1), c = c(1, 1), a = 1:2)
  )
})

test_that("types preserved when recycling in entibble() (#284)", {
  expect_equal(
    entibble(a = 1:2, b = as.difftime(1, units = "hours")),
    entibble(a = 1:2, b = as.difftime(c(1, 1), units = "hours"))
  )

  expect_equal(
    entibble(b = as.difftime(1, units = "hours"), a = 1:2),
    entibble(b = as.difftime(c(1, 1), units = "hours"), a = 1:2)
  )
})

# Data frame and matrix columns -------------------------------------------

test_that("can make tibble containing data.frame or array (#416)", {
  expect_identical(
    entibble(mtcars = tibble::remove_rownames(mtcars)),
    tibble::new_tibble(list(mtcars = tibble::remove_rownames(mtcars)), nrow = nrow(mtcars))
  )
  expect_identical(
    entibble(diag(5)),
    tibble::new_tibble(list(`diag(5)` = diag(5)), nrow = 5)
  )
})

test_that("auto-splicing anonymous tibbles (#581)", {
  df <- entibble(a = 1, b = 2)
  expect_identical(
    entibble(df),
    df
  )
  expect_identical(
    entibble(df, c = b),
    tibble::add_column(df, c = 2)
  )
})

test_that("can coerce list data.frame or array (#416)", {
  expect_identical(
    entibble(x = trees),
    tibble::new_tibble(list(x = trees), nrow = nrow(trees))
  )
})

test_that("susbsetting returns the correct number of rows", {
  expect_identical(
    entibble(x = mtcars)[1:3, ],
    entibble(x = mtcars[1:3, ])
  )
  expect_identical(
    entibble(y = diag(5))[1:3, ],
    entibble(y = diag(5)[1:3, ])
  )
})

test_that("subsetting one row retains columns", {
  expect_identical(
    entibble(y = diag(5))[1, ],
    entibble(y = diag(5)[1, , drop = FALSE])
  )
})

test_that("package_version is a vector (#690)", {
  ver <- utils::packageVersion("tibble")
  expect_identical(entibble(x = ver)$x, ver)
})


# tibble_row() ------------------------------------------------------------
test_that("subsetting forces rownames", {
  expect_equal(
    entibble(trees[1, ]) |> ncol(), 4
  )
  expect_equal(
    entibble(trees[2:3, ])$rowname,
    c("2", "3")
  )
})


test_that("returns a single row (#416)", {
  model <- lm(Height ~ Girth + Volume, data = trees)
  expect_identical(
    tibble::tibble_row(a = 1, b = vctrs::list_of(2:3), lm = model),
    entibble(a = 1, b = vctrs::list_of(2:3), lm = list(model))
  )
})

# is_tibble ---------------------------------------------------------------

test_that("is_tibble", {
  expect_true(tibble::is_tibble(entibble(trees)))
})


test_that("output test", {
  expect_error( entibble(a = 1, a = 1) )
  expect_error( entibble(a = rlang::new_environment()) )
  expect_error( entibble(a = 1, b = 2:3, c = 4:6, d = 7:10) )
})


test_that("vector gets tibbled", {
  expect_equal(
    letters[1:5] |> entibble(),
    structure(
      list(
        "letters[1:5]" = c(
          "a", "b", "c", "d", "e"
        )
      ),
      class = c(
        "tbl_df", "tbl", "data.frame"
      ),
      row.names = c(
        NA, -5L
      )
    )

  )
  expect_equal(
    1:5 |> entibble()
    ,
    structure(
      list(
        "1:5" = 1:5
      ),
      class = c(
        "tbl_df", "tbl", "data.frame"
      ),
      row.names = c(
        NA, -5L
      )
    )
  )
})

test_that("forced rownames on vector", {
  expect_equal(
    letters[1:5] |> purrr::set_names() |> entibble()
    ,
    structure(
      list(
        rowname = c(
          "a", "b", "c", "d", "e"
        ),
        "purrr::set_names(letters[1:5])" = c(
          "a", "b", "c", "d", "e"
        )
      ),
      class = c(
        "tbl_df", "tbl", "data.frame"
      ),
      row.names = c(
        NA, -5L
      )
    )
  )
})

test_that("matrices are tibbled with rownames", {
  expect_equal(
    as.matrix(warpbreaks[1:4, ]) |> entibble()
    ,
    structure(
      list(
        rowname = c(
          "1", "2", "3", "4"
        ),
        breaks = c(
          "26", "30", "54", "25"
        ),
        wool = c(
          "A", "A", "A", "A"
        ),
        tension = c(
          "L", "L", "L", "L"
        )
      ),
      class = c(
        "tbl_df", "tbl", "data.frame"
      ),
      row.names = c(
        NA, -4L
      )
    )
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
    ,1:2 |> paste()
  )
})

test_that("functions fail as input", {
  expect_error(
    purrr::map |> entibble()
  )
})


test_that("functions are named in list", {
  expect_equal(
    purrr::map |> enlist() |> entibble() |> dplyr::pull(1)
    ,"purrr::map"
    )
})


test_that("Unnamed arguments are named with their expression:", {
  a <- 1:5
  expect_equal(tibble::tibble(a, a * 2, a*3), entibble(a, a * 2, a*3) )
})

test_that("Scalars (vectors of length one) are recycled:", {
  a <- 1:5
  expect_equal(tibble::tibble(a, b = a * 2, c = 1), entibble(a, b = a * 2, c = 1) )
})

test_that("Columns are available in subsequent expressions", {
  x = runif(10)
  expect_equal(
    tibble::tibble(x, y = x * 2), entibble(x , y = x * 2)
    )
})

test_that('entibble and tibble act alike', {
# tibble::tibble() never coerces its inputs,
expect_equal( tibble::tibble(letters), entibble(letters) )
expect_equal( tibble::tibble(x = list(diag(1), diag(2))), entibble(x = list(diag(1), diag(2))) )
expect_equal( tibble::tibble(`a + b` = 1:5), entibble(`a + b` = 1:5) )
# but it forces you to take charge of names, if they need repair:
expect_equal(
  suppressMessages( tibble::tibble(x = 1, x = 2, .name_repair = "unique") ),
  suppressMessages( entibble(x = 1, x = 2, .name_repair = "unique") )
)
expect_equal(
  tibble::tibble(x = 1, x = 2, .name_repair = "minimal"),
  entibble(x = 1, x = 2, .name_repair = "minimal") )
expect_equal(
  ## By default, non-syntactic names are allowed,
  tibble::tibble(`a 1` = 1, `a 2` = 2), entibble(`a 1` = 1, `a 2` = 2)
)
expect_equal(
  ## Syntactic names are easier to work with, though, and you can request them:
  suppressMessages(tibble::tibble(`a 1` = 1, `a 2` = 2, .name_repair = "universal")),
  suppressMessages(entibble(`a 1` = 1, `a 2` = 2, .name_repair = "universal"))
)
expect_equal(
  ## You can specify your own name repair function:
  tibble::tibble(x = 1, x = 2, .name_repair = make.unique), entibble(x = 1, x = 2, .name_repair = make.unique)
)
fix_names <- function(x) gsub("\\s+", "_", x)
expect_equal(
  tibble::tibble(`year 1` = 1, `year 2` = 2, .name_repair = fix_names),
  entibble(`year 1` = 1, `year 2` = 2, .name_repair = fix_names)
)

# You can unquote an expression:
x <- 3
expect_equal( tibble::tibble(x = 1, y = x), entibble(x = 1, y = x) )
expect_equal( tibble::tibble(x = 1, y = !!x), entibble(x = 1, y = !!x) )

# You can splice-unquote a list of quosures and expressions:
expect_equal(
  tibble::tibble(!!!list(x = rlang::quo(1:10), y = quote(x * 2))),
  entibble(!!!list(x = rlang::quo(1:10), y = quote(x * 2)))
)

# Use .data, .env and !! to refer explicitly to columns or outside objects
a <- 1
expect_equal( tibble::tibble(a = 2, b = a), entibble(a = 2, b = a) )
expect_equal( tibble::tibble(a = 2, b = .data$a), entibble(a = 2, b = .data$a) )
expect_equal( tibble::tibble(a = 2, b = .env$a), entibble(a = 2, b = .env$a) )
expect_equal( tibble::tibble(a = 2, b = !!a), entibble(a = 2, b = !!a) )

})


