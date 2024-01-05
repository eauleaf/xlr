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
  expect_equal(enlist(list()), enlist())
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
  expect_equal(names(enlist("a")),'a')
  expect_equal(
    names(xlr:::.remove_doublequotes(as.list(tibble::tibble(!!!letters)))),
    letters)
})

test_that("paren-wrapped expressions do not eval to internal list name", {
  expect_equal(enlist((blue = 'green')), list(`(blue = "green")` = "green"))
  expect_equal(enlist(list((blue = 'green'))), list(`list((blue = "green"))` = "green"))
  expect_equal(enlist(enlist((blue = 'green'))), list(`(blue = "green")` = "green"))
  expect_equal(enlist((blue = 'green'), 'red'), list(`(blue = "green")` = "green", red = "red"))
})

test_that("outer list names change and user-defined names take precedence", {
  expect_equal(
    enlist(blue = 'green', list('red', blue = 'red')),
    list(blue = "green", `list("red", blue = "red")` = list("red", blue = "red"))
    )
  expect_equal(
    enlist(blue = 'green', purple = list('red', blue = 'red')),
    list(blue = "green", purple = list("red", blue = "red"))
    )
  expect_equal(
    enlist(blue = 'green', purple = enlist('red', blue = 'red')),
    list(blue = "green", purple = list(red = "red", blue = "red"))
    )
})

test_that("NULLs get named `NULL`", {
  expect_equal(enlist(NULL), list(`NULL` = NULL))
  expect_equal(enlist(NULL = NULL), list(`NULL` = NULL))
  expect_equal(enlist("NULL"), list(`NULL` = "NULL"))
  expect_equal(enlist(NULL, green = enlist('red')), list(`NULL` = NULL, green = list(red = "red")))

})

test_that("enlisted lists disembed when just single list", {
  expect_equal(enlist(green = enlist('red')), list(red = "red"))
  expect_equal(enlist(green = list('red')), list(green = "red"))
  expect_equal(enlist(enlist('red')), list(red = "red"))
  expect_equal(enlist(list('red')), list(`list("red")` = "red"))
  expect_equal(enlist(list(green = 'red')), list(green = "red"))
  expect_equal(enlist(list()), structure(list(), names = character(0)))
})

test_that("naming heirarchy for single passed lists (inner to outer)", {
  expect_equal(enlist(list(green = 'blue')),list(green = "blue"))
  expect_equal(enlist(blue = enlist('green')), list(green = "green"))
  expect_equal(enlist(blue = list('green')), list(blue = "green"))
  expect_equal(enlist(list('green')), list(`list("green")` = "green"))
  expect_equal(enlist(list(blue = 'green')),list(blue = "green"))
  expect_equal(enlist(green = list('red')) , list(green = "red"))
  expect_equal(enlist(green = enlist('red')) , list(red = "red"))
})

test_that("does not autoname list if passed a list with 2 or more list elements", {
  expect_equal(enlist(list(green = 'blue', red = 'red', 'purple')), list(green = "blue", red = "red", "purple"))
  expect_equal(enlist(list(blue = 'green', list('red'))),list(blue = "green", list("red")))
  expect_equal(enlist(list(blue = 'green', 'red')),list(blue = "green", "red"))
  expect_equal(enlist(list(blue = 'green', as.character('red'))),list(blue = "green", "red"))
  expect_true(is.null(names(enlist(hi = list(iris, list(),list())))))
  expect_equal(enlist(list(iris, list(),list())),list(iris, list(),list()))
})


test_that("autonames when embedded or passed comma separated elements", {
  expect_equal(enlist(green = 'blue', red = 'red', hi = enlist('purple')),
               list(green = "blue", red = "red", hi = list(purple = "purple")))
  expect_equal(enlist(green = 'blue', red = 'red', hi = list('purple')),
               list(green = "blue", red = "red", hi = list("purple")))
  expect_equal(enlist(green = 'blue', red = 'red', list('purple')),
               list(green = "blue", red = "red", `list("purple")` = list("purple")))
  expect_equal(enlist(list(letters)), list(`list(letters)` = letters))
  expect_equal(enlist(list(list(letters))), list(`list(list(letters))` = list(letters)))
  expect_equal(enlist(list(list('hi')), NULL),list(`list(list("hi"))` = list(list("hi")), `NULL` = NULL))
  expect_equal(names(enlist(list(green = 'blue'))), 'green')
  expect_equal(names(enlist(red = list(green = 'blue'))), 'green')
  expect_equal(names(enlist(red = list('blue'))),'red')
  expect_equal(names(enlist(list('blue'))) ,"list(\"blue\")")
  expect_equal(names(enlist(list(green = 'blue'), 'red')),c("list(green = \"blue\")", "red"))
  expect_equal(enlist(list(), list()),list(`list()` = list(), `list()` = list()))
})

test_that("drops outer-name when dis-embedding if no place for the name", {
  expect_equal(enlist(pink = list(green = 'blue', red = 'red')),list(green = "blue", red = "red"))
  expect_equal(enlist(pink = enlist('blue', 'red')),list(blue = "blue", red = "red"))
})

test_that("works with rlang list types", {
  expect_equal(enlist(green = rlang::dots_list('blue')),list(green = "blue"))
  expect_equal(enlist(green = rlang::list2('blue')),list(green = "blue"))
  expect_equal(enlist(rlang::dots_list('blue')),list(`rlang::dots_list("blue")` = "blue"))
  expect_equal(enlist(rlang::list2('blue')),list(`rlang::list2("blue")` = "blue"))
})

test_that("test examples", {
  expect_equal(names(enlist(head(iris), tail(mtcars))),c("head(iris)", "tail(mtcars)"))
  expect_equal(names(enlist(some_name = letters)),'some_name')
  expect_equal(names(enlist(letters, .label =  'some_name')),'some_name')
  expect_equal(names(enlist(letters, .label =  ~paste0(.,'_1'))),'letters_1')
  expect_equal(
    enlist('black','white','cyan', .label =  'color_grp_1'),
    list(color_grp_1 = "black", color_grp_1 = "white", color_grp_1 = "cyan"))
  expect_equal(names(enlist(letters, .label =  ~'')),'')
  expect_equal(enlist(enlist(enlist(letters))),list(letters = letters))
  expect_equal(list(letters, b = enlist(a = letters, 'blue')) |> enlist(),
               list(letters, b = list(a = letters, blue = "blue")))
  candy <- list('lollipops','gum')
  expect_equal(
    enlist(candy, !!!candy),
    list(candy = list("lollipops", "gum"), lollipops = "lollipops", gum = "gum")
  )
})


