
test_that("splits by column names", {
  expect_equal(
    mtcars |> head() |> splitter(gear, cyl),
    list(
      `3|6` = structure(list(
          mpg = c(21.4, 18.1),
          cyl = c(6, 6),
          disp = c(258, 225),
          hp = c(110, 105),
          drat = c(3.08, 2.76),
          wt = c(3.215, 3.46),
          qsec = c(19.44, 20.22),
          vs = c(1, 1),
          am = c(0, 0),
          gear = c(3, 3),
          carb = c(1, 1)),
        row.names = c("Hornet 4 Drive", "Valiant"),
        class = "data.frame"),
      `3|8` = structure(list(
          mpg = 18.7, cyl = 8, disp = 360, hp = 175, drat = 3.15, wt = 3.44,
          qsec = 17.02, vs = 0, am = 0, gear = 3, carb = 2),
        row.names = "Hornet Sportabout", class = "data.frame"),
      `4|4` = structure(list(
          mpg = 22.8, cyl = 4, disp = 108, hp = 93, drat = 3.85, wt = 2.32,
          qsec = 18.61, vs = 1, am = 1, gear = 4, carb = 1
        ),
        row.names = "Datsun 710", class = "data.frame"),
      `4|6` = structure(list(
          mpg = c(21, 21),
          cyl = c(6, 6),
          disp = c(160, 160),
          hp = c(110, 110),
          drat = c(3.9, 3.9),
          wt = c(2.62, 2.875),
          qsec = c(16.46, 17.02),
          vs = c(0, 0),
          am = c(1, 1),
          gear = c(4, 4),
          carb = c(4, 4)),
        row.names = c("Mazda RX4", "Mazda RX4 Wag"),
        class = "data.frame"
      )
    )
  )
})

test_that("splits by column numbers", {
  expect_equal(
    mtcars |> splitter(gear, cyl),
    mtcars |> splitter(10,2)
    )
})

test_that("splits by text columns", {
  expect_equal(
    mtcars |> splitter('gear', 'cyl'),
    mtcars |> splitter(gear, cyl)
    )
})

test_that("NSE works", {
  expect_equal(
    mtcars |> splitter(!!!list('gear', 'cyl')),
    mtcars |> splitter(gear, cyl)
    )
})

test_that("splits by `dplyr` grouped variables if no split info is passed as an argument", {
  expect_equal(
    mtcars |> dplyr::group_by(cyl) |> splitter() |> length(),
    3
    )
})

test_that("ignores split-by-groupings if passed split args directly", {
  expect_equal(
    mtcars |> dplyr::group_by(cyl) |> splitter(carb) |> names(),
    c("1", "2", "3", "4", "6", "8")
  )
})

test_that("preserves `dplyr` groups in split list output, and
          ignores groups as splitting variables if user also passed an argument", {
  expect_equal(
    mtcars |> dplyr::group_by(cyl) |> splitter(gear) |> purrr::map(dplyr::group_size),
    list(`3` = c(1L, 2L, 12L), `4` = c(8L, 4L), `5` = c(2L, 1L, 2L))
  )
})

test_that("separator works when 2 or more vars, but not if only 1", {
  expect_equal(
    mtcars |> splitter(cyl, gear, .sep = ' <> ') |> names(),
    c("4 <> 3", "4 <> 4", "4 <> 5", "6 <> 3", "6 <> 4", "6 <> 5", "8 <> 3", "8 <> 5")
  )
  expect_equal(
    mtcars |> splitter(cyl, gear, carb, .sep = ' <> ') |> names(),
    c("4 <> 3 <> 1", "4 <> 4 <> 1", "4 <> 4 <> 2", "4 <> 5 <> 2", "6 <> 3 <> 1",
      "6 <> 4 <> 4", "6 <> 5 <> 6", "8 <> 3 <> 2", "8 <> 3 <> 3", "8 <> 3 <> 4",
      "8 <> 5 <> 4", "8 <> 5 <> 8")
  )
  expect_equal(
    mtcars |> dplyr::group_by(cyl) |> splitter(.sep = ' <> ') |> names(),
    c("4", "6", "8")
  )
  expect_equal(
    mtcars |> dplyr::group_by(cyl, gear) |> splitter(.sep = ' <> ') |> names(),
    c("4 <> 3", "4 <> 4", "4 <> 5", "6 <> 3", "6 <> 4", "6 <> 5", "8 <> 3", "8 <> 5")
  )
})

test_that("throws error if no vars to split on", {
  expect_error( splitter(mtcars, .sep = ' <> ')  )
})

test_that("throws error if nothing passed in", {
  expect_error( splitter(NULL) )
  expect_error( splitter() )
  expect_error( mtcars |> splitter(NA) )
})

test_that("splits by `dplyr` grouping expression", {
  expect_equal(
    mtcars |> head(3) |> dplyr::group_by(vs==1) |> splitter(),
    list(
      `FALSE` = structure(
        list(
          mpg = c(
            21, 21
          ),
          cyl = c(
            6, 6
          ),
          disp = c(
            160, 160
          ),
          hp = c(
            110, 110
          ),
          drat = c(
            3.9, 3.9
          ),
          wt = c(
            2.62, 2.875
          ),
          qsec = c(
            16.46, 17.02
          ),
          vs = c(
            0, 0
          ),
          am = c(
            1, 1
          ),
          gear = c(
            4, 4
          ),
          carb = c(
            4, 4
          ),
          `vs == 1` = c(
            FALSE, FALSE
          )
        ),
        class = c(
          "grouped_df", "tbl_df", "tbl", "data.frame"
        ),
        row.names = c(
          NA, -2L
        ),
        groups = structure(
          list(
            `vs == 1` = FALSE, .rows = structure(
              list(
                1:2
              ),
              ptype = integer(0), class = c(
                "vctrs_list_of", "vctrs_vctr", "list"
              )
            )
          ),
          class = c(
            "tbl_df", "tbl", "data.frame"
          ),
          row.names = c(
            NA, -1L
          ),
          .drop = TRUE
        )
      ),
      `TRUE` = structure(
        list(
          mpg = 22.8, cyl = 4, disp = 108, hp = 93, drat = 3.85, wt = 2.32,
          qsec = 18.61, vs = 1, am = 1, gear = 4, carb = 1,
          `vs == 1` = TRUE
        ),
        class = c(
          "grouped_df", "tbl_df", "tbl", "data.frame"
        ),
        row.names = c(
          NA, -1L
        ),
        groups = structure(
          list(
            `vs == 1` = TRUE, .rows = structure(
              list(
                1L
              ),
              ptype = integer(0), class = c(
                "vctrs_list_of", "vctrs_vctr", "list"
              )
            )
          ),
          class = c(
            "tbl_df", "tbl", "data.frame"
          ),
          row.names = c(
            NA, -1L
          ),
          .drop = TRUE
        )
      )
    )

    )
})


