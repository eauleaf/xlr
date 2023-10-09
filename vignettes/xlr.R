## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----setup, include = FALSE---------------------------------------------------
library(xlr)

## ----download-xlr, eval = F---------------------------------------------------
#  # Install
#  remotes::install_github('eauleaf/xlr', build_vignettes = TRUE)
#  library(xlr)

## ----xlr-example1, eval = F---------------------------------------------------
#  xl(mtcars, iris, letters, AirPassengers, Titanic)

## ----enlist-example1, eval = T------------------------------------------------
enlist(enlist(enlist('hello'))) |> str()

## ----enlist-example2, eval = T------------------------------------------------
list(list(list('hello'))) |> str()

## ----enlist-example3, eval = T------------------------------------------------
enlist(list(letters), list(letters)) |> str()

## ----copy_for_xl-example, eval = F--------------------------------------------
#  copy_for_xl(mtcars)

## ----paste_from_xl-example, eval = F------------------------------------------
#  my_data <- paste_from_xl()

## ----paste_from_xl-example2, eval = F-----------------------------------------
#  my_data <- paste_from_xl(has_fieldnames = TRUE)

## ----enscript-example1, eval = F----------------------------------------------
#  enscript(head(mtcars))

## ----enscript-example2, eval = F----------------------------------------------
#  iris |> split(f = iris$Species) |> purrr::map(head, 2) |> enscript()

## ----repeated-example, eval = T-----------------------------------------------
mtcars |> dplyr::filter(repeated(disp))

## ----repeated-example2, eval = F----------------------------------------------
#  # AND statements
#  mtcars |> dplyr::filter(repeated(hp), repeated(disp))
#  # OR statements
#  mtcars |> dplyr::filter(repeated(hp) | repeated(disp))

## ----example-sysopen, eval = F------------------------------------------------
#  sys_open()

## ----list_iron-example, eval = T----------------------------------------------
ridiculous <- list(list(car_data = mtcars, list(list(flower_data = iris))))

# Original list depth
ridiculous |> str()

# Ironed list depth
ridiculous |> list_iron() |> str()

## ----list_iron-example2, eval = T---------------------------------------------
list_iron(ridiculous, .f = ~tail(.,2))

## ----entibble-example, eval = T-----------------------------------------------
alpha <- rlang::set_names(letters, LETTERS)

# rownames are forced in so they show up in spreadsheets
entibble(alpha) |> head(2)
tibble::tibble(alpha) |> head(2)

# lists passed to entibble are dumped and their objects joined like comma-separated 
# inputs if the listed objects have compatible row dimensions
entibble(list(alpha, letters)) |> tail(2)

# compare the above to `tibble()`
tibble::tibble(list(alpha, letters)) |> tail(2)


