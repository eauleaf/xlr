% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entibble.R
\name{entibble}
\alias{entibble}
\title{Convert data objects to a tibble with defaults optimized for spreadsheet (2D) presentation}
\usage{
entibble(
  ...,
  .rowname = "rowname",
  .name_repair = c("minimal", "check_unique", "unique", "universal", "unique_quiet",
    "universal_quiet")
)
}
\arguments{
\item{...}{object or expression to convert to a tibble}

\item{.rowname}{string; to name the column containing rownames. If rownames are not
present in the dataframe, \code{.rowname} is ignored.}

\item{.name_repair}{Treatment of problematic column names:
\itemize{
\item \code{"minimal"}: No name repair or checks, beyond basic existence,
\item \code{"unique"}: Make sure names are unique and not empty,
\item \code{"check_unique"}: (default value), no name repair, but check they are
\code{unique},
\item \code{"universal"}: Make the names \code{unique} and syntactic
\item a function: apply custom name repair (e.g., \code{.name_repair = make.names}
for names in the style of base R).
\item A purrr-style anonymous function, see \code{\link[rlang:as_function]{rlang::as_function()}}
}

This argument is passed on as \code{repair} to \code{\link[vctrs:vec_as_names]{vctrs::vec_as_names()}}.
See there for more details on these terms and the strategies used
to enforce them.}
}
\value{
a tibble with rownames in a column
}
\description{
Default behaviors:
\itemize{
\item Always includes rownames in tibble, if available
\item Does not repeat input elements to obtain common lengths
\item Arranges lists with uniform arrays into columns of a single tibble
\item A list of ragged elements becomes a two column tibble where the first column contains names and
the second column is a nested list
\item Is unconcerned with duplicate column names unless specified by user with .name_repair
}
}
\details{
@details
Built to work with the function \code{\link[=xl]{xl()}} to produce 2D output for spreadsheets.
\code{entibble()} is liberal about names, such as duplicate column names, and tries to produce
flat 2D data in place of nested data.
}
\examples{
letters |> entibble()

# naming examples
(example_data <- letters |>  purrr::set_names(LETTERS))
example_data |> entibble()
example_data |> entibble(.rowname = 'a user-specified name')
example_data |> entibble(.name_repair = ~c('name_A', 'name_B'))
entibble(example_data, example_data)
entibble(x = 1:3, x = 11:13, .name_repair = 'unique_quiet')
mtcars |> entibble(.rowname = 'vehicle')

# nests ragged data
entibble(example_data, !!!letters)

# spreads conforming rows and lists of conforming rows
entibble(x = 1:3, y = 11:13)
entibble(x = 1:3, y = 10:14)

# be somewhat careful about conforming-row inputs; it is different from `tibble()`
enlist(head(iris), tail(mtcars)) |> entibble()
as.matrix(list(1:10,11:20)) |> entibble()

# if you want to ensure separate lists, call [purrr::map()] or [list_iron()]
enlist(head(iris), tail(mtcars)) |> purrr::map(entibble)
enlist(head(iris), tail(mtcars)) |> list_iron(.f = entibble)

# makes multi-dimensional tables 2D:
Titanic |> dim()
Titanic |> entibble()

}
\seealso{
\code{\link[tibble:tibble]{tibble::tibble()}}
}
