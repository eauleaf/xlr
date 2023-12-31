% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_iron.R
\name{list_iron}
\alias{list_iron}
\title{Iron embedded lists out of a list of objects}
\usage{
list_iron(
  ...,
  name_spec = "{outer}|{inner}",
  name_repair = c("minimal", "unique", "check_unique", "universal", "unique_quiet",
    "universal_quiet"),
  .f = identity
)
}
\arguments{
\item{...}{objects or list of objects.}

\item{name_spec}{If both inner and outer names are present, control
how they are combined. Should be a glue specification that uses
variables \code{inner} and \code{outer}.}

\item{name_repair}{One of \code{"minimal"}, \code{"unique"}, \code{"universal"}, or
\code{"check_unique"}. See \code{\link[vctrs:vec_as_names]{vctrs::vec_as_names()}} for the meaning of these
options.}

\item{.f}{function to apply to every object in the list; default function is \code{identity}.}
}
\value{
A flat named list of objects
}
\description{
Recursively dis-embed objects in lists of lists to a single, linear list of list objects.
}
\details{
\itemize{
\item If object is not in a list, \code{list_iron} places the object in a list.
\item Removes all empty embedded lists.
\item Removes empty comma-separated arguments.
}

Because spreadsheets tabs have no hierarchical structure, any list of data
objects to prepare for presentation within a workbook must be flattened to sheets.
This function is a precursor for printing listed data objects with \link{xlr}.

\code{list_iron} forces all objects in an embedded object list to become a
single-file list, and optionally conform to a function passed by the user, e.g. \code{\link[=entibble]{entibble()}}.
\code{list_iron} acts similar to \code{\link[purrr:list_flatten]{purrr::list_flatten()}}, but collapses all embedded
lists rather than a single layer. Also concatenates embedded-list names via parameter \code{name_spec}.
}
\examples{
head(iris) |> list_iron()
head(iris) |> list() |> list() |> list() |> list_iron()
list_iron(list(list(iris)), mtcars, .f = ~tail(., 2))

# Compare list structure to purrr::list_flatten():
a_list <- list(list(1, list(), 2, list(3)))
a_list |> str()
a_list |> purrr::list_flatten() |> str()
a_list |> list_iron() |> str()

# Naming examples:
messy_list <- list(list(list(1:5), a = list(5:1, 'green', list('blue')), letters))
messy_list |> str()
messy_list |> list_iron() |> str()
messy_list |> list_iron(name_repair = 'unique') |> names()
messy_list |> list_iron(name_spec = '', name_repair = 'unique') |> names()
messy_list |> list_iron(name_spec = '{outer}', name_repair = 'unique') |> names()

}
