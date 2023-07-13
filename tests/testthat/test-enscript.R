test_that('invisible deparsed output and quiet', {
  expect_equal(
    1:5 |> enscript(quiet = T),
    "1:5"
  )
})

test_that('width works', {
  expect_equal(
    dplyr::starwars$name[1:5] |> enscript(width = 4, quiet = T),
    c(
      "", "c(", "\"Luke Skywalker\", ", "\"C-3PO\", ", "\"R2-D2\", ",
      "\"Darth Vader\", ", "\"Leia Organa\"", ")", ""
    )
  )
  expect_equal(
    dplyr::starwars$name[1:5] |> enscript(width = 80, quiet = T),
    "c(\"Luke Skywalker\", \"C-3PO\", \"R2-D2\", \"Darth Vader\", \"Leia Organa\")"
  )
})

test_that('iris script (copied to user clipboard) is correct: datasets::iris |> head() |> enscript()', {
  expect_equal(
    structure(
      list(
        Sepal.Length = c(
          5.1, 4.9, 4.7, 4.6, 5, 5.4
        ),
        Sepal.Width = c(
          3.5, 3, 3.2, 3.1, 3.6, 3.9
        ),
        Petal.Length = c(
          1.4, 1.4, 1.3, 1.5, 1.4, 1.7
        ),
        Petal.Width = c(
          0.2, 0.2, 0.2, 0.2, 0.2, 0.4
        ),
        Species = structure(
          c(
            1L, 1L, 1L, 1L, 1L, 1L
          ),
          levels = c(
            "setosa", "versicolor", "virginica"
          ),
          class = "factor"
        )
      ),
      row.names = c(
        NA, 6L
      ),
      class = "data.frame"
    ),
    datasets::iris |> head()
  )
})

test_that('starwars script (copied to user clipboard) is correct: dplyr::starwars |> tibble::tibble() |> head() |> enscript()', {
  expect_equal(
    structure(
      list(
        name = c(
          "Luke Skywalker", "C-3PO", "R2-D2", "Darth Vader", "Leia Organa",
          "Owen Lars"
        ),
        height = c(
          172L, 167L, 96L, 202L, 150L, 178L
        ),
        mass = c(
          77, 75, 32, 136, 49, 120
        ),
        hair_color = c(
          "blond", NA, NA, "none", "brown", "brown, grey"
        ),
        skin_color = c(
          "fair", "gold", "white, blue", "white", "light", "light"
        ),
        eye_color = c(
          "blue", "yellow", "red", "yellow", "brown", "blue"
        ),
        birth_year = c(
          19, 112, 33, 41.9, 19, 52
        ),
        sex = c(
          "male", "none", "none", "male", "female", "male"
        ),
        gender = c(
          "masculine", "masculine", "masculine", "masculine", "feminine",
          "masculine"
        ),
        homeworld = c(
          "Tatooine", "Tatooine", "Naboo", "Tatooine", "Alderaan", "Tatooine"
        ),
        species = c(
          "Human", "Droid", "Droid", "Human", "Human", "Human"
        ),
        films = list(
          c(
            "The Empire Strikes Back", "Revenge of the Sith",
            "Return of the Jedi", "A New Hope", "The Force Awakens"
          ),
          c(
            "The Empire Strikes Back", "Attack of the Clones",
            "The Phantom Menace", "Revenge of the Sith", "Return of the Jedi",
            "A New Hope"
          ),
          c(
            "The Empire Strikes Back", "Attack of the Clones",
            "The Phantom Menace", "Revenge of the Sith", "Return of the Jedi",
            "A New Hope", "The Force Awakens"
          ),
          c(
            "The Empire Strikes Back", "Revenge of the Sith",
            "Return of the Jedi", "A New Hope"
          ),
          c(
            "The Empire Strikes Back", "Revenge of the Sith",
            "Return of the Jedi", "A New Hope", "The Force Awakens"
          ),
          c(
            "Attack of the Clones", "Revenge of the Sith", "A New Hope"
          )
        ),
        vehicles = list(
          c(
            "Snowspeeder", "Imperial Speeder Bike"
          ),
          character(0),
          character(0),
          character(0),
          "Imperial Speeder Bike",
          character(0)
        ),
        starships = list(
          c(
            "X-wing", "Imperial shuttle"
          ),
          character(0),
          character(0),
          "TIE Advanced x1",
          character(0),
          character(0)
        )
      ),
      row.names = c(
        NA, -6L
      ),
      class = c(
        "tbl_df", "tbl", "data.frame"
      )
    ),
    dplyr::starwars |> tibble::tibble() |> head()
  )
})

test_that('test another expression rlang::set_names(letters, LETTERS) |> enlist() |> enscript()', {
  expect_equal(
    list(
      "rlang::set_names(letters, LETTERS)" = c(
        A = "a", B = "b", C = "c", D = "d", E = "e", F = "f", G = "g",
        H = "h", I = "i", J = "j", K = "k", L = "l", M = "m", N = "n",
        O = "o", P = "p", Q = "q", R = "r", S = "s", T = "t", U = "u",
        V = "v", W = "w", X = "x", Y = "y", Z = "z"
      )
    ),
    rlang::set_names(letters, LETTERS) |> enlist()
  )
})





