test_that("setting xlr options works", {
   store_options <- getOption('xlr.xl')
   set_xl_args(reset = TRUE)
   expect_equal(NULL, getOption('xlr.xl'))

   set_xl_args(.quiet = TRUE, .tabname_spec = list(zoom = 65))
   expect_equal(list(.quiet = TRUE, .tabname_spec = list(zoom = 65)), getOption('xlr.xl'))

   set_xl_args(.workbook_spec = list(asTable = FALSE, zoom = 85, withFilter = TRUE))
   expect_equal(
     list(
       .quiet = TRUE, .tabname_spec = list(zoom = 65),
       .workbook_spec = list(asTable = FALSE, zoom = 85, withFilter = TRUE)
     ),
     getOption('xlr.xl')
   )

   set_xl_args(.quiet = FALSE, reset = TRUE)
   expect_equal(getOption('xlr.xl'), list(.quiet = FALSE))

   # overwrites names, i.e. doesn't record names twice
   set_xl_args(.sheet_titles = toupper)
   set_xl_args(.sheet_titles = toupper)
   expect_equal(names(getOption('xlr.xl')), c(".quiet", ".sheet_titles"))

   set_xl_args(.sheet_titles = toupper,reset = TRUE)
   expect_equal(names(getOption('xlr.xl')), c(".sheet_titles"))


   # restore original options
   set_xl_args(store_options, reset = TRUE)

})
