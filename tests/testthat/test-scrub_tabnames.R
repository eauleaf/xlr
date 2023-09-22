# # {.fn .fix_forbidden_chars) ----------------------------------------------
#
# test_that("Replacement chars in tabnames work correctly.", {
#
#   expect_equal(.fix_forbidden_tabnames(" ''''''", quiet = T), " ")
#   expect_equal("'''1' '" |> .fix_forbidden_tabnames(quiet = T), "1' ")
#   expect_equal(.fix_forbidden_tabnames(quiet = T, "'[ ]hi];'"), "( )hi);")
#   expect_equal("!@#$%^&*()_+ {HI} ^" |> .fix_forbidden_tabnames(quiet = T), "!@#$%^&#()_+ {HI} ^")
#   expect_equal(purrr::map(enlist(NA, '[hi?]'), ~.fix_forbidden_tabnames(., quiet = T)),
#                list( "NA" = as.character(NA), "[hi?]" = "(hi!)"))
#   expect_equal("'histor'y'" |> .fix_forbidden_tabnames(quiet = T), "histor'y")
#   expect_equal("[:/] " |> .fix_forbidden_tabnames(quiet = T), "(--) ")
#   expect_equal("?/a\\" |> .fix_forbidden_tabnames(quiet = T), "!-a-")
#   expect_equal("?,*?*?'" |> .fix_forbidden_tabnames(quiet = T), "!,#!#!")
#   expect_equal("[hIStory coursework]" |> .fix_forbidden_tabnames(quiet = T), "(hISt coursework)")
#   expect_equal("'HISTORIC' History '''" |> .fix_forbidden_tabnames(quiet = T), "HISTORIC' Hist ")
#   expect_equal("*[history' buff]*' " |> .fix_forbidden_tabnames(quiet = T), "#(hist' buff)#' ")
#   expect_equal(NA |> .fix_forbidden_tabnames(quiet = T), NA_character_)
#   # character(0) is fine. NULL case caught in scrub_tabnames()
#   expect_equal(NULL |> .fix_forbidden_tabnames(quiet = T), character(0))
#   expect_equal(c(NA, '[hi?]',"") |> .fix_forbidden_tabnames(quiet = T), c(NA, "(hi!)", ""))
#   expect_equal(.fix_forbidden_tabnames('🎊', quiet = T), '🎊')
#   expect_equal("'`` '?HisTory,*?*!@#$%^&*()_+ '[{1'}]'[:/\\]''" |>
#                  .fix_forbidden_tabnames(quiet = T), "`` '!HisT,#!#!@#$%^&#()_+ '({1'})'(---)")
#   expect_equal(.fix_forbidden_tabnames(1:10, quiet = T), as.character(1:10))
#
# })
#
#
# # {.fn .check_forbidden_pad} -----------------------------------------------
#
# test_that("All forbidden characters, multi-char, and non-chars throw errors.", {
#   expect_error(.check_forbidden_pad(":"))
#   expect_error(.check_forbidden_pad("\\"))
#   expect_error(.check_forbidden_pad("/"))
#   expect_error(.check_forbidden_pad("*"))
#   expect_error(.check_forbidden_pad("?"))
#   expect_error(.check_forbidden_pad("]"))
#   expect_error(.check_forbidden_pad("["))
#   expect_error(.check_forbidden_pad("'"))
#   expect_error(.check_forbidden_pad("aa"))
#   expect_error(.check_forbidden_pad(".."))
#   expect_error(.check_forbidden_pad("  "))
#   expect_error(.check_forbidden_pad(11))
#   expect_error(.check_forbidden_pad(1:2))
#   expect_error(.check_forbidden_pad('00'))
#   expect_error(.check_forbidden_pad(0))
#   expect_error(.check_forbidden_pad(''))
#   expect_error(.check_forbidden_pad(NA))
#   expect_error(.check_forbidden_pad(NULL))
# })
#
# test_that("Coercion, single, odd, or no chars work.", {
#   expect_equal(.check_forbidden_pad(" "), ' ')
#   expect_equal(.check_forbidden_pad("-"), '-')
#   expect_equal(.check_forbidden_pad("."), '.')
#   expect_equal(.check_forbidden_pad('0'), '0')
#   expect_equal(.check_forbidden_pad('🎊'), '🎊')
# })
#
#
#
# # (.fn .check_tabwidth} ---------------------------------------------------
#
# test_that("Tabname widths okayed or overridden with message.", {
#   expect_equal(.check_tabwidth(32, quiet = T), 31)
#   expect_equal(.check_tabwidth(5), 5 )
#   expect_message(.check_tabwidth(100))
#   expect_message(.check_tabwidth(5, 6))
# })
#
# test_that("Unreasonable tab width args fail with error.", {
#   expect_error(.check_tabwidth(NA) )
#   expect_error(.check_tabwidth(NULL) )
#   expect_error(.check_tabwidth(-1) )
# })
#
#
#
# # {.fn .paste_names} ------------------------------------------------------
#
# test_that("Switching paste by side works correctly.", {
#   expect_equal(.paste_names('--1', c('hello','goodbye')), c("hello.--1", "goodbye.--1"))
#   expect_equal(.paste_names(1:2, c('hello','goodbye'), '>>', 'left'), c("1>>hello", "2>>goodbye"))
#   expect_equal(.paste_names('NA', c('hello','goodbye'), '_'), c("hello_NA", "goodbye_NA"))
#   expect_equal(.paste_names('NULL', c('hello','goodbye'), '_'), c("hello_NULL", "goodbye_NULL"))
# })
#
#
#
# # {.fn .uniquify_tabnames} ------------------------------------------------
#
# test_that("Tabnames are forced to be unique within repeated groupings.", {
#   expect_equal(.uniquify_tabnames(NULL), NULL)
#   expect_equal(.uniquify_tabnames(NA), NA_character_)
#   expect_equal(
#     .uniquify_tabnames(c("a", rep("b",10), 'c'), quiet = TRUE),
#     c("a", "b->.1", "b->.2", "b->.3", "b->.4", "b->.5", "b->.6", "b->.7", "b->.8","b->.9", "b->10", "c")
#     )
#   expect_equal(
#     .uniquify_tabnames(c("test", "test", "another", "another", "nonduplicate"), quiet = T, sep = '.'),
#     c("test.1", "test.2", "another.1","another.2", "nonduplicate"))
#
# })
#
#
# test_that("Truncation & recursion messages work", {
#   expect_message(
#     .uniquify_tabnames( rep('flying_spaghetti', 3), width = 5)
#   )
#   expect_message(
#     .uniquify_tabnames(rep(c('ab','ac'), 3), width = 4)
#   ) |> expect_message() |> expect_message()
# })
#
#
# test_that("Long vectors and names trucate correctly.", {
#   expect_equal(
#     .uniquify_tabnames( rep('flying_spaghetti', 3), width = 5, quiet = T)
#     ,c("fl->1", "fl->2", "fl->3")
#   )
#   expect_equal(
#     .uniquify_tabnames(rep('fly', 3), width = 3, quiet = T)
#     ,c("->1", "->2", "->3")
#   )
#   expect_equal(
#     .uniquify_tabnames(rep(c('ab','ac'), 3), width = 4, quiet = T)
#     ,c("a->1", "a->2", "a->3", "a->4", "a->5", "a->6")
#   )
#   expect_equal(
#     .uniquify_tabnames(rep(c('ab','ac'), 3), width = 3, sep = '', quiet = T)
#     ,c("ab1", "ac1", "ab2", "ac2", "ab3", "ac3")
#   )
#   expect_equal(
#     .uniquify_tabnames(rep(c('ab','acdc'), 3), width = 1, sep = '', quiet = T)
#     ,paste(1:6)
#   )
#   expect_equal(
#     .uniquify_tabnames(rep(c('ab','acdc'), 2), width = 4, ellipsis = '~', sep = '|', quiet = T)
#     ,c("ab|1", "a~|1", "ab|2", "a~|2")
#   )
#   expect_equal(
#     .uniquify_tabnames(rep(c('ab','acdc'), 2), width = 4, ellipsis = '~~', sep = '|', quiet = T)
#     ,c("ab|1", "~~|1", "ab|2", "~~|2")
#   )
#   expect_equal(
#     .uniquify_tabnames(rep(c('ab','acdc'), 5), width = 4, ellipsis = '~', sep = '|', truncate_side = 'center', quiet = T)
#     ,c("ab|1", "a~|1", "ab|2", "a~|2", "ab|3", "a~|3", "ab|4", "a~|4", "ab|5", "a~|5")
#   )
#   expect_equal(
#     .uniquify_tabnames(rep(c('ab','acdc'), 2), width = 5, ellipsis = '~', sep = '|', truncate_side = 'center', quiet = T)
#     ,c("ab|1", "a~c|1", "ab|2", "a~c|2")
#   )
#   expect_equal(
#     .uniquify_tabnames(rep(NA, 10), width = 5, ellipsis = '~', sep = '|', paste_side = 'left', quiet = T)
#     ,c(".1|NA", ".2|NA", ".3|NA", ".4|NA", ".5|NA", ".6|NA", ".7|NA", ".8|NA", ".9|NA", "10|NA")
#   )
#   expect_equal(
#     .uniquify_tabnames('supercalifragalisticexpealidocious', quiet = T, width = 7, truncate_side = 'center', ellipsis = '~')
#     ,"sup~ous"
#   )
#
# })
#
#
#
# # {.fn scrub_tabnames} ----------------------------------------------------
#
# test_that("Replacement chars in tabnames work correctly.", {
#
#   expect_equal(scrub_tabnames(" ''''''", quiet = T), " ")
#   expect_equal("'''1' '" |> scrub_tabnames(quiet = T), "1' ")
#   expect_equal(scrub_tabnames(quiet = T, "'[ ]hi];'"), "( )hi);")
#   expect_equal("!@#$%^&*()_+ {HI} ^" |> scrub_tabnames(quiet = T), "!@#$%^&#()_+ {HI} ^")
#   expect_error(scrub_tabnames(NULL))
#   expect_equal(purrr::map(enlist(NA, '[hi?]'), ~scrub_tabnames(., quiet = T)),
#                list("NA" = "NA", "[hi?]" = "(hi!)"))
#   expect_equal("'histor'y'" |> scrub_tabnames(quiet = T), "histor'y")
#   expect_equal("[:/] " |> scrub_tabnames(quiet = T), "<--> ")
#   expect_equal("?/a\\" |> scrub_tabnames(quiet = T), "!-a-")
#   expect_equal("?,*?*?'" |> scrub_tabnames(quiet = T), "!,#!#!")
#   expect_equal("[hIStory coursework]" |> scrub_tabnames(quiet = T), "<hISt coursework>")
#   expect_equal("'HISTORIC' History '''" |> scrub_tabnames(quiet = T), "HISTORIC' Hist ")
#   expect_equal("*[history' buff]*' " |> scrub_tabnames(quiet = T), "#<hist' buff>#' ")
#   expect_equal(NA |> scrub_tabnames(quiet = T), "NA")
#   expect_equal(c(NULL, NA, '[hi?]',"") |> scrub_tabnames(quiet = T), c("NA", "<hi!>", ""))
#   expect_equal(scrub_tabnames('🎊', quiet = T), '🎊')
#   expect_equal(scrub_tabnames("'`` '?HisTory,*?*!@#$%^&*()_+ '[{1'}]'[:/\\]''", quiet = T), "`` '!HisT,#!#!@#$%^&#()_+ ")
#   expect_equal(scrub_tabnames(1:10, quiet = T), as.character(1:10))
#
# })
#
# test_that("Vector or forbidden characters are replaced correctly, one-for-one, except preceeding singlequotes", {
#   expect_equal(
#     scrub_tabnames(c("\\:blue/:", "red", "gr?*een///////", "[]", "[orang[e]", "", NA, "'don't'"), quiet = T),
#     c("--blue--", "red", "gr!#een-------", "<>", "<orang<e>", "", "NA", "don't")
#   )
#   expect_equal(
#     scrub_tabnames(c("\\:blue/:", "red", "gr?*een///////", "[]", "[orang[e]", "", NA, "'don't'"), quiet = T) |> nchar(),
#     c("--blue--", "red", "gr!#een-------", "<>", "<orang<e>", "", "NA", "don't") |> nchar()
#   )
#   expect_equal(
#     scrub_tabnames(c("'''/ hello '", "red'head;'"), quiet = T),
#                    c("- hello ", "red'head;")
#   )
# })
#
#
# test_that("Each forbidden character is correctly swapped.", {
#   expect_equal(
#     list("\\", "/", ":", "?", "*", "'", "[", "]",'HISTORY', NA) |>
#       purrr::map(~scrub_tabnames(., quiet = T)),
#     list("-", "-", "-", "!", "#", "", "<", ">", 'HIST', "NA")
#     )
# })
#
# test_that("Each forbidden character is correctly swapped and duplicates are corrected.", {
#   expect_equal(
#     c("\\", "/", ":", "?", "*", "'", "[", "]",'HISTORY', NA) |> scrub_tabnames(quiet = T),
#     c("-.1", "-.2", "-.3", "!", "#", "", "<", ">", "HIST", "NA")
#   )
# })
#
#
# test_that("NULL tabname throws error", {
#   expect_error(scrub_tabnames(NULL, quiet = T))
# })
#
#
# test_that("Too many characters in 'sep' throws error.", {
#   # nchar("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
#   expect_error( scrub_tabnames('', sep = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", quiet = T) )
#
#   # nchar('----------------------------->')
#   chars30 <- '----------------------------->'
#   expect_equal(
#     scrub_tabnames(c("",""), sep = chars30, quiet = T),
#     c("----------------------------->1", "----------------------------->2")
#     )
#
#   expect_error( scrub_tabnames(rep("", 10), sep = chars30, quiet = T) )
#
# })
#
# test_that("'sep' only used when uniquifying tabnames", {
#   expect_equal(
#   list("\\", "/", ":", "?", "*", "'", "[", "]", NA, 1, 'a') |>
#     purrr::map(~scrub_tabnames('', sep = ., quiet = T)),
#   list("", "", "", "", "", "", "", "", "", "", "")
#   )
# })
#
# test_that("'sep' fails if non-scalar", {
#   expect_error( scrub_tabnames('', sep = 1:5, quiet = T) )
# })
#
#
# test_that("No forbidden characters in 'sep'", {
#   expect_equal(
#   list("\\", "/", ":", "?", "*", "'", "[", "]",NA, 1, 'a') |>
#     purrr::map(~scrub_tabnames(c(NA, NA), sep = ., quiet = T)),
#   list(
#     c("NA-1", "NA-2"), c("NA-1", "NA-2"), c("NA-1", "NA-2"), c("NA!1", "NA!2"),
#     c("NA#1","NA#2"), c("NA1", "NA2"), c("NA<1", "NA<2"), c("NA>1", "NA>2"),
#     c("NA1", "NA2"), c("NA11","NA12" ), c("NAa1", "NAa2")
#   )
# )
# })
#
# test_that("'pad' fails if non-scalar or more than 2 chars", {
#   expect_error( scrub_tabnames('', pad = 1:5, quiet = T) )
#   expect_error( scrub_tabnames('', pad = 'aa', quiet = T) )
# })
#
#
# list("", NA, NULL, "\\", "/", ":", "?", "*", "'", "[", "]", "aa", 1, c("a", "b")) |>
#   purrr::walk(~test_that("No forbidden characters in 'pad'", {
#     expect_error(scrub_tabnames("blue", pad = ., quiet = TRUE))
#   }))
#
# test_that("unreasonable ", {
#
# })
#
# test_that("Tabname widths are reasonable, truncated correctly, and overridden with message.", {
#   expect_equal(scrub_tabnames("a long name to test out for trunc", max_width = 32, quiet = T) |> nchar(), 31)
#   expect_equal(scrub_tabnames("a long name to test out with trunc", max_width = 10, truncate_side = 'left') , "with trunc")
#   expect_equal(scrub_tabnames("a long name to test out with trunc", max_width = 15, truncate_side = 'center'), "a long ~h trunc")
#   expect_equal(scrub_tabnames("", max_width = 32, quiet = T) , "")
#   expect_equal(scrub_tabnames("", max_width = 5) , "" )
#   expect_message(scrub_tabnames("", max_width = 32))
#   expect_message(scrub_tabnames("", max_width = 100))
#   expect_message(scrub_tabnames(1:5, max_width = 0) )
#   expect_equal( scrub_tabnames(1:5, max_width = 0, quiet = TRUE), paste(1:5))
#   expect_equal( scrub_tabnames(NA, max_width = 20), 'NA')
# })
#
# test_that("Unreasonable tab width args fail with error.", {
#   expect_error(scrub_tabnames("", max_width = -5) )
#   expect_error(scrub_tabnames("", max_width = NA) )
#   expect_error(scrub_tabnames("", max_width = NULL) )
#   expect_error(scrub_tabnames("", max_width = -1) )
#   expect_error(scrub_tabnames("", max_width = "green") )
# })
#
# test_that("Tests that scrub_tabnames variable 'paste_side' works.", {
#   expect_equal(
#     scrub_tabnames(c('','','hello','hello','goodbye'), paste_side = "right", quiet = T),
#     c(".1", ".2", "hello.1", "hello.2", "goodbye"))
#   expect_equal(
#     scrub_tabnames(c('','','hello','hello','goodbye'), paste_side = "right", sep = '_', quiet = T),
#     c("_1", "_2", "hello_1", "hello_2", "goodbye"))
#   expect_equal(
#     scrub_tabnames(c('','','hello','hello','goodbye'), paste_side = "right", sep = '"~(._.)~"', quiet = T),
#     c("\"~(._.)~\"1", "\"~(._.)~\"2", "hello\"~(._.)~\"1", "hello\"~(._.)~\"2", "goodbye"))
#   expect_equal(scrub_tabnames(c('','','hello','hello','goodbye'), sep = '<-', paste_side = "left", quiet = T),
#                c("1<-", "2<-", "1<-hello", "2<-hello", "goodbye"))
#   expect_equal(scrub_tabnames(c('','','hello','hello','goodbye'), sep = '<-', max_width = 1, truncate_side = 'left', paste_side = "left", quiet = T),
#                c("1<-", "2<-", "1<-hello", "2<-hello", "goodbye"))
#   expect_error(scrub_tabnames(c('','','hello','hello','goodbye'), paste_side = "green"))
# })
#
#
#
# # # Test case 3: Check if the function correctly handles input that is not a character vector
# # test_that('Non-character input is handled correctly', { expect_error( scrub_tabnames(1:5), 'Expected a character
# # vector input' ) }) # Test case 4: Check if the function correctly handles tab names longer than the maximum
# # width test_that('Tab names longer than max_width are truncated correctly', { expect_equal(
# # scrub_tabnames(c('abcdefghijklmnopqrstuvwxyzabcdef'), max_width = 10), c('abcdefghij') ) }) # Test case 5: Check
# # if the function handles 'center' truncation correctly test_that(''Center' truncation is handled correctly', {
# # expect_equal( scrub_tabnames(c('abcdefghijklmnopqrstuvwxyzabcdef'), max_width = 10, truncate_side = 'center'),
# # c('abcde~fghij') ) }) # Test case 6: Check if the function correctly handles 'history' special case
# # test_that(''History' special case is handled correctly', { expect_equal( scrub_tabnames(c('history', 'History',
# # 'HISTORY')), c('hist', 'Hist', 'HIST') ) }) # Test case 7: Check if the function correctly handles empty strings
# # and NA values test_that('Empty strings and NA values are handled correctly', { expect_equal(
# # scrub_tabnames(c('', NA)), c('', NA) ) }) # Test case 8: Check if the function handles strings that have leading
# # or trailing apostrophes correctly test_that('Leading or trailing apostrophes are removed correctly', {
# # expect_equal( scrub_tabnames(c(''don't worry'', '`NA`', ''HIS'T'', 'Entire History of 'the' \'Universe\'')),
# # c('don't worry', 'NA', 'HIST', 'Entire History of 'the' Universe') ) }) test_that(''pad' of NA fails',
# # {scrub_tabnames(pad = ) testthat::expect_error()
#
#
#
#


#' tab_names <- c(NA, "''''''", "[]hi];", "'HI'", "'hist''", "[:/]", "?/a\\\\",
#' "'?,*?*'", "'[history coursework]'", "'HISTORIC'AL''''")
#' xlr:::.forbidden_chars_replace(tab_names, pattern = '[\\\\/:]', replacement =
#' '-', pattern_text = '\\, /, or :') xlr:::.forbidden_chars_replace(tab_names,
#' pattern = '[?*]', replacement = '#', pattern_text = '? or *')
#' xlr:::.forbidden_chars_replace(tab_names, pattern = '(?i)(hist)ory',
#' replacement = '\\1', pattern_text = 'history', repl_text = 'hist')
#' xlr:::.forbidden_chars_replace(tab_names, pattern = '\\[', replacement = '{', pattern_text = '[brackets]', repl_text = '{curly braces}')
#' xlr:::.forbidden_chars_replace(tab_names, pattern = ']', replacement = '}', quiet = TRUE)
#' xlr:::.forbidden_chars_replace(tab_names, pattern = "^'+", replacement = '`',
#' pattern_text = "single quotes '' at tabname start or end", repl_text = "the
#' empty string ''") xlr:::.forbidden_chars_replace(tab_names, pattern = "'+$",
#' replacement = '', quiet = TRUE)


# scrub_tabnames
#' scrub_tabnames(1:5)
#' c('\\:blue/:', 'red', 'gr?*een///////', '[]', '[orange]','', NA) |> scrub_tabnames()
#' c('history', NA, 'HISTORY', 'Entire History of the Universe',NULL,NULL) |> scrub_tabnames()
#' c("'\'don't worry\''", '`NA`', NA, "'HIS'T''", 'Entire History of \'the\' \'Universe\'') |> scrub_tabnames()
#' c('\\/:[]?*', '\\?:*/[]', '~!@#$%^&()-_=+{}|;:,<.> ') |> scrub_tabnames()
#' paste0(names(datasets::precip),'/',datasets::precip) |> scrub_tabnames(max_width = 10)
#' paste0(names(datasets::precip),'/',datasets::precip) |> scrub_tabnames(max_width = 8, sep = '_#', pad = '0')
#' paste0(names(datasets::precip),'/',datasets::precip) |> scrub_tabnames(max_width = 0, sep = '...', pad = '0')
#' paste0(names(datasets::precip),'/',datasets::precip) |> scrub_tabnames(max_width = 0, sep = '..', pad = '.')
#' rep('', 15) |> scrub_tabnames(max_width = 0, sep = '..', pad = '.')
#' # rep('', 15) |> scrub_tabnames(max_width = 0, sep = '', pad = '') # expect_fail
#' # rep('', 15) |> scrub_tabnames(max_width = 0, sep = '', pad = ' ') # expect_fail
#' # dplyr::starwars |> dplyr::mutate(new_name = paste(name,'of', homeworld)) |> dplyr::pull(new_name) |> scrub_tabnames(max_width = 20, sep = '//', pad = ']') # expect_fail
#' # dplyr::starwars |> dplyr::mutate(new_name = paste(name,'of', homeworld)) |> dplyr::pull(new_name) |> scrub_tabnames(max_width = 0, sep = '.', pad = '.') # expect_fail
#'
#' # when stringr::str_trunc() gets fixed, the below code will work.
#' ### dplyr::starwars |> dplyr::mutate(new_name = paste(name,'of', homeworld)) |> dplyr::pull(new_name) |> scrub_tabnames(max_width = 0, sep = '', pad = '.') # ??????
#' # if you request a width of characters that is fewer than your replacement characters, you get some weird looking names:
#' dplyr::starwars |> dplyr::mutate(new_name = paste(name,'of', homeworld)) |> dplyr::pull(new_name) |> scrub_tabnames(max_width = 2, sep = '...', pad = '.')
#' # However, if you call zero width..., that's useful for naming.
#' dplyr::starwars |> dplyr::mutate(new_name = paste(name,'of', homeworld)) |> dplyr::pull(new_name) |> scrub_tabnames(max_width = 0)
#' dplyr::starwars |> dplyr::mutate(new_name = paste(name,'of', homeworld)) |> dplyr::pull(new_name) |> scrub_tabnames(max_width = 0, sep = 'tab > ', pad = '0')
#' dplyr::starwars |> dplyr::mutate(new_name = paste(name,'of', homeworld)) |> dplyr::pull(new_name) |> scrub_tabnames(max_width = 0,truncate_side = 'center')
