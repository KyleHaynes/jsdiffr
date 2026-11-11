red <- "\033[31m"
green <- "\033[32m"
reset <- "\033[0m"

strip <- function(x) gsub("\033\\[[0-9;]*m", "", x, perl = TRUE)

test_that("markup_to_ansi converts markers to colour", {
  expect_identical(
    markup_to_ansi("ab[-c-]{+d+}", color = TRUE),
    paste0("ab", red, "c", reset, green, "d", reset)
  )
  # colour off leaves the markup alone - the markers are all the information
  # there is without colour.
  expect_identical(markup_to_ansi("ab[-c-]{+d+}", color = FALSE),
                   "ab[-c-]{+d+}")
  expect_identical(markup_to_ansi(character(0), color = TRUE), character(0))
  expect_identical(markup_to_ansi(NA_character_, color = TRUE), NA_character_)
  expect_error(markup_to_ansi("x", add = "y", color = TRUE), "length-2")
})

test_that("markup_to_ansi handles custom markers and newlines", {
  expect_identical(
    markup_to_ansi("ab-[c]+[d]", add = c("+[", "]"), remove = c("-[", "]"),
                   color = TRUE),
    paste0("ab", red, "c", reset, green, "d", reset)
  )
  # a line diff's markup spans newlines, so the regex must not stop at one
  expect_identical(
    markup_to_ansi("a\n[-b\n-]{+c\n+}", color = TRUE),
    paste0("a\n", red, "b\n", reset, green, "c\n", reset)
  )
  withr::with_options(
    list(jsdiffr.markup_add = c("<", ">"), jsdiffr.markup_remove = c("(", ")")),
    expect_identical(markup_to_ansi("ab(c)<d>", color = TRUE),
                     paste0("ab", red, "c", reset, green, "d", reset))
  )
})

test_that("has_markup detects diff columns", {
  expect_true(has_markup(diff_words(c("the cat", "a b"), c("the dog", "a b"))))
  expect_false(has_markup(c("no", "markup", "here")))
  expect_false(has_markup(character(0)))

  dt <- data.table::data.table(a = c("the cat", "a b"), b = c("the dog", "a b"))
  expect_true(has_markup(dt[, .(d = diff_words(a, b), a)]))
  expect_false(has_markup(dt))
  # a format = "changes" list column counts too
  expect_true(has_markup(dt[, .(d = diff_words(a, b, format = "changes"))]))
  expect_false(has_markup(data.table::data.table(l = list(1, 2))))

  # only the head and tail are scanned, so that long tables stay cheap
  long <- c(rep("x", 50L), "a[-b-]", rep("x", 50L))
  expect_false(has_markup(long, max_scan = 5L))
  expect_true(has_markup(long, max_scan = 60L))
})

test_that("diff_print colours a table and keeps columns aligned", {
  df <- data.frame(old = c("the cat sat", "12 Smith St"),
                   new = c("the dog sat", "12 Smith Street"))
  df$diff <- diff_words(df$old, df$new)
  df$n <- 1:2

  out <- capture.output(diff_print(df, color = TRUE))
  expect_length(out, 3L)                       # header + 2 rows
  expect_match(out[2L], red, fixed = TRUE)
  expect_match(out[2L], green, fixed = TRUE)
  expect_false(any(grepl("[-", out, fixed = TRUE)))

  # alignment is measured on the visible width, so the escapes must not shift
  # the trailing column
  plain <- strip(out)
  expect_identical(nchar(plain[2L]), nchar(plain[3L]))
  expect_identical(regmatches(plain[1L], regexpr("n$", plain[1L])), "n")

  # colour off prints the markup as it stands
  bw <- capture.output(diff_print(df, color = FALSE))
  expect_match(bw[2L], "[-cat-]{+dog+}", fixed = TRUE)
  expect_false(any(grepl("\033", bw, fixed = TRUE)))
})

test_that("diff_print uses data.table-style row numbers and truncates", {
  dt <- data.table::data.table(
    d = diff_chars(sprintf("a%d", 1:50), "a1"),
    n = 1:50
  )
  out <- capture.output(diff_print(dt, color = FALSE, nrows = 10L, topn = 2L))
  expect_match(out[2L], "^ 1:")
  expect_match(out[3L], "^ 2:")
  expect_match(out[4L], "^---")
  expect_match(out[5L], "^49:")
  expect_match(out[6L], "^50:")
  expect_match(out[7L], "50 rows (4 shown), 2 cols", fixed = TRUE)

  # everything fits, so no truncation and no footer
  short <- capture.output(diff_print(dt[1:3], color = FALSE))
  expect_length(short, 4L)
  expect_false(any(grepl("---", short, fixed = TRUE)))
})

test_that("diff_print renders vectors, changes and changes columns", {
  v <- diff_words(c("the cat sat", "hello"), c("the dog sat", "hello"))
  out <- capture.output(diff_print(v, color = TRUE))
  expect_identical(out[1L], paste0("[1] the ", red, "cat", reset, green, "dog",
                                   reset, " sat"))
  expect_identical(out[2L], "[2] hello")
  expect_identical(capture.output(diff_print(character(0))), "character(0)")

  # a long column shows its head and tail, as a table does
  long <- diff_chars(sprintf("a%d", 1:50), "a1")
  out <- capture.output(diff_print(long, color = FALSE, nrows = 10L, topn = 2L))
  expect_length(out, 5L)
  expect_match(out[1L], "^ \\[1\\] ")
  expect_match(out[3L], "^ ---$")
  expect_match(out[5L], "^\\[50\\] ")

  # a single jsdiff_changes object defers to its own print method
  expect_identical(capture.output(diff_print(diff_chars("abc", "abd"),
                                             color = TRUE)),
                   paste0("ab", red, "c", reset, green, "d", reset))

  # a list column of change tables is rendered inline
  ch <- data.table::data.table(
    d = diff_words(c("the cat", "a b"), c("the dog", "a b"),
                   format = "changes"),
    n = 1:2
  )
  out <- capture.output(diff_print(ch, color = TRUE))
  expect_match(out[2L], paste0("the ", red, "cat", reset, green, "dog", reset),
               fixed = TRUE)
})

test_that("diff_print escapes newlines in table cells", {
  dt <- data.table::data.table(d = diff_lines(c("a\nb\n", "x\n"),
                                              c("a\nc\n", "x\n")),
                               n = 1:2)
  out <- capture.output(diff_print(dt, color = FALSE))
  expect_length(out, 3L)                       # one line per row, not per \n
  expect_match(out[2L], "a\\n[-b\\n-]{+c\\n+}", fixed = TRUE)
})

test_that("cols restricts which columns are treated as markup", {
  dt <- data.table::data.table(a = c("x [-b-]", "y"), b = c("[-c-]", "z"))

  # detected automatically: both columns are coloured
  both <- capture.output(diff_print(dt, color = TRUE))
  expect_match(both[2L], paste0("x ", red, "b", reset), fixed = TRUE)
  expect_match(both[2L], paste0(red, "c", reset), fixed = TRUE)

  # named explicitly: only that column is, the other keeps its markers
  out <- capture.output(diff_print(dt, cols = "a", color = TRUE))
  expect_match(out[2L], paste0("x ", red, "b", reset), fixed = TRUE)
  expect_match(out[2L], "[-c-]", fixed = TRUE)
  expect_identical(out, capture.output(diff_print(dt, cols = 1L, color = TRUE)))

  expect_error(diff_print(dt, cols = "nope"), "Unknown column")
  expect_error(diff_print(dt, cols = 9L), "out of range")
  expect_error(diff_print(diff_chars("a", "b"), add = "x"), "length-2")
})

test_that("diff_print reports empty tables", {
  dt <- data.table::data.table(a = character(0), b = integer(0))
  expect_match(capture.output(diff_print(dt)), "Empty table: 0 rows, 2 cols")
})

test_that("diff_print returns its input invisibly", {
  dt <- data.table::data.table(d = diff_chars(c("ab", "x"), "ab"))
  expect_output(res <- withVisible(diff_print(dt, color = FALSE)))
  expect_false(res$visible)
  expect_identical(res$value, dt)
})

test_that("diff_print_auto wraps printing and delegates everything else", {
  e <- new.env(parent = globalenv())
  expect_true(diff_print_auto(envir = e, quiet = TRUE))
  expect_true(exists("print.data.table", envir = e, inherits = FALSE))
  expect_true(exists("print.data.frame", envir = e, inherits = FALSE))

  dt_shim <- get("print.data.table", envir = e, inherits = FALSE)
  df_shim <- get("print.data.frame", envir = e, inherits = FALSE)

  dt <- data.table::data.table(a = c("the cat", "x"), b = c("the dog", "x"))
  diffed <- dt[, .(d = diff_words(a, b))]
  withr::with_options(list(jsdiffr.color = TRUE), {
    expect_match(paste(capture.output(dt_shim(diffed)), collapse = "\n"),
                 red, fixed = TRUE)
  })

  # tables without diff markup print exactly as they would have
  expect_identical(capture.output(dt_shim(dt)),
                   capture.output(getS3method("print", "data.table")(dt)))
  expect_identical(capture.output(df_shim(head(iris, 2))),
                   capture.output(print.data.frame(head(iris, 2))))

  # calling it again must not wrap the wrapper
  expect_true(diff_print_auto(envir = e, quiet = TRUE))
  expect_identical(get("print.data.table", envir = e, inherits = FALSE),
                   dt_shim)

  expect_false(diff_print_auto(FALSE, envir = e, quiet = TRUE))
  expect_false(exists("print.data.table", envir = e, inherits = FALSE))
  expect_false(exists("print.data.frame", envir = e, inherits = FALSE))
})

test_that("diff_print_auto leaves methods it did not define alone", {
  e <- new.env(parent = globalenv())
  mine <- function(x, ...) cat("mine\n")
  assign("print.data.frame", mine, envir = e)
  diff_print_auto(FALSE, envir = e, quiet = TRUE)
  expect_identical(get("print.data.frame", envir = e, inherits = FALSE), mine)

  # wrapping an existing method keeps it as the fallback
  diff_print_auto(TRUE, envir = e, quiet = TRUE)
  shim <- get("print.data.frame", envir = e, inherits = FALSE)
  expect_false(identical(shim, mine))
  expect_identical(capture.output(shim(head(iris, 2))), "mine")
  diff_print_auto(FALSE, envir = e, quiet = TRUE)
  expect_identical(get("print.data.frame", envir = e, inherits = FALSE), mine)
})

test_that("diff_print_auto messages unless quiet", {
  e <- new.env(parent = globalenv())
  expect_message(diff_print_auto(envir = e, quiet = FALSE), "print.data.table")
  expect_message(diff_print_auto(FALSE, envir = e, quiet = FALSE), "Removed")
  expect_silent(diff_print_auto(FALSE, envir = e, quiet = FALSE))
})
