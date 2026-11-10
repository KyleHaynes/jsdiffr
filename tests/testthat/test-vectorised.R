test_that("length-1 inputs still return a jsdiff_changes object", {
  d <- diff_chars("abc", "abd")
  expect_s3_class(d, "jsdiff_changes")
  expect_s3_class(diff_words("the cat", "the dog"), "jsdiff_changes")
  expect_s3_class(diff_lines("a\n", "b\n"), "jsdiff_changes")
})

test_that("vector inputs diff element-wise and return markup by default", {
  old <- c("the cat sat", "12 Smith St", "hello")
  new <- c("the dog sat", "12 Smith Street", "hello")
  got <- diff_words(old, new)
  expect_type(got, "character")
  expect_length(got, 3L)
  expect_identical(
    got,
    c("the [-cat-]{+dog+} sat", "12 Smith [-St-]{+Street+}", "hello")
  )
})

test_that("every string diff function is vectorised", {
  funs <- list(diff_chars, diff_words, diff_words_with_space, diff_lines,
               diff_trimmed_lines, diff_sentences, diff_css)
  for (f in funs) {
    got <- f(c("abc", "abd"), c("abd", "abd"))
    expect_type(got, "character")
    expect_length(got, 2L)
    expect_identical(got[2L], "abd")
  }
})

test_that("non-character columns are coerced, including factors", {
  dt <- data.table::data.table(iris)
  out <- dt[, .(blah = diff_chars(Sepal.Length, Sepal.Width), Species)]
  expect_s3_class(out, "data.table")
  expect_identical(nrow(out), 150L)
  expect_type(out$blah, "character")
  expect_identical(out$blah[1L], "[-5-]{+3+}.[-1-]{+5+}")

  expect_identical(diff_chars(c(1.5, 2), c(1.5, 3)), c("1.5", "[-2-]{+3+}"))
  expect_identical(
    diff_chars(factor(c("a", "b")), c("a", "c")),
    c("a", "[-b-]{+c+}")
  )
})

test_that("a length-1 side is recycled", {
  expect_identical(
    diff_chars(c("abc", "abd", "xbc"), "abc"),
    c("abc", "ab[-d-]{+c+}", "[-x-]{+a+}bc")
  )
  expect_identical(
    diff_chars("abc", c("abc", "abd")),
    c("abc", "ab[-c-]{+d+}")
  )
})

test_that("mismatched lengths are an error", {
  expect_error(diff_chars(c("a", "b"), c("a", "b", "c")), "same length")
  expect_error(diff_chars(character(0), c("a", "b")), "same length")
})

test_that("zero-length input gives zero-length output", {
  expect_identical(diff_chars(character(0), character(0)), character(0))
  expect_identical(diff_chars(character(0), "a"), character(0))
  expect_identical(diff_chars(character(0), character(0), format = "changes"),
                   list())
})

test_that("NA on either side gives NA out", {
  expect_identical(
    diff_words(c("the cat", NA, "a"), c("the dog", "x", NA)),
    c("the [-cat-]{+dog+}", NA, NA)
  )
  expect_null(diff_chars(NA, "abc"))
  expect_null(diff_chars("abc", NA_character_))
  expect_null(diff_words(c("a", NA), c("a", "b"), format = "changes")[[2L]])
})

test_that("format controls the rendering", {
  old <- "the cat sat"
  new <- "the dog sat"
  expect_identical(diff_words(old, new, format = "markup"),
                   "the [-cat-]{+dog+} sat")
  expect_identical(diff_words(old, new, format = "xml"),
                   "the <del>cat</del><ins>dog</ins> sat")
  expect_identical(diff_words(old, new, format = "added"), "dog")
  expect_identical(diff_words(old, new, format = "removed"), "cat")
  expect_match(diff_words(old, new, format = "html"),
               '^<span class="jsdiff-context">the </span>')
  expect_match(diff_words(old, new, format = "ansi"), "\033\\[31mcat\033\\[0m")
  expect_error(diff_words(old, new, format = "nope"), "should be one of")
})

test_that("format is honoured for length-1 inputs too", {
  got <- diff_chars("abc", "abd", format = "markup")
  expect_type(got, "character")
  expect_identical(got, "ab[-c-]{+d+}")
})

test_that("format = 'changes' returns a list column of change tables", {
  got <- diff_words(c("the cat", "a b"), c("the dog", "a b"), format = "changes")
  expect_type(got, "list")
  expect_length(got, 2L)
  expect_s3_class(got[[1L]], "jsdiff_changes")
  expect_identical(got[[1L]]$value, c("the ", "cat", "dog"))

  dt <- data.table::data.table(a = c("the cat", "a b"), b = c("the dog", "a b"))
  out <- dt[, .(d = diff_words(a, b, format = "changes"))]
  expect_type(out$d, "list")
  expect_s3_class(out$d[[1L]], "jsdiff_changes")
})

test_that("other arguments still apply per element", {
  expect_identical(diff_chars(c("ABC", "abc"), c("abc", "abd"),
                              ignore_case = TRUE),
                   c("abc", "ab[-c-]{+d+}"))  # unchanged value comes from `new`
  expect_identical(diff_chars(c("aaaaaaaa", "ab"), c("bbbbbbbb", "ab"),
                              max_edit_length = 2),
                   c(NA, "ab"))
  expect_identical(diff_words(c("foo bar", "x"), c("foo  bar", "x"),
                              ignore_whitespace = FALSE),
                   c("foo[- -]{+  +}bar", "x"))
})

test_that("repeated pairs are diffed once but reported per element", {
  old <- rep(c("the cat sat", "hello"), each = 3L)
  new <- rep(c("the dog sat", "hello"), each = 3L)
  expect_identical(
    diff_words(old, new),
    rep(c("the [-cat-]{+dog+} sat", "hello"), each = 3L)
  )

  calls <- 0L
  counting <- function(a, b) {
    calls <<- calls + 1L
    new_changes(a, FALSE, FALSE, 1L)
  }
  res <- diff_vectorised(counting, rep(c("a", "b"), 50L), rep("z", 100L),
                         "markup")
  expect_length(res, 100L)
  expect_identical(calls, 2L)
})

test_that("changes_to_markup honours custom markers", {
  d <- diff_chars("abc", "abd")
  expect_identical(changes_to_markup(d), "ab[-c-]{+d+}")
  expect_identical(
    changes_to_markup(d, add = c("+[", "]"), remove = c("-[", "]")),
    "ab-[c]+[d]"
  )
  expect_identical(changes_to_markup(NULL), NA_character_)
  expect_identical(changes_to_markup(diff_chars("", "")), "")
  expect_error(changes_to_markup(d, add = "x"), "length-2")
  expect_error(changes_to_markup(diff_arrays(list(1), list(2))),
               "not an array diff")

  withr::with_options(
    list(jsdiffr.markup_add = c("<", ">"), jsdiffr.markup_remove = c("(", ")")),
    expect_identical(diff_chars(c("abc", "x"), c("abd", "x"))[1L], "ab(c)<d>")
  )
})

test_that("camelCase aliases vectorise too", {
  expect_identical(diffChars(c("abc", "x"), c("abd", "x")),
                   c("ab[-c-]{+d+}", "x"))
  expect_identical(diffWords(c("the cat", "x"), c("the dog", "x"), format = "added"),
                   c("dog", ""))
})

test_that("diff_html survives NA rows", {
  html <- diff_html(c("a", NA), c("a", "b"), view = FALSE)
  expect_type(html, "character")
  expect_match(html, "<table")
})
