test_that("diff_chars returns a classed data.table with expected columns", {
  d <- diff_chars("abc", "abd")
  expect_s3_class(d, "jsdiff_changes")
  expect_s3_class(d, "data.table")
  expect_named(d, c("value", "added", "removed", "count"))
  expect_equal(d$value, c("ab", "c", "d"))
  expect_equal(d$removed, c(FALSE, TRUE, FALSE))
  expect_equal(d$added, c(FALSE, FALSE, TRUE))
  expect_equal(d$count, c(2L, 1L, 1L))
})

test_that("identical inputs yield a single unchanged block", {
  d <- diff_chars("hello", "hello")
  expect_equal(nrow(d), 1L)
  expect_false(d$added)
  expect_false(d$removed)
  expect_equal(d$value, "hello")
})

test_that("empty inputs yield zero-row diffs", {
  expect_equal(nrow(diff_chars("", "")), 0L)
  expect_equal(nrow(diff_lines("", "")), 0L)
})

test_that("ignore_case treats case-different tokens as equal", {
  d <- diff_chars("ABC", "abc", ignore_case = TRUE)
  expect_equal(nrow(d), 1L)
  expect_false(d$added || d$removed)
})

test_that("diff_words ignores whitespace in matching but preserves it", {
  d <- diff_words("foo bar baz", "foo qux baz")
  expect_equal(d$value[d$removed], "bar")
  expect_equal(d$value[d$added], "qux")
})

test_that("max_edit_length aborts and returns NULL when exceeded", {
  expect_null(diff_chars("aaaaaaaa", "bbbbbbbb", max_edit_length = 2))
})

test_that("one_change_per_token emits one row per token", {
  d <- diff_chars("ab", "ac", one_change_per_token = TRUE)
  # tokens: a(common) b(removed) c(added) -> 3 rows, each count 1
  expect_true(all(d$count == 1L))
  expect_equal(nrow(d), 3L)
})

test_that("diff_lines newline_is_token splits newlines into their own tokens", {
  d <- diff_lines("a\nb\n", "a\nc\n", newline_is_token = TRUE)
  expect_true(any(d$value == "\n"))
})

test_that("format produces a concatenated string without colour", {
  d <- diff_words("the cat", "the dog")
  s <- format(d, color = FALSE)
  expect_equal(s, paste0(d$value, collapse = ""))
  expect_match(s, "dog")
  expect_match(s, "cat")
})

test_that("format with colour wraps additions/removals in ANSI codes", {
  d <- diff_chars("abc", "abd")
  s <- format(d, color = TRUE)
  expect_match(s, "\033\\[32m", perl = TRUE)  # green for additions
  expect_match(s, "\033\\[31m", perl = TRUE)  # red for removals
})
