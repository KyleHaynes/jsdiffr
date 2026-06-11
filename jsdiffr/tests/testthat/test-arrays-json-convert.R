test_that("diff_arrays compares sequences element-wise", {
  d <- diff_arrays(c("a", "b", "c"), c("a", "c", "d"))
  expect_s3_class(d, "jsdiff_changes")
  expect_identical(attr(d, "jsdiffr_value_type"), "list")
  # reconstruct old and new from the change list
  old <- unlist(d$value[!d$added])
  new <- unlist(d$value[!d$removed])
  expect_equal(old, c("a", "b", "c"))
  expect_equal(new, c("a", "c", "d"))
})

test_that("diff_arrays honours a custom comparator", {
  d <- diff_arrays(c(1, 2, 3), c(1.4, 2.2, 9),
                   comparator = function(a, b) floor(a) == floor(b))
  # 1~1.4 equal, 2~2.2 equal, 3 vs 9 differ
  expect_equal(sum(d$removed), 1L)
  expect_equal(sum(d$added), 1L)
})

test_that("diff_json diffs serialised objects line by line", {
  d <- diff_json(list(a = 1, b = 2), list(a = 1, b = 3))
  expect_s3_class(d, "jsdiff_changes")
  expect_true(any(d$removed))
  expect_true(any(d$added))
})

test_that("diff_json accepts pre-serialised JSON strings", {
  a <- "{\n  \"x\": 1\n}"
  b <- "{\n  \"x\": 2\n}"
  d <- diff_json(a, b)
  expect_true(any(d$added) && any(d$removed))
})

test_that("convert_changes_to_xml wraps ins/del and escapes HTML", {
  d <- diff_chars("a<b", "a>b")
  xml <- convert_changes_to_xml(d)
  expect_match(xml, "<del>&lt;</del>")
  expect_match(xml, "<ins>&gt;</ins>")
})

test_that("convert_changes_to_dmp uses 1/-1/0 operations", {
  d <- diff_chars("ab", "ac")
  dmp <- convert_changes_to_dmp(d)
  ops <- vapply(dmp, function(x) x$operation, integer(1))
  expect_true(all(ops %in% c(-1L, 0L, 1L)))
})

test_that("diff_to_html produces span-wrapped, escaped output", {
  html <- diff_to_html(diff_words("the cat", "the dog"))
  expect_match(html, "jsdiff-added")
  expect_match(html, "jsdiff-removed")
  expect_match(html, "^<div class=\"jsdiff\">")
})

test_that("diff_to_html rejects array diffs", {
  expect_error(diff_to_html(diff_arrays(1:3, 2:4)), "string-based")
})
