test_that("create_patch / apply_patch round-trips", {
  old <- "line one\nline two\nline three\n"
  new <- "line one\nline 2\nline three\nline four\n"
  patch <- create_patch("f.txt", old, new)
  expect_type(patch, "character")
  expect_identical(apply_patch(old, patch), new)
})

test_that("structured_patch produces hunks with expected fields", {
  p <- structured_patch("a", "b", "x\ny\nz\n", "x\nY\nz\n")
  expect_s3_class(p, "jsdiff_patch")
  expect_true(length(p$hunks) >= 1)
  h <- p$hunks[[1]]
  expect_true(all(c("old_start", "old_lines", "new_start", "new_lines", "lines") %in% names(h)))
})

test_that("parse_patch round-trips through format_patch", {
  old <- "a\nb\nc\nd\ne\n"
  new <- "a\nB\nc\nd\nE\n"
  patch <- create_patch("f", old, new)
  parsed <- parse_patch(patch)
  expect_type(parsed, "list")
  expect_identical(format_patch(parsed), patch)
})

test_that("reverse_patch undoes a patch", {
  old <- "one\ntwo\nthree\n"
  new <- "one\n2\nthree\n"
  patch <- parse_patch(create_patch("f", old, new))
  reversed <- format_patch(reverse_patch(patch))
  expect_identical(apply_patch(new, reversed), old)
})

test_that("apply_patch fails gracefully on non-matching context", {
  patch <- create_patch("f", "a\nb\nc\n", "a\nB\nc\n")
  expect_false(apply_patch("completely\ndifferent\ntext\n", patch))
})

test_that("apply_patch with fuzz_factor tolerates context drift", {
  old <- "ctx1\nctx2\nTARGET\nctx3\nctx4\n"
  patch <- create_patch("f", old, "ctx1\nctx2\nCHANGED\nctx3\nctx4\n")
  drifted <- "EXTRA\nctx1\nctx2\nTARGET\nctx3\nctx4\n"
  expect_identical(apply_patch(drifted, patch),
                   "EXTRA\nctx1\nctx2\nCHANGED\nctx3\nctx4\n")
})

test_that("line-ending detection and conversion are consistent", {
  patch <- parse_patch(create_patch("f", "a\nb\n", "a\nB\n"))
  expect_true(is_unix(patch))
  win <- unix_to_win(patch)
  expect_true(is_win(win))
  expect_true(is_unix(win_to_unix(win)))
})

test_that("apply_patch auto-converts line endings to match source", {
  patch <- create_patch("f", "a\nb\nc\n", "a\nB\nc\n")  # unix patch
  win_source <- "a\r\nb\r\nc\r\n"
  expect_identical(apply_patch(win_source, patch), "a\r\nB\r\nc\r\n")
})
