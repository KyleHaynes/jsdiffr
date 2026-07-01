test_that("diff_to_html wraps additions/removals in spans", {
  html <- diff_to_html(diff_words("the cat", "the dog"), wrap = FALSE, pre = FALSE)
  expect_match(html, '<span class="jsdiff-removed">cat</span>', fixed = TRUE)
  expect_match(html, '<span class="jsdiff-added">dog</span>', fixed = TRUE)
})

test_that("diff_html view = FALSE returns HTML without displaying it", {
  html <- diff_html(c("a", "b"), c("a", "B"), view = FALSE)
  expect_type(html, "character")
  expect_match(html, "<table", fixed = TRUE)
})

test_that("diff_html(view = TRUE) uses an explicit viewer over auto-detection", {
  called <- NULL
  fake_viewer <- function(path) called <<- path
  out <- diff_html(c("a", "b"), c("a", "B"), view = TRUE, viewer = fake_viewer)
  expect_true(file.exists(called))
  expect_true(nchar(out) > 0)
})

test_that(".resolve_viewer() picks up getOption('viewer') when set", {
  withr::local_options(viewer = function(path) NULL)
  viewer <- .resolve_viewer()
  expect_identical(viewer, getOption("viewer"))
})

test_that(".resolve_viewer() returns NULL when nothing is available", {
  withr::local_options(viewer = NULL)
  # In a plain Rscript session (this test run) rstudioapi::isAvailable() is
  # FALSE, so this exercises the true "nothing detected" path directly.
  expect_null(.resolve_viewer())
})

test_that("diff_html(view = TRUE) falls back to browseURL with a message when no viewer is found", {
  withr::local_options(viewer = NULL)
  called <- NULL
  testthat::local_mocked_bindings(browseURL = function(url) called <<- url)
  expect_message(
    diff_html(c("a", "b"), c("a", "B"), view = TRUE, viewer = NULL),
    "no IDE viewer detected"
  )
  expect_true(file.exists(called))
})
