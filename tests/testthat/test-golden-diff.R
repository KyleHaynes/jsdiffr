test_that("diff functions match jsdiff golden output exactly", {
  golden <- jsonlite::fromJSON(test_path("fixtures/diff_golden.json"),
                               simplifyVector = FALSE)
  expect_gt(length(golden), 100)
  failures <- 0L
  first_msg <- NULL
  for (cs in golden) {
    got <- run_golden_case(cs)
    msg <- diff_mismatch(got, cs$result)
    if (!is.null(msg)) {
      failures <- failures + 1L
      if (is.null(first_msg)) {
        first_msg <- sprintf("%s args=%s: %s", cs$fn,
                             paste(deparse(cs$args), collapse = ""), msg)
      }
    }
  }
  expect_equal(failures, 0L, info = first_msg)
})
