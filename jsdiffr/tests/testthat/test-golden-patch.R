test_that("patch + convert functions match jsdiff golden output", {
  golden <- jsonlite::fromJSON(test_path("fixtures/patch_golden.json"),
                               simplifyVector = FALSE)
  expect_gt(length(golden), 50)
  for (cs in golden) {
    if (cs$type == "createPatch") {
      got <- create_patch(cs$file, cs$old, cs$new, context = cs$context)
      expect_identical(got, cs$patch)
      applied <- apply_patch(cs$old, cs$patch)
      expect_identical(applied, if (is.null(cs$applied)) FALSE else cs$applied)
    } else if (cs$type == "xml") {
      expect_identical(convert_changes_to_xml(diff_chars(cs$a, cs$b)), cs$out)
    } else if (cs$type == "dmp") {
      got <- convert_changes_to_dmp(diff_chars(cs$a, cs$b))
      expect_equal(length(got), length(cs$out))
      for (i in seq_along(got)) {
        expect_identical(as.integer(got[[i]]$operation), as.integer(cs$out[[i]]$operation))
        expect_identical(got[[i]]$value, cs$out[[i]]$value)
      }
    } else if (cs$type == "reverse") {
      reversed <- format_patch(reverse_patch(parse_patch(create_patch("f", cs$old, cs$new))))
      expect_identical(reversed, cs$reversed)
      back <- apply_patch(cs$new, reversed)
      expect_identical(back, if (is.null(cs$back)) FALSE else cs$back)
    }
  }
})
