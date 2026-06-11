suppressMessages(pkgload::load_all("c:/Users/kyleh/GitHub/anthropic_diff/jsdiffr", quiet = TRUE))
g <- jsonlite::fromJSON("c:/Users/kyleh/GitHub/anthropic_diff/tools/patch_golden.json",
                        simplifyVector = FALSE)

n_fail <- 0L; fails <- list()
note_fail <- function(k, what, detail) {
  n_fail <<- n_fail + 1L
  if (length(fails) < 12) fails[[length(fails) + 1L]] <<- sprintf("[%d] %s: %s", k, what, detail)
}

for (k in seq_along(g)) {
  cs <- g[[k]]
  if (cs$type == "createPatch") {
    got <- create_patch(cs$file, cs$old, cs$new, context = cs$context)
    if (!identical(got, cs$patch)) {
      note_fail(k, "createPatch", paste0("\n--- got ---\n", got, "\n--- exp ---\n", cs$patch))
      next
    }
    # applyPatch roundtrip parity
    applied <- apply_patch(cs$old, cs$patch)
    exp_applied <- if (is.null(cs$applied)) FALSE else cs$applied
    if (!identical(applied, exp_applied)) {
      note_fail(k, "applyPatch", sprintf("got %s exp %s", deparse(applied), deparse(exp_applied)))
    }
  } else if (cs$type == "xml") {
    got <- convert_changes_to_xml(diff_chars(cs$a, cs$b))
    if (!identical(got, cs$out)) note_fail(k, "xml", sprintf("got %s exp %s", got, cs$out))
  } else if (cs$type == "dmp") {
    got <- convert_changes_to_dmp(diff_chars(cs$a, cs$b))
    exp <- cs$out
    ok <- length(got) == length(exp)
    if (ok) for (i in seq_along(got)) {
      if (!identical(as.integer(got[[i]]$operation), as.integer(exp[[i]]$operation)) ||
          !identical(got[[i]]$value, exp[[i]]$value)) { ok <- FALSE; break }
    }
    if (!ok) note_fail(k, "dmp", "mismatch")
  } else if (cs$type == "reverse") {
    patches <- parse_patch(create_patch("f", cs$old, cs$new))
    reversed <- format_patch(reverse_patch(patches))
    if (!identical(reversed, cs$reversed)) {
      note_fail(k, "reverse-format", paste0("\n--got--\n", reversed, "\n--exp--\n", cs$reversed))
      next
    }
    back <- apply_patch(cs$new, reversed)
    exp_back <- if (is.null(cs$back)) FALSE else cs$back
    if (!identical(back, exp_back)) note_fail(k, "reverse-apply", sprintf("got %s exp %s", deparse(back), deparse(exp_back)))
  }
}

cat("Total patch cases:", length(g), "  Failures:", n_fail, "\n\n")
for (f in fails) cat(f, "\n")
