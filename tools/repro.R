suppressMessages(pkgload::load_all("c:/Users/kyleh/GitHub/anthropic_diff/jsdiffr", quiet = TRUE))
g <- jsonlite::fromJSON("c:/Users/kyleh/GitHub/anthropic_diff/tools/patch_golden.json",
                        simplifyVector = FALSE)
for (k in seq_along(g)) {
  cs <- g[[k]]
  if (cs$type != "createPatch") next
  got <- create_patch(cs$file, cs$old, cs$new, context = cs$context)
  if (!identical(got, cs$patch)) {
    cat("CASE", k, "context=", cs$context, "\n")
    cat("old=", deparse(cs$old), "\n")
    cat("new=", deparse(cs$new), "\n")
    cat("--- diff_lines tokens ---\n")
    d <- diff_lines(cs$old, cs$new)
    print(as.data.frame(d))
    break
  }
}
