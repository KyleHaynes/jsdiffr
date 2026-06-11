# Subsample the large golden files into a compact fixture shipped with the
# package tests (so tests need no Node/jsdiff installed).
set.seed(7)
diff_g <- jsonlite::fromJSON("c:/Users/kyleh/GitHub/anthropic_diff/tools/golden.json",
                             simplifyVector = FALSE)
patch_g <- jsonlite::fromJSON("c:/Users/kyleh/GitHub/anthropic_diff/tools/patch_golden.json",
                              simplifyVector = FALSE)

# Keep all fixed edge cases (first ~25) plus a random sample of the fuzzed ones,
# balanced across functions.
fns <- vapply(diff_g, function(c) c$fn, character(1))
keep <- integer(0)
for (fn in unique(fns)) {
  idx <- which(fns == fn)
  keep <- c(keep, head(idx, 60))
}
keep <- sort(unique(c(seq_len(25), keep)))
diff_small <- diff_g[keep]

patch_small <- patch_g[sort(sample(seq_along(patch_g), 250))]

dir.create("c:/Users/kyleh/GitHub/anthropic_diff/jsdiffr/tests/testthat/fixtures",
           recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(diff_small,
  "c:/Users/kyleh/GitHub/anthropic_diff/jsdiffr/tests/testthat/fixtures/diff_golden.json",
  auto_unbox = TRUE)
jsonlite::write_json(patch_small,
  "c:/Users/kyleh/GitHub/anthropic_diff/jsdiffr/tests/testthat/fixtures/patch_golden.json",
  auto_unbox = TRUE)
cat("diff fixture:", length(diff_small), " patch fixture:", length(patch_small), "\n")
