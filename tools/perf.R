suppressMessages(pkgload::load_all("c:/Users/kyleh/GitHub/anthropic_diff/jsdiffr", quiet = TRUE))

set.seed(1)
n <- 50000
old_lines <- paste0("line ", seq_len(n), " content ", sample(letters, n, TRUE))
new_lines <- old_lines
# Mutate ~2% of lines, delete some, insert some.
mut <- sample(n, n * 0.02)
new_lines[mut] <- paste0(new_lines[mut], " CHANGED")
old_str <- paste0(old_lines, collapse = "\n")
new_str <- paste0(new_lines, collapse = "\n")
cat(sprintf("old: %d lines / %d chars\n", n, nchar(old_str)))

t <- system.time(d <- diff_lines(old_str, new_str))
cat(sprintf("diff_lines on %d lines: %.2fs, %d change blocks\n", n, t[["elapsed"]], nrow(d)))

t2 <- system.time(p <- create_patch("big.txt", old_str, new_str, context = 3))
cat(sprintf("create_patch: %.2fs, patch %d chars\n", t2[["elapsed"]], nchar(p)))

t3 <- system.time(ap <- apply_patch(old_str, p))
cat(sprintf("apply_patch: %.2fs, round-trips: %s\n", t3[["elapsed"]], identical(ap, new_str)))

# Character diff on a large-ish string
a <- paste0(sample(letters, 20000, TRUE), collapse = "")
b <- a
idx <- sample(nchar(b), 200)
bb <- strsplit(b, "")[[1]]; bb[idx] <- sample(LETTERS, 200, TRUE); b <- paste0(bb, collapse = "")
t4 <- system.time(dc <- diff_chars(a, b))
cat(sprintf("diff_chars on %d chars: %.2fs, %d blocks\n", nchar(a), t4[["elapsed"]], nrow(dc)))
