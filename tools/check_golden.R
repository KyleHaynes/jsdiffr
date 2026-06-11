# Compare the R port's output against jsdiff golden output (tools/golden.json).
suppressMessages(pkgload::load_all("c:/Users/kyleh/GitHub/anthropic_diff/jsdiffr", quiet = TRUE))

golden <- jsonlite::fromJSON("c:/Users/kyleh/GitHub/anthropic_diff/tools/golden.json",
                             simplifyVector = FALSE)

fn_map <- list(
  diffChars = diff_chars, diffWords = diff_words,
  diffWordsWithSpace = diff_words_with_space, diffLines = diff_lines,
  diffTrimmedLines = diff_trimmed_lines, diffSentences = diff_sentences,
  diffCss = diff_css, diffArrays = diff_arrays
)

camel_to_snake <- function(x) tolower(gsub("([A-Z])", "_\\1", x))

run_case <- function(cs) {
  fn <- fn_map[[cs$fn]]
  args <- cs$args
  if (cs$fn == "diffArrays") {
    args <- list(unlist(args[[1]]), unlist(args[[2]]))
  }
  opts <- cs$options
  if (length(opts)) names(opts) <- camel_to_snake(names(opts))
  do.call(fn, c(args, opts))
}

norm_value <- function(v) {
  if (is.list(v)) as.character(unlist(v)) else as.character(v)
}

compare <- function(got, expected) {
  if (is.null(got)) return("got NULL")
  if (nrow(got) != length(expected)) {
    return(sprintf("nrow %d != %d", nrow(got), length(expected)))
  }
  if (nrow(got) == 0) return(NULL)
  for (i in seq_len(nrow(got))) {
    e <- expected[[i]]
    gv <- got$value[[i]]
    ev <- e$value
    if (!identical(norm_value(gv), norm_value(ev))) {
      return(sprintf("row %d value: got %s exp %s", i,
                     paste(deparse(gv), collapse = ""),
                     paste(deparse(ev), collapse = "")))
    }
    if (!identical(isTRUE(got$added[i]), isTRUE(e$added)) ||
        !identical(isTRUE(got$removed[i]), isTRUE(e$removed))) {
      return(sprintf("row %d flags: got a=%s r=%s exp a=%s r=%s", i,
                     got$added[i], got$removed[i], isTRUE(e$added), isTRUE(e$removed)))
    }
    if (!identical(as.integer(got$count[i]), as.integer(e$count))) {
      return(sprintf("row %d count: got %d exp %d", i, got$count[i], e$count))
    }
  }
  NULL
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

fails <- list()
by_fn_total <- table(vapply(golden, function(c) c$fn, character(1)))
by_fn_fail <- integer(0)
for (k in seq_along(golden)) {
  cs <- golden[[k]]
  got <- tryCatch(run_case(cs), error = function(e) structure(list(), err = conditionMessage(e)))
  msg <- if (!is.null(attr(got, "err"))) paste("ERROR:", attr(got, "err")) else compare(got, cs$result)
  if (!is.null(msg)) {
    by_fn_fail[cs$fn] <- (by_fn_fail[cs$fn] %||% 0L) + 1L
    if (length(fails) < 15) {
      fails[[length(fails) + 1L]] <- list(k = k, fn = cs$fn, args = cs$args,
                                          options = cs$options, msg = msg)
    }
  }
}

cat("Total cases:", length(golden), "\n")
cat("Failures:", sum(unlist(by_fn_fail)), "\n\n")
cat("Per-function totals:\n"); print(by_fn_total)
if (length(by_fn_fail)) { cat("\nPer-function failures:\n"); print(by_fn_fail) }
cat("\nFirst failures:\n")
for (f in fails) {
  cat(sprintf("[%d] %s args=%s opts=%s\n   %s\n", f$k, f$fn,
              paste(deparse(f$args), collapse = ""),
              paste(deparse(f$options), collapse = ""), f$msg))
}
